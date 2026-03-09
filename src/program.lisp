;;; program.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Main program loop and runtime

(in-package #:tuition)

(defvar *current-program* nil
  "The currently running program, bound during the event loop.")

(defvar *exec-suspended* nil
  "When T, signal handlers should not send messages (TUI is suspended for exec).")

(defclass program ()
  ((model :initarg :model :accessor program-model)
   (renderer :initform (make-instance 'renderer) :accessor program-renderer)
   (msg-channel :initform (trivial-channels:make-channel) :accessor msg-channel)
   (running :initform nil :accessor program-running)
   (input-paused :initform nil :accessor program-input-paused
                 :documentation "When T, the input loop will not read from stdin.")
   (options :initarg :options :initform nil :accessor program-options)
   (tty-stream :initform nil :accessor program-tty-stream)
   (restore-fn :initform nil :accessor program-restore-fn)
   (cmd-pool :initform nil :accessor program-cmd-pool))
  (:documentation "A Bubble Tea program instance."))

(defun make-program (model &key (pool-size *default-pool-size*))
  "Create a new program with the given initial model.

In v2, terminal modes (alt-screen, mouse, focus events) are controlled
declaratively through the view-state returned by the view method.

Options (keyword args only):
  :pool-size  number of worker threads for command execution (default: 4)
              Set to NIL to disable thread pool (spawns thread per command)"
  (when (and pool-size (not (and (integerp pool-size) (> pool-size 0))))
    (error "Invalid :pool-size ~S; expected positive integer or NIL" pool-size))
  (let ((opts (list :pool-size pool-size)))
    (make-instance 'program :model model :options opts)))

(defun send (program msg)
  "Send a message to the program's update loop."
  (when (program-running program)
    (trivial-channels:sendmsg (msg-channel program) msg)))

(defun quit (program)
  "Quit the program gracefully."
  (send program (make-quit-msg)))

(defun kill-program (program)
  "Kill the program immediately."
  (setf (program-running program) nil))

(defun stop (program)
  "Request the program to stop by setting running to NIL."
  (setf (program-running program) nil))

(defun join (thread)
  "Join a thread (compat wrapper)."
  (bt:join-thread thread))

(defun run (program)
  "Run the program's main loop. Blocks until the program exits.
In v2, starts with minimal terminal setup (raw mode only).
Terminal modes are applied declaratively from the first view-state render."
  (let* ((opts (program-options program))
         (pool-size (getf opts :pool-size))
         (*current-program* program)
         ;; Open /dev/tty for direct terminal I/O
         (tty-stream (get-tty-stream))
         (*standard-output* (or tty-stream *standard-output*)))
    ;; Minimal terminal setup - just raw mode and bracketed paste
    ;; Alt-screen, mouse, focus etc. are handled by view-state transitions
    (with-raw-terminal ()
      (setf (program-running program) t)

      ;; Initialize thread pool if enabled
      (when (and *use-thread-pool* pool-size)
        (setf (program-cmd-pool program) (make-pool pool-size program)))

      ;; Use /dev/tty stream if available
      (setf (program-tty-stream program) (or tty-stream *terminal-io*))
      (when tty-stream
        (setf (output-stream (program-renderer program)) tty-stream))

      ;; Set up signal handlers (POSIX signals not available on Windows)
      #+(and sbcl (not windows))
      (let ((old-sigwinch-handler nil)
            (old-sigtstp-handler nil)
            (old-sigcont-handler nil))
        ;; SIGWINCH - terminal resize
        ;; Signal handlers must not call send directly (mutex acquisition is
        ;; unsafe in signal context).  Defer the send to a short-lived thread.
        (setf old-sigwinch-handler
              (sb-sys:enable-interrupt
               sb-posix:sigwinch
               (lambda (signal code scp)
                 (declare (ignore signal code scp))
                 (unless *exec-suspended*
                   (let* ((size (get-terminal-size))
                          (width (car size))
                          (height (cdr size)))
                     ;; Update renderer dimensions (slot writes are safe)
                     (setf (renderer-width (program-renderer program)) width
                           (renderer-height (program-renderer program)) height)
                     ;; Defer channel send to a new thread
                     (bt:make-thread
                      (lambda ()
                        (send program (make-window-size-msg :width width :height height)))
                      :name "tuition-sigwinch"))))))

        ;; SIGTSTP - suspend (Ctrl+Z)
        (setf old-sigtstp-handler
              (sb-sys:enable-interrupt
               sb-posix:sigtstp
               (lambda (signal code scp)
                 (declare (ignore signal code scp))
                 ;; Suspend based on current view-state
                 (let* ((renderer (program-renderer program))
                        (vs (last-view-state renderer)))
                   (setf (program-restore-fn program)
                         (suspend-terminal
                          :alt-screen (and vs (view-state-alt-screen vs))
                          :mouse (and vs (view-state-mouse-mode vs))
                          :focus-events (and vs (view-state-report-focus vs)))))
                 (sb-posix:kill (sb-posix:getpid) sb-posix:sigstop))))

        ;; SIGCONT - resume after suspension
        (setf old-sigcont-handler
              (sb-sys:enable-interrupt
               sb-posix:sigcont
               (lambda (signal code scp)
                 (declare (ignore signal code scp))
                 (when (program-restore-fn program)
                   (resume-terminal (program-restore-fn program))
                   (setf (program-restore-fn program) nil))
                 ;; Defer channel send to a new thread
                 (bt:make-thread
                  (lambda ()
                    (send program (make-resume-msg)))
                  :name "tuition-sigcont"))))

        ;; Start input thread
        (let ((input-thread (bt:make-thread
                             (lambda () (input-loop program))
                             :name "tuition-input")))

          ;; Send initial window size and update renderer dimensions
          (let* ((size (get-terminal-size))
                 (width (car size))
                 (height (cdr size)))
            (setf (renderer-width (program-renderer program)) width
                  (renderer-height (program-renderer program)) height)
            (send program (make-window-size-msg :width width :height height)))

          ;; Run initial command
          (let ((init-cmd (init (program-model program))))
            (when init-cmd
              (run-command program init-cmd)))

          ;; Render initial view
          (render (program-renderer program)
                  (view (program-model program)))

          ;; Main event loop
          (event-loop program)

          ;; Shutdown
          (setf (program-running program) nil)

          ;; Clean up terminal state from last view-state
          (let ((vs (last-view-state (program-renderer program))))
            (when vs
              (when (view-state-mouse-mode vs) (disable-mouse))
              (when (view-state-report-focus vs) (disable-focus-events))
              (when (view-state-keyboard-enhancements vs) (disable-kitty-keyboard))
              (when (view-state-alt-screen vs) (exit-alt-screen))))

          ;; Shutdown thread pool if active
          (when (program-cmd-pool program)
            (shutdown-pool (program-cmd-pool program))
            (setf (program-cmd-pool program) nil))

          (bt:join-thread input-thread)

          ;; Restore old signal handlers
          (when old-sigwinch-handler
            (sb-sys:enable-interrupt sb-posix:sigwinch old-sigwinch-handler))
          (when old-sigtstp-handler
            (sb-sys:enable-interrupt sb-posix:sigtstp old-sigtstp-handler))
          (when old-sigcont-handler
            (sb-sys:enable-interrupt sb-posix:sigcont old-sigcont-handler))))

      #-(and sbcl (not windows))
      (progn
        ;; Non-SBCL or Windows
        (let ((input-thread (bt:make-thread
                             (lambda () (input-loop program))
                             :name "tuition-input")))
          (let ((init-cmd (init (program-model program))))
            (when init-cmd
              (run-command program init-cmd)))
          (render (program-renderer program)
                  (view (program-model program)))
          (event-loop program)
          (setf (program-running program) nil)

          ;; Clean up terminal state from last view-state
          (let ((vs (last-view-state (program-renderer program))))
            (when vs
              (when (view-state-mouse-mode vs) (disable-mouse))
              (when (view-state-report-focus vs) (disable-focus-events))
              (when (view-state-keyboard-enhancements vs) (disable-kitty-keyboard))
              (when (view-state-alt-screen vs) (exit-alt-screen))))

          ;; Shutdown thread pool if active
          (when (program-cmd-pool program)
            (shutdown-pool (program-cmd-pool program))
            (setf (program-cmd-pool program) nil))

          (bt:join-thread input-thread)))
      ) ; close with-raw-terminal
    ;; Clean up /dev/tty stream after terminal is restored
    (close-tty-stream)))

(defun event-loop (program)
  "Main event processing loop with batched message processing.
Uses non-blocking getmsg with sleep to avoid trivial-channels mutex
issues with timed recvmsg on SBCL."
  (loop while (program-running program) do
    (handler-case
        (let ((first-msg (trivial-channels:getmsg (msg-channel program))))
          (if first-msg
              (let ((messages (list first-msg)))
                (loop for msg = (trivial-channels:getmsg (msg-channel program))
                      while msg
                      do (push msg messages))
                (setf messages (nreverse messages))
                (handle-messages-batch program messages))
              (sleep 0.005)))
      (error (e)
        (handle-error :event-loop e)))))

(defun coalesce-scroll-events (messages)
  "Combine consecutive scroll events in the same direction into one."
  (when (null messages)
    (return-from coalesce-scroll-events nil))
  (let ((result nil)
        (prev-scroll nil))
    (dolist (msg messages)
      (cond
        ((mouse-wheel-msg-p msg)
         (let ((dir (mouse-wheel-direction msg)))
           (if (and prev-scroll (eq dir (mouse-wheel-direction prev-scroll)))
               (incf (mouse-wheel-count prev-scroll))
               (progn
                 (push msg result)
                 (setf prev-scroll msg)))))
        (t
         (push msg result)
         (setf prev-scroll nil))))
    (nreverse result)))

(defun handle-messages-batch (program messages)
  "Process multiple messages, rendering only once at the end."
  (let ((messages (coalesce-scroll-events messages))
        (should-render nil)
        (pending-cmds nil))
    (dolist (msg messages)
      (cond
        ;; Quit message - stop immediately
        ((quit-msg-p msg)
         (setf (program-running program) nil)
         (return-from handle-messages-batch))

        ;; Window size - update renderer dimensions
        ((window-size-msg-p msg)
         (setf (renderer-width (program-renderer program)) (window-size-msg-width msg)
               (renderer-height (program-renderer program)) (window-size-msg-height msg))
         (multiple-value-bind (new-model cmd)
             (update (program-model program) msg)
           (setf (program-model program) new-model)
           (setf should-render t)
           (when cmd (push cmd pending-cmds))))

        ;; Mouse events with on-mouse handler
        ((and (typep msg 'mouse-event)
              (let ((vs (last-view-state (program-renderer program))))
                (and vs (view-state-on-mouse vs))))
         (let* ((vs (last-view-state (program-renderer program)))
                (handler (view-state-on-mouse vs))
                (mouse-cmd (funcall handler msg)))
           (when mouse-cmd
             (push mouse-cmd pending-cmds)))
         ;; Still let the model handle it
         (multiple-value-bind (new-model cmd)
             (update (program-model program) msg)
           (setf (program-model program) new-model)
           (setf should-render t)
           (when cmd (push cmd pending-cmds))))

        ;; All other messages
        (t
         (multiple-value-bind (new-model cmd)
             (update (program-model program) msg)
           (setf (program-model program) new-model)
           (setf should-render t)
           (when cmd
             (push cmd pending-cmds))))))

    ;; Run all accumulated commands
    (dolist (cmd (nreverse pending-cmds))
      (run-command program cmd))

    ;; Render once after all messages processed
    (when should-render
      (render (program-renderer program)
              (view (program-model program))))))

(defun run-command (program cmd)
  "Execute a command."
  (cond
    ((null cmd) nil)
    ((exec-cmd-p cmd)
     (run-exec-command program cmd))
    ((listp cmd)
     (if (eql (first cmd) :sequence)
         (run-sequence program (rest cmd))
         (run-batch program cmd)))
    ((functionp cmd)
     (let ((pool (program-cmd-pool program)))
       (if (and pool (thread-pool-running pool))
           (submit-command pool cmd)
           (bt:make-thread
            (lambda ()
              (handler-case
                  (let ((msg (funcall cmd)))
                    (when msg
                      (send program msg)))
                (error (e)
                  (handle-error :command e))))
            :name "tuition-cmd"))))
    (t nil)))

(defun run-exec-command (program cmd)
  "Run an external program with full TUI suspension."
  (let* ((renderer (program-renderer program))
         (vs (last-view-state renderer))
         (exec-program (exec-cmd-program cmd))
         (exec-args (exec-cmd-args cmd))
         (callback (exec-cmd-callback cmd)))
    (setf (program-input-paused program) t)
    (setf *exec-suspended* t)
    (let ((restore-fn (suspend-terminal
                       :alt-screen (and vs (view-state-alt-screen vs))
                       :mouse (and vs (view-state-mouse-mode vs))
                       :focus-events (and vs (view-state-report-focus vs)))))
      (unwind-protect
          (handler-case
              (uiop:run-program (cons exec-program exec-args)
                                :input :interactive
                                :output :interactive
                                :error-output :interactive)
            (error (e)
              (handle-error :exec-command e)))
        (when restore-fn
          (resume-terminal restore-fn))
        (setf *exec-suspended* nil)
        (setf (program-input-paused program) nil)
        ;; Force full re-render by clearing last buffer
        (setf (last-buffer renderer) nil)
        (let* ((size (get-terminal-size))
               (width (car size))
               (height (cdr size)))
          (send program (make-window-size-msg :width width :height height)))
        (when callback
          (let ((msg (funcall callback)))
            (when msg
              (send program msg))))))))

(defun run-batch (program cmds)
  "Run multiple commands concurrently."
  (dolist (cmd cmds)
    (when cmd
      (run-command program cmd))))

(defun run-sequence (program cmds)
  "Run multiple commands in sequence."
  (bt:make-thread
   (lambda ()
     (dolist (cmd cmds)
       (when (and cmd (program-running program))
         (let ((msg (funcall cmd)))
           (when msg
             (send program msg))))))
   :name "tuition-sequence"))

(defun read-all-available-events ()
  "Read all available input events and return them as a list."
  (let ((events nil))
    (loop for msg = (read-key)
          while msg
          do (push msg events))
    (nreverse events)))

(defun send-batch (program msgs)
  "Send multiple messages to ensure proper batching."
  (when (and (program-running program) msgs)
    (let ((channel (msg-channel program)))
      (dolist (msg msgs)
        (trivial-channels:sendmsg channel msg)))))

(defun input-loop (program)
  "Read input and send messages to the program."
  (setf *input-stream* (program-tty-stream program))
  #+(and sbcl (not windows))
  (handler-bind ((warning #'muffle-warning))
    (handler-case
        (loop while (program-running program) do
          (handler-case
              (if (program-input-paused program)
                  (sleep 0.05)
                  (let ((events (read-all-available-events)))
                    (if events
                        (progn
                          (%ilog "input-loop: batch of ~D events" (length events))
                          (send-batch program events))
                        (sleep 0.001))))
            (error (e)
              (handle-error :input-loop e))))
      (error (e)
        (handle-error :input-loop e))))
  #-(and sbcl (not windows))
  (handler-case
      (loop while (program-running program) do
        (handler-case
            (if (program-input-paused program)
                (sleep 0.05)
                (let ((events (read-all-available-events)))
                  (if events
                      (progn
                        (%ilog "input-loop: batch of ~D events" (length events))
                        (send-batch program events))
                      (sleep 0.001))))
          (error (e)
            (handle-error :input-loop e))))
    (error (e)
      (handle-error :input-loop e))))

;;; Convenience macro to define a program class and its handlers
(defmacro defprogram (class-name &key slots init update view)
  "Define a model class named CLASS-NAME and its TEA protocol methods."
  `(progn
     (defclass ,class-name () ,(or slots '()))
     ,(when (or init (null init))
        `(defmethod init ((model ,class-name))
           ,init))
     ,(when update
        `(defmethod update ((model ,class-name) msg)
           ,update))
     ,(when view
        `(defmethod view ((model ,class-name))
           ,view))))
