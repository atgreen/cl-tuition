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
   (cmd-pool :initform nil :accessor program-cmd-pool)
   (pending-commands :initform #-tuition-single-threaded nil
                                  #+tuition-single-threaded (make-st-queue)
                     :accessor program-pending-commands)
   (pending-signal-thunks :initform #-tuition-single-threaded nil
                                     #+tuition-single-threaded (make-st-queue)
                          :accessor program-pending-signal-thunks))
  (:documentation "A Bubble Tea program instance."))

(defun make-program (model &key (pool-size *default-pool-size*))
  "Create a new program with the given initial model.

In v2, terminal modes (alt-screen, mouse, focus events) are controlled
declaratively through the view-state returned by the view method.

Options (keyword args only):
  :pool-size  number of worker threads for command execution (default: 4)
              Set to NIL to disable thread pool (spawns thread per command)"
  #+tuition-single-threaded
  (declare (ignore pool-size))
  (let ((resolved-pool-size #-tuition-single-threaded pool-size
                            #+tuition-single-threaded nil))
    (when (and resolved-pool-size
               (not (and (integerp resolved-pool-size) (> resolved-pool-size 0))))
      (error "Invalid :pool-size ~S; expected positive integer or NIL" resolved-pool-size))
    (make-instance 'program :model model
                            :options (list :pool-size resolved-pool-size))))

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

#+tuition-single-threaded
(defun enqueue-async-command (program cmd)
  "Queue a command for cooperative execution in the single-threaded event loop."
  (st-queue-push (program-pending-commands program) cmd))

#+tuition-single-threaded
(defun %invoke-queued-command (program cmd)
  "Run one async command thunk and send its message, if any."
  (handler-case
      (alexandria:when-let ((msg (funcall cmd)))
        (when msg
          (send program msg)))
    (error (e)
      (handle-error :command e))))

#+tuition-single-threaded
(defun process-one-pending-command (program)
  "Execute at most one queued command without blocking the event loop indefinitely."
  (let ((cmd (st-queue-pop (program-pending-commands program))))
    (when cmd
      (%invoke-queued-command program cmd))))

#+tuition-single-threaded
(defun enqueue-deferred-signal-thunk (program thunk)
  "Queue THUNK for execution on the main loop (safe from signal handlers)."
  (st-queue-push (program-pending-signal-thunks program) thunk))

#+tuition-single-threaded
(defun process-pending-signal-thunks (program)
  "Run at most one signal-deferred thunk on the main loop."
  (let ((thunk (st-queue-pop (program-pending-signal-thunks program))))
    (when thunk
      (handler-case (funcall thunk)
        (error (e)
          (handle-error :signal-defer e))))))

(defun cleanup-program-terminal (program)
  "Restore terminal modes from the last view-state."
  (let ((vs (last-view-state (program-renderer program))))
    (when vs
      (when (view-state-mouse-mode vs) (disable-mouse))
      (when (view-state-report-focus vs) (disable-focus-events))
      (when (view-state-keyboard-enhancements vs) (disable-kitty-keyboard))
      (when (view-state-alt-screen vs) (exit-alt-screen)))))

(defun shutdown-program (program)
  "Shared shutdown: stop workers and restore terminal state."
  (setf (program-running program) nil)
  #-tuition-single-threaded
  (when (program-cmd-pool program)
    (shutdown-pool (program-cmd-pool program))
    (setf (program-cmd-pool program) nil))
  (cleanup-program-terminal program))

(defun prepare-program-run (program pool-size)
  "Common startup: bind globals, open tty, init thread pool, set dimensions."
  #+tuition-single-threaded
  (declare (ignore pool-size))
  (let* ((*current-program* program)
         (tty-stream (get-tty-stream))
         (*standard-output* (or tty-stream *standard-output*)))
    (setf (program-running program) t)
    #-tuition-single-threaded
    (when (and *use-thread-pool* pool-size)
      (setf (program-cmd-pool program) (make-pool pool-size program)))
    (setf (program-tty-stream program) (or tty-stream *terminal-io*))
    (when tty-stream
      (setf (output-stream (program-renderer program)) tty-stream))
    (values tty-stream)))

(defun run-initial-model (program)
  "Run init command, send window size, and render the first frame."
  (let* ((size (get-terminal-size))
         (width (car size))
         (height (cdr size)))
    (setf (renderer-width (program-renderer program)) width
          (renderer-height (program-renderer program)) height)
    (send program (make-window-size-msg :width width :height height)))
  (let ((init-cmd (init (program-model program))))
    (when init-cmd
      (run-command program init-cmd)))
  (render (program-renderer program)
          (view (program-model program))))

#-tuition-single-threaded
(defun join (thread)
  "Join a thread (compat wrapper)."
  (bt:join-thread thread))

#+tuition-single-threaded
(defun join (thread)
  "No-op in single-threaded mode (no background threads to join)."
  (declare (ignore thread))
  nil)

#-tuition-single-threaded
(defun defer-from-signal (program thunk &optional name)
  "Run THUNK to send a message from a signal handler safely."
  (declare (ignore program))
  (bt:make-thread thunk :name (or name "tuition-signal-defer")))

#+tuition-single-threaded
(defun defer-from-signal (program thunk &optional name)
  "Queue THUNK for the main loop (do not run Lisp from a signal handler)."
  (declare (ignore name))
  (enqueue-deferred-signal-thunk program thunk))

(defun run (program)
  "Run the program's main loop. Blocks until the program exits.
In v2, starts with minimal terminal setup (raw mode only).
Terminal modes are applied declaratively from the first view-state render."
  (let* ((opts (program-options program))
         (pool-size (getf opts :pool-size)))
    (with-raw-terminal ()
      (prepare-program-run program pool-size)
      (flet ((run-main-loop ()
               #-tuition-single-threaded
               (let ((input-thread (bt:make-thread
                                    (lambda () (input-loop program))
                                    :name "tuition-input")))
                 (run-initial-model program)
                 (event-loop program)
                 (shutdown-program program)
                 (bt:join-thread input-thread))
               #+tuition-single-threaded
               (progn
                 (run-initial-model program)
                 (unified-event-loop program)
                 (shutdown-program program))))
        #+(and sbcl (not windows))
        (let ((old-sigwinch-handler nil)
              (old-sigtstp-handler nil)
              (old-sigcont-handler nil))
          (setf old-sigwinch-handler
                (sb-sys:enable-interrupt
                 sb-posix:sigwinch
                 (lambda (signal code scp)
                   (declare (ignore signal code scp))
                   (unless *exec-suspended*
                     (let* ((size (get-terminal-size))
                            (width (car size))
                            (height (cdr size)))
                       (setf (renderer-width (program-renderer program)) width
                             (renderer-height (program-renderer program)) height)
                       (defer-from-signal
                        program
                        (lambda ()
                          (send program (make-window-size-msg :width width :height height)))
                        "tuition-sigwinch"))))))
          (setf old-sigtstp-handler
                (sb-sys:enable-interrupt
                 sb-posix:sigtstp
                 (lambda (signal code scp)
                   (declare (ignore signal code scp))
                   (let* ((renderer (program-renderer program))
                          (vs (last-view-state renderer)))
                     (setf (program-restore-fn program)
                           (suspend-terminal
                            :alt-screen (and vs (view-state-alt-screen vs))
                            :mouse (and vs (view-state-mouse-mode vs))
                            :focus-events (and vs (view-state-report-focus vs)))))
                   (sb-posix:kill (sb-posix:getpid) sb-posix:sigstop))))
          (setf old-sigcont-handler
                (sb-sys:enable-interrupt
                 sb-posix:sigcont
                 (lambda (signal code scp)
                   (declare (ignore signal code scp))
                   (when (program-restore-fn program)
                     (resume-terminal (program-restore-fn program))
                     (setf (program-restore-fn program) nil))
                   (defer-from-signal
                    program
                    (lambda () (send program (make-resume-msg)))
                    "tuition-sigcont"))))
          (run-main-loop)
          (when old-sigwinch-handler
            (sb-sys:enable-interrupt sb-posix:sigwinch old-sigwinch-handler))
          (when old-sigtstp-handler
            (sb-sys:enable-interrupt sb-posix:sigtstp old-sigtstp-handler))
          (when old-sigcont-handler
            (sb-sys:enable-interrupt sb-posix:sigcont old-sigcont-handler)))
        #-(and sbcl (not windows))
        (run-main-loop))
      (close-tty-stream))))

(defun process-channel-messages (program)
  "Drain and handle all pending messages on PROGRAM's channel.
Returns T when any messages were processed."
  (let ((first-msg (trivial-channels:getmsg (msg-channel program))))
    (when first-msg
      (let ((messages (list first-msg)))
        (loop for msg = (trivial-channels:getmsg (msg-channel program))
              while msg
              do (push msg messages))
        (handle-messages-batch program (nreverse messages))
        t))))

(defun event-loop (program)
  "Main event processing loop with batched message processing.
Uses non-blocking getmsg with sleep to avoid channel mutex
issues with timed recvmsg on SBCL."
  (loop while (program-running program) do
    (handler-case
        (if (process-channel-messages program)
            nil
            (sleep 0.005))
      (error (e)
        (handle-error :event-loop e)))))

#+tuition-single-threaded
(defun unified-event-loop (program)
  "Single-threaded loop: input polling, message processing, and command execution."
  (setf *input-stream* (program-tty-stream program))
  (loop while (program-running program) do
    (handler-case
        (progn
          (process-pending-signal-thunks program)
          (process-one-pending-command program)
          (unless (program-input-paused program)
            (let ((events (read-all-available-events)))
              (when events
                (%ilog "input-loop: batch of ~D events" (length events))
                (send-batch program events))))
          (if (process-channel-messages program)
              nil
              (sleep 0.001)))
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

        ;; Raw escape writes (clipboard, color queries) - written here on
        ;; the render thread so they never interleave with a frame
        ((write-escape-msg-p msg)
         (let ((stream (output-stream (program-renderer program))))
           (write-string (write-escape-msg-sequence msg) stream)
           (force-output stream)))

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
     #-tuition-single-threaded
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
            :name "tuition-cmd")))
     #+tuition-single-threaded
     (enqueue-async-command program cmd))
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

(defun %run-sequence-thunk (program cmds)
  (lambda ()
    (dolist (cmd cmds)
      (when (and cmd (program-running program))
        (handler-case
            (let ((msg (funcall cmd)))
              (when msg
                (send program msg)))
          (error (e)
            (handle-error :command e)))))))

(defun run-sequence (program cmds)
  "Run multiple commands in sequence.

An error in one command is reported via HANDLE-ERROR and the sequence
continues with the next command, rather than silently killing the
sequence thread (mirrors bubbletea's nested-panic recovery)."
  #-tuition-single-threaded
  (bt:make-thread (%run-sequence-thunk program cmds) :name "tuition-sequence")
  #+tuition-single-threaded
  (dolist (cmd cmds)
    (when cmd
      (enqueue-async-command
       program
       (lambda ()
         (when (program-running program)
           (%invoke-queued-command program cmd)))))))

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
