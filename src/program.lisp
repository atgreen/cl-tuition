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

(defun make-program (model &key alt-screen mouse (pool-size *default-pool-size*))
  "Create a new program with the given initial model.

Options (keyword args only):
  :alt-screen t|nil
  :mouse      one of :cell-motion, :all-motion, or NIL
  :pool-size  number of worker threads for command execution (default: 4)
              Set to NIL to disable thread pool (spawns thread per command)"
  (when (and mouse (not (member mouse '(:cell-motion :all-motion) :test #'eq)))
    (error "Invalid :mouse option ~S; expected :cell-motion, :all-motion, or NIL" mouse))
  (when (and pool-size (not (and (integerp pool-size) (> pool-size 0))))
    (error "Invalid :pool-size ~S; expected positive integer or NIL" pool-size))
  (let ((opts (list :alt-screen (not (null alt-screen))
                    :mouse mouse
                    :pool-size pool-size)))
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
  "Run the program's main loop. Blocks until the program exits."
  (let* ((opts (program-options program))
         (alt (getf opts :alt-screen))
         (mouse (getf opts :mouse))
         (pool-size (getf opts :pool-size))
         (*current-program* program))
    (with-raw-terminal (:alt-screen alt :mouse mouse)
      (setf (program-running program) t)

      ;; Initialize thread pool if enabled
      (when (and *use-thread-pool* pool-size)
        (setf (program-cmd-pool program) (make-pool pool-size program)))

      ;; Use *terminal-io* for input
      (setf (program-tty-stream program) *terminal-io*)

      ;; Set up signal handlers (POSIX signals not available on Windows)
      #+(and sbcl (not windows))
      (let ((old-sigwinch-handler nil)
            (old-sigtstp-handler nil)
            (old-sigcont-handler nil))
        ;; SIGWINCH - terminal resize
        (setf old-sigwinch-handler
              (sb-sys:enable-interrupt
               sb-posix:sigwinch
               (lambda (signal code scp)
                 (declare (ignore signal code scp))
                 ;; Ignore resize signals when suspended for exec
                 (unless *exec-suspended*
                   (let* ((size (get-terminal-size))
                          (width (car size))
                          (height (cdr size)))
                     (send program (make-window-size-msg :width width :height height)))))))

        ;; SIGTSTP - suspend (Ctrl+Z)
        (setf old-sigtstp-handler
              (sb-sys:enable-interrupt
               sb-posix:sigtstp
               (lambda (signal code scp)
                 (declare (ignore signal code scp))
                 ;; Suspend the terminal and store restore function
                 (setf (program-restore-fn program)
                       (suspend-terminal :alt-screen alt :mouse mouse))
                 ;; Send ourselves SIGSTOP to actually suspend
                 (sb-posix:kill (sb-posix:getpid) sb-posix:sigstop))))

        ;; SIGCONT - resume after suspension
        (setf old-sigcont-handler
              (sb-sys:enable-interrupt
               sb-posix:sigcont
               (lambda (signal code scp)
                 (declare (ignore signal code scp))
                 ;; Restore the terminal
                 (when (program-restore-fn program)
                   (resume-terminal (program-restore-fn program))
                   (setf (program-restore-fn program) nil))
                 ;; Send a resume message to the application
                 (send program (make-resume-msg)))))

        ;; Start input thread
        (let ((input-thread (bt:make-thread
                             (lambda () (input-loop program))
                             :name "tuition-input")))

          ;; Send initial window size
          (let* ((size (get-terminal-size))
                 (width (car size))
                 (height (cdr size)))
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

          ;; Cooperative shutdown: ask input loop to exit and join
          (setf (program-running program) nil)

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
        ;; Non-SBCL or Windows: no SIGWINCH support, just run without resize handling
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

          ;; Shutdown thread pool if active
          (when (program-cmd-pool program)
            (shutdown-pool (program-cmd-pool program))
            (setf (program-cmd-pool program) nil))

          (bt:join-thread input-thread))))))

;; Terminal setup/cleanup handled by WITH-RAW-TERMINAL

(defun event-loop (program)
  "Main event processing loop with batched message processing."
  (loop while (program-running program) do
    (handler-case
        ;; Block on condition variable until message arrives (with timeout to check running flag)
        (let ((first-msg (trivial-channels:recvmsg (msg-channel program) 0.1)))
          (when first-msg
            ;; Got a message - drain all pending and process as batch
            (let ((messages (list first-msg)))
              ;; Collect all immediately available messages
              (loop for msg = (trivial-channels:getmsg (msg-channel program))
                    while msg
                    do (push msg messages))
              (setf messages (nreverse messages))
              ;; Process batch
              (handle-messages-batch program messages))))
      (error (e)
        (handle-error :event-loop e)))))

(defun coalesce-scroll-events (messages)
  "Combine consecutive scroll events in the same direction into one.
   The count slot accumulates how many events were combined.
   Returns the coalesced message list."
  (when (null messages)
    (return-from coalesce-scroll-events nil))
  (let ((result nil)
        (prev-scroll nil))  ; The scroll event we're accumulating into
    (dolist (msg messages)
      (cond
        ;; Scroll event
        ((mouse-scroll-event-p msg)
         (let ((dir (mouse-scroll-direction msg)))
           (if (and prev-scroll (eq dir (mouse-scroll-direction prev-scroll)))
               ;; Same direction as previous - increment count
               (incf (mouse-scroll-count prev-scroll))
               ;; New direction or first scroll - start new accumulator
               (progn
                 (push msg result)
                 (setf prev-scroll msg)))))
        ;; Non-scroll event breaks the scroll sequence
        (t
         (push msg result)
         (setf prev-scroll nil))))
    (nreverse result)))

(defun handle-messages-batch (program messages)
  "Process multiple messages, rendering only once at the end."
  ;; Coalesce scroll events to avoid jumping multiple rows per wheel click
  (let ((messages (coalesce-scroll-events messages))
        (should-render nil)
        (pending-cmds nil))
    (dolist (msg messages)
      (cond
        ;; Quit message - stop immediately
        ((quit-msg-p msg)
         (setf (program-running program) nil)
         (return-from handle-messages-batch))

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
  "Execute a command.

  If thread pool is enabled and available, submits the command to the pool.
  Otherwise, spawns a new thread for each command (original behavior).
  exec-cmd is handled specially with full TUI suspension."
  (cond
    ;; Nil command - do nothing
    ((null cmd) nil)

    ;; Exec command - run external program with full TUI suspension
    ((exec-cmd-p cmd)
     (run-exec-command program cmd))

    ;; Batch commands
    ((listp cmd)
     (if (eql (first cmd) :sequence)
         (run-sequence program (rest cmd))
         (run-batch program cmd)))

    ;; Single command function
    ((functionp cmd)
     (let ((pool (program-cmd-pool program)))
       (if (and pool (thread-pool-running pool))
           ;; Use thread pool
           (submit-command pool cmd)
           ;; Fallback to spawning a thread (original behavior)
           ;; This matches Go's Bubble Tea which spawns a goroutine per command
           (bt:make-thread
            (lambda ()
              (handler-case
                  (let ((msg (funcall cmd)))
                    (when msg
                      (send program msg)))
                (error (e)
                  (handle-error :command e))))
            :name "tuition-cmd"))))

    ;; Unknown command type
    (t nil)))

(defun run-exec-command (program cmd)
  "Run an external program with full TUI suspension.
   Pauses input, suspends signal handling, suspends terminal,
   runs the program, then restores everything."
  (let* ((opts (program-options program))
         (alt (getf opts :alt-screen))
         (mouse (getf opts :mouse))
         (exec-program (exec-cmd-program cmd))
         (exec-args (exec-cmd-args cmd))
         (callback (exec-cmd-callback cmd)))
    ;; Pause input reading
    (setf (program-input-paused program) t)
    ;; Mark as suspended (prevents signal handlers from sending messages)
    (setf *exec-suspended* t)
    ;; Suspend terminal (exit alt screen, restore cooked mode, etc.)
    (let ((restore-fn (suspend-terminal :alt-screen alt :mouse mouse)))
      (unwind-protect
          (handler-case
              ;; Run the external program
              (uiop:run-program (cons exec-program exec-args)
                                :input :interactive
                                :output :interactive
                                :error-output :interactive)
            (error (e)
              (handle-error :exec-command e)))
        ;; Always restore terminal state
        (when restore-fn
          (resume-terminal restore-fn))
        ;; Clear suspended flag
        (setf *exec-suspended* nil)
        ;; Resume input reading
        (setf (program-input-paused program) nil)
        ;; Send a window size message to trigger redraw with correct size
        (let* ((size (get-terminal-size))
               (width (car size))
               (height (cdr size)))
          (send program (make-window-size-msg :width width :height height)))
        ;; Call callback if provided
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
  "Read all available input events and return them as a list.
   Returns nil if no input is available."
  (let ((events nil))
    (loop for msg = (read-key)
          while msg
          do (push msg events))
    (nreverse events)))

(defun send-batch (program msgs)
  "Send multiple messages atomically to ensure proper batching."
  (when (and (program-running program) msgs)
    (trivial-channels:sendmsgs (msg-channel program) msgs)))

(defun input-loop (program)
  "Read input and send input messages (key/mouse/paste) to the program.
   Reads ALL available events and sends them atomically to ensure proper batching.
   When program-input-paused is T, input reading is suspended."
  ;; Set the input stream for this thread
  (setf *input-stream* (program-tty-stream program))
  ;; Suppress SBCL warnings about I/O operations in this thread (Unix only)
  #+(and sbcl (not windows))
  (handler-bind ((warning #'muffle-warning))
    (handler-case
        (loop while (program-running program) do
          (handler-case
              (if (program-input-paused program)
                  ;; When paused, just sleep and don't read input
                  (sleep 0.05)
                  ;; Normal input reading
                  (let ((events (read-all-available-events)))
                    (if events
                        (progn
                          (%ilog "input-loop: batch of ~D events" (length events))
                          (send-batch program events))
                        ;; Only sleep when no input available (prevent busy-waiting)
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
                      ;; Only sleep when no input available (prevent busy-waiting)
                      (sleep 0.001))))
          (error (e)
            (handle-error :input-loop e))))
    (error (e)
      (handle-error :input-loop e))))

;;; Convenience macro to define a program class and its handlers
(defmacro defprogram (class-name &key slots init update view)
  "Define a model class named CLASS-NAME and its TEA protocol methods.

Parameters:
- SLOTS: list of DEFCLASS slot specs.
- INIT:  body producing the initial command (evaluated with MODEL bound).
- UPDATE: body that receives (model msg) and returns (values new-model cmd).
- VIEW:  body that receives MODEL and returns a string.

Example:
  (tui:defprogram counter
    :slots ((count :initform 0 :accessor count))
    :init  nil
    :update (cond ((and (tui:key-msg-p msg) (char= (tui:key-msg-key msg) #\q))
                   (values model (tui:quit-cmd)))
                  (t (values model nil)))
    :view (format nil \"Count: ~D\" (count model)))"
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

;;; No sentinel-style helpers; prefer keyword options in make-program.
