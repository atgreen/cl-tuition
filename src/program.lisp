;;; program.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Main program loop and runtime

(in-package #:tuition)

(
defclass program ()
  ((model :initarg :model :accessor program-model)
   (renderer :initform (make-instance 'renderer) :accessor program-renderer)
   (msg-channel :initform (trivial-channels:make-channel) :accessor msg-channel)
   (running :initform nil :accessor program-running)
   (options :initarg :options :initform nil :accessor program-options)
   (tty-stream :initform nil :accessor program-tty-stream))
  (:documentation "A Bubble Tea program instance."))

(defun make-program (model &key alt-screen mouse)
  "Create a new program with the given initial model.

Options (keyword args only):
  :alt-screen t|nil
  :mouse      one of :cell-motion, :all-motion, or NIL"
  (when (and mouse (not (member mouse '(:cell-motion :all-motion) :test #'eq)))
    (error "Invalid :mouse option ~S; expected :cell-motion, :all-motion, or NIL" mouse))
  (let ((opts (list :alt-screen (not (null alt-screen))
                    :mouse mouse)))
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
         (mouse (getf opts :mouse)))
    (with-raw-terminal (:alt-screen alt :mouse mouse)
      (setf (program-running program) t)
      ;; Use *terminal-io* for input
      (setf (program-tty-stream program) *terminal-io*)

      ;; Start input thread
      (let ((input-thread (bt:make-thread
                           (lambda () (input-loop program))
                           :name "tuition-input")))

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
        (bt:join-thread input-thread)))))

;; Terminal setup/cleanup handled by WITH-RAW-TERMINAL

(defun event-loop (program)
  "Main event processing loop."
  (loop while (program-running program) do
    (handler-case
        (let ((msg (trivial-channels:getmsg (msg-channel program))))
          (when msg
            (handle-message program msg))
          (unless msg
            (sleep 0.01))) ; Small sleep if no messages
      (error (e)
        (handle-error :event-loop e)))))

(defun handle-message (program msg)
  "Process a message through the update function."
  (cond
    ;; Quit message
    ((quit-msg-p msg)
     (setf (program-running program) nil))

    ;; All other messages
    (t
     (multiple-value-bind (new-model cmd)
         (update (program-model program) msg)
       (setf (program-model program) new-model)

       ;; Run command if returned
       (when cmd
         (run-command program cmd))

       ;; Re-render
       (render (program-renderer program)
               (view new-model))))))

(defun run-command (program cmd)
  "Execute a command in a separate thread."
  (cond
    ;; Nil command - do nothing
    ((null cmd) nil)

    ;; Batch commands
    ((listp cmd)
     (if (eql (first cmd) :sequence)
         (run-sequence program (rest cmd))
         (run-batch program cmd)))

    ;; Single command function
    ((functionp cmd)
     (bt:make-thread
      (lambda ()
        (let ((msg (funcall cmd)))
          (when msg
            (send program msg))))
      :name "tuition-cmd"))

    ;; Unknown command type
    (t nil)))


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

(defun input-loop (program)
  "Read input and send key messages to the program."
  ;; Set the input stream for this thread
  (setf *input-stream* (program-tty-stream program))
  (handler-case
      (loop while (program-running program) do
        (handler-case
            (let ((key-msg (read-key)))
              (when key-msg
                (send program key-msg)))
          (error (e)
            (handle-error :input-loop e)))
        (sleep 0.01)) ; Small delay to prevent busy-waiting
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
  (let ((m (gensym "MODEL"))
        (msg (gensym "MSG")))
    `(progn
       (defclass ,class-name () ,(or slots '()))
       ,(when (or init (null init))
          `(defmethod init ((,m ,class-name))
             ,init))
       ,(when update
          `(defmethod update ((,m ,class-name) ,msg)
             ,update))
       ,(when view
          `(defmethod view ((,m ,class-name))
             ,view)))))

;;; No sentinel-style helpers; prefer keyword options in make-program.
