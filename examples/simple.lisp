;;;; Simple countdown example - counts down from 5 and exits

(defpackage #:tuition-example-simple
  (:use #:cl #:tuition))

(in-package #:tuition-example-simple)

;;; Model and protocol using defprogram
(tui:defprogram countdown-model
  :slots ((seconds :initarg :seconds :accessor seconds :initform 5))
  :init  (tick-cmd)
  :view (format nil "Hi. This program will exit in ~D seconds.~%~%To quit sooner press q or ctrl-c...~%"
                (seconds model)))

;;; Message for timer ticks (CLOS)
(tui:defmessage tick-msg ())

;;; Handle tick messages with generic dispatch
(defmethod tui:update-message ((model countdown-model) (msg tick-msg))
  (declare (ignore msg))
  (decf (seconds model))
  (if (<= (seconds model) 0)
      (values model (tui:quit-cmd))
      (values model (tick-cmd))))

;;; Handle key messages with generic dispatch
(defmethod tui:update-message ((model countdown-model) (msg tui:key-msg))
  (let ((key (tui:key-string msg)))
    (cond
      ((or (string= key "q")
           (and (tui:key-msg-ctrl msg) (char= (tui:key-msg-key msg) #\c)))
       (values model (tui:quit-cmd)))
      ;; Suspend on ctrl+z (not implemented)
      ((and (tui:key-msg-ctrl msg) (char= (tui:key-msg-key msg) #\z))
       (values model nil))
      (t (values model nil)))))

;;; Command to create timer ticks
(defun tick-cmd ()
  (lambda ()
    (sleep 1)
    (make-instance 'tick-msg)))


;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'countdown-model :seconds 5))))
    (tui:run program)))

;;; Allow running from command line
#+nil
(main)
