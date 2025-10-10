;;;; SPDX-License-Identifier: MIT
;;;; Condition types and error handling hooks

(in-package #:tuition)

(define-condition tuition-error (error)
  ()
  (:documentation "Base condition for Tuition errors."))

(define-condition terminal-error (tuition-error)
  ((reason :initarg :reason :reader terminal-error-reason))
  (:report (lambda (c s)
             (format s "Terminal error: ~A" (terminal-error-reason c)))))

(define-condition terminal-operation-error (terminal-error)
  ((operation :initarg :operation :reader terminal-error-operation))
  (:report (lambda (c s)
             (format s "Terminal ~A failed: ~A"
                     (terminal-error-operation c)
                     (terminal-error-reason c)))))

(define-condition input-error (tuition-error)
  ((reason :initarg :reason :reader input-error-reason))
  (:report (lambda (c s)
             (format s "Input error: ~A" (input-error-reason c)))))

(defparameter *error-handler*
  (lambda (where condition)
    (declare (ignore where))
    (warn "~A" condition))
  "Function of (WHERE CONDITION) called when internal errors are caught.
WHERE is a keyword indicating origin, e.g., :event-loop, :input-loop, :terminal.
Users may bind or set this to customize error reporting.")

(defun handle-error (where condition)
  "Invoke the configured error handler for a caught CONDITION at WHERE."
  (funcall *error-handler* where condition))
