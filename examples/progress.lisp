;;; progress.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Progress bar example - demonstrates animated progress

(defpackage #:tuition-example-progress
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-progress)

;;; Messages
(tui:defmessage tick-msg ())

;;; Model
(defclass progress-model ()
  ((percent :initform 0.0 :accessor progress-percent)
   (width :initform 40 :accessor progress-width)))

;;; Init
(defmethod tui:init ((model progress-model))
  (lambda ()
    (sleep 1.0)
    (make-instance 'tick-msg)))

;;; Helper to render progress bar
(defun render-progress-bar (percent width)
  "Render a simple ASCII progress bar"
  (let* ((filled (floor (* percent width)))
         (empty (- width filled))
         (bar (concatenate 'string
                          (make-string filled :initial-element #\█)
                          (make-string empty :initial-element #\░))))
    (format nil "[~A] ~3D%" bar (floor (* percent 100)))))

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model progress-model) (msg tui:key-msg))
  (declare (ignore msg))
  (values model (tui:quit-cmd)))

(defmethod tui:update-message ((model progress-model) (msg tick-msg))
  (let ((new-percent (min 1.0 (+ (progress-percent model) 0.25))))
    (setf (progress-percent model) new-percent)
    (if (>= new-percent 1.0)
        ;; Done - quit
        (values model (tui:quit-cmd))
        ;; Continue
        (values model
                (lambda ()
                  (sleep 1.0)
                  (make-instance 'tick-msg))))))

;;; View
(defmethod tui:view ((model progress-model))
  (format nil "~%  ~A~%~%  Press any key to quit~%"
          (render-progress-bar (progress-percent model)
                              (progress-width model))))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'progress-model))))
    (tui:run program)))

#+nil
(main)
