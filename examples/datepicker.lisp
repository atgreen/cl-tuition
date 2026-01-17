;;; datepicker.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Date picker example - demonstrates interactive calendar date selection

(asdf:load-system :tuition)

(defpackage #:tuition-example-datepicker
  (:use #:cl #:tuition)
  (:documentation "Date picker example - interactive calendar date selection.")
  (:export #:main))

(in-package #:tuition-example-datepicker)

;;; Model
(defclass datepicker-model ()
  ((picker :initarg :picker
           :accessor model-picker
           :documentation "The datepicker component")
   (quitting :initform nil
             :accessor model-quitting))
  (:documentation "Model for the datepicker example application."))

(defun make-model ()
  "Create a new datepicker model."
  (make-instance 'datepicker-model
                 :picker (tui.datepicker:make-datepicker)))

;;; Init
(defmethod tui:init ((model datepicker-model))
  (tui.datepicker:datepicker-init (model-picker model)))

;;; Update
(defmethod tui:update-message ((model datepicker-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Quit on q or Ctrl+C
      ((or (and (characterp key) (char= key #\q))
           (and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c)))
       (setf (model-quitting model) t)
       (values model (tui:quit-cmd)))

      ;; Pass other keys to datepicker
      (t
       (multiple-value-bind (new-picker cmd)
           (tui.datepicker:datepicker-update (model-picker model) msg)
         (setf (model-picker model) new-picker)
         (values model cmd))))))

;;; View
(defmethod tui:view ((model datepicker-model))
  (let* ((picker (model-picker model))
         (selected (tui.datepicker:datepicker-selected picker))
         (selected-str (if selected
                           (multiple-value-bind (sec min hour day month year)
                               (decode-universal-time selected)
                             (declare (ignore sec min hour))
                             (format nil "Selected: ~D/~D/~D" month day year))
                           "No date selected")))
    (format nil "~%  Date Picker Demo~%~%~A~%~%  ~A~%~%  ~
Controls:~%  Arrow keys: Navigate days/weeks~%  [ / ]: Previous/next month~%  ~
{ / }: Previous/next year~%  Enter/Space: Select date~%  Escape: Clear selection~%  ~
Home: Go to today~%  q: Quit~%"
            (tui.datepicker:datepicker-view picker)
            selected-str)))

;;; Main entry point
(defun main ()
  "Run the datepicker example application."
  (let ((program (tui:make-program (make-model))))
    (tui:run program)))

(eval-when (:load-toplevel :execute)
  (main))
