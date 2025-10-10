;;; spinner-component.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Spinner component example - demonstrates using the reusable spinner component

(defpackage #:tuition-example-spinner-component
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-spinner-component)

;;; Model
(defclass spinner-component-model ()
  ((spinner :initform (tui.spinner:make-spinner
                       :frames tui.spinner:*spinner-dot*
                       :fps 0.1)
            :accessor model-spinner)
   (quitting :initform nil :accessor model-quitting)))

;;; Init - delegate to spinner
(defmethod tui:init ((model spinner-component-model))
  (tui.spinner:spinner-init (model-spinner model)))

;;; Update
(defmethod tui:update ((model spinner-component-model) msg)
  (cond
    ;; Handle key presses
    ((tui:key-msg-p msg)
     (let ((key (tui:key-msg-key msg)))
       (cond
         ;; Quit on q or ctrl+c
         ((and (characterp key) (char= key #\q))
          (setf (model-quitting model) t)
          (values model (tui:quit-cmd)))

         ((and (tui:key-msg-ctrl msg)
               (characterp key)
               (char= key #\c))
          (setf (model-quitting model) t)
          (values model (tui:quit-cmd)))

         (t (values model nil)))))

    ;; Delegate spinner tick messages to the spinner component
    ((tui.spinner:spinner-tick-msg-p msg)
     (multiple-value-bind (new-spinner cmd)
         (tui.spinner:spinner-update (model-spinner model) msg)
       (setf (model-spinner model) new-spinner)
       (values model cmd)))

    (t (values model nil))))

;;; View - use spinner's view
(defmethod tui:view ((model spinner-component-model))
  (format nil "~%~%   ~A Loading forever...press q to quit~%~%~A"
          (tui.spinner:spinner-view (model-spinner model))
          (if (model-quitting model) "~%" "")))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'spinner-component-model))))
    (tui:run program)))

#+nil
(main)
