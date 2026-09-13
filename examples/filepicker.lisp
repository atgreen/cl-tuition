;;; filepicker.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
;;;
;;;; Filepicker example - pick a file from the filesystem

(asdf:load-system :tuition)

(defpackage #:tuition-example-filepicker
  (:use #:cl #:tuition)
  (:documentation "Filepicker example - navigate and select a file.")
  (:export #:main))

(in-package #:tuition-example-filepicker)

;;; Model
(defclass filepicker-model ()
  ((picker :initarg :picker :accessor model-picker)
   (selected :initform nil :accessor model-selected
             :documentation "The path the user picked, if any")
   (rejected :initform nil :accessor model-rejected
             :documentation "A disallowed path the user tried to pick"))
  (:documentation "Model for the filepicker example application."))

(defun make-model ()
  (make-instance 'filepicker-model
                 :picker (tui.filepicker:make-filepicker
                          :current-directory "."
                          ;; Uncomment to only allow picking Lisp sources:
                          ;; :allowed-types '(".lisp" ".asd")
                          )))

;;; Init
(defmethod tui:init ((model filepicker-model))
  (tui.filepicker:filepicker-init (model-picker model)))

;;; Update
(defun %delegate (model msg)
  (multiple-value-bind (picker cmd)
      (tui.filepicker:filepicker-update (model-picker model) msg)
    (setf (model-picker model) picker)
    (multiple-value-bind (did path)
        (tui.filepicker:filepicker-did-select-file picker msg)
      (when did
        (setf (model-selected model) path
              (model-rejected model) nil)))
    (multiple-value-bind (did path)
        (tui.filepicker:filepicker-did-select-disabled-file picker msg)
      (when did
        (setf (model-rejected model) path)))
    (values model cmd)))

(defmethod tui:update-message ((model filepicker-model) (msg tui:key-press-msg))
  (let ((key (tui:key-event-code msg)))
    (if (or (and (characterp key) (char= key #\q))
            (and (tui:mod-contains (tui:key-event-mod msg) tui:+mod-ctrl+)
                 (characterp key) (char= key #\c)))
        (values model (tui:quit-cmd))
        (%delegate model msg))))

(defmethod tui:update-message ((model filepicker-model) (msg t))
  ;; Read-dir results and window-size messages go to the picker too.
  (%delegate model msg))

;;; View
(defmethod tui:view ((model filepicker-model))
  (let ((picker (model-picker model)))
    (tui:make-view
     (format nil "~%  Pick a file:  (~A)~%~%~A~%  ~A~%~%  ~
j/k or arrows: move   Enter: open/select   Backspace: up   q: quit~%"
             (tui.filepicker:filepicker-current-directory picker)
             (tui.filepicker:filepicker-view picker)
             (cond ((model-rejected model)
                    (format nil "That file type isn't allowed: ~A"
                            (model-rejected model)))
                   ((model-selected model)
                    (format nil "Selected: ~A" (model-selected model)))
                   (t "Nothing selected yet."))))))

;;; Main entry point
(defun main ()
  "Run the filepicker example application."
  (let ((program (tui:make-program (make-model))))
    (tui:run program)))

(eval-when (:load-toplevel :execute)
  (main))
