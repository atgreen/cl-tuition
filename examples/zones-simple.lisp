;;; zones-simple.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Simple zones example - for debugging

(defpackage #:tuition-example-zones-simple
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-zones-simple)

;;; Model
(defclass simple-zones-model ()
  ((clicked :initform nil :accessor model-clicked)))

;;; Init
(defmethod tui:init ((model simple-zones-model))
  ;; Initialize global zone manager
  (tui:init-global-zone-manager)
  nil)

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model simple-zones-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (when (and (characterp key) (char= key #\q))
      (return-from tui:update-message (values model (tui:quit-cmd))))
    (values model nil)))

(defmethod tui:update-message ((model simple-zones-model) (msg tui:mouse-msg))
  (when (and (eql (tui:mouse-msg-action msg) :press)
             (eql (tui:mouse-msg-button msg) :left))
    (let ((zone (tui:zone-get "test-button")))
      (when (tui:zone-in-bounds-p zone msg)
        (setf (model-clicked model) t))))
  (values model nil))

;;; View
(defmethod tui:view ((model simple-zones-model))
  ;; Simple view without zone-scan first to test
  (format nil "~%Simple Zones Test~%~%~A~%~%Status: ~A~%~%Press q to quit~%"
          "[Click Here]"
          (if (model-clicked model) "CLICKED!" "Not clicked yet")))

;;; Main
(defun main ()
  (let ((program (tui:make-program (make-instance 'simple-zones-model)
                                   :alt-screen t
                                   :mouse :cell-motion)))
    (tui:run program)))

#+nil
(main)
