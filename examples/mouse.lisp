;;;; SPDX-License-Identifier: MIT
;;;; Mouse example - demonstrates mouse tracking

(defpackage #:tuition-example-mouse
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-mouse)

;;; Model
(defclass mouse-model ()
  ((last-event :initform nil :accessor mouse-last-event)))

;;; Init
(defmethod tui:init ((model mouse-model))
  nil)

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model mouse-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((and (characterp key) (char= key #\q)) (values model (tui:quit-cmd)))
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c)) (values model (tui:quit-cmd)))
      ((eq key :escape) (values model (tui:quit-cmd)))
      (t (values model nil)))))

(defmethod tui:update-message ((model mouse-model) (msg tui:mouse-msg))
  (setf (mouse-last-event model) msg)
  (values model nil))

;;; Helper to format mouse event
(defun format-mouse-event (msg)
  "Format a mouse event as a string"
  (if msg
      (format nil "(X: ~D, Y: ~D) ~A ~A~A~A~A"
              (tui:mouse-msg-x msg)
              (tui:mouse-msg-y msg)
              (tui:mouse-msg-action msg)
              (or (tui:mouse-msg-button msg) "")
              (if (tui:mouse-msg-shift msg) " +shift" "")
              (if (tui:mouse-msg-alt msg) " +alt" "")
              (if (tui:mouse-msg-ctrl msg) " +ctrl" ""))
      "No mouse events yet"))

;;; View
(defmethod tui:view ((model mouse-model))
  (format nil "~%Do mouse stuff. When you're done press q to quit.~%~%~
               Last event: ~A~%~%"
          (format-mouse-event (mouse-last-event model))))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'mouse-model)
                                   :mouse :all-motion)))
    (tui:run program)))

#+nil
(main)
