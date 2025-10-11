;;; mouse.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Mouse example - demonstrates mouse tracking

(defpackage #:tuition-example-mouse
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-mouse)

(eval-when (:load-toplevel :execute)
  (asdf:load-system :tuition))

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

(defmethod tui:update-message ((model mouse-model) (msg tui:mouse-event))
  (setf (mouse-last-event model) msg)
  (values model nil))

;;; Helper to format mouse event
(defun format-mouse-event (msg)
  "Format a mouse event as a string"
  (if msg
      (let ((x (tui:mouse-event-x msg))
            (y (tui:mouse-event-y msg))
            (shift (tui:mouse-event-shift msg))
            (alt (tui:mouse-event-alt msg))
            (ctrl (tui:mouse-event-ctrl msg))
            (event-type (cond
                          ((tui:mouse-press-event-p msg) "press")
                          ((tui:mouse-release-event-p msg) "release")
                          ((tui:mouse-drag-event-p msg) "drag")
                          ((typep msg 'tui:mouse-move-event) "move")
                          ((tui:mouse-scroll-event-p msg) "scroll")
                          (t "unknown")))
            (button (when (typep msg 'tui:mouse-button-event)
                      (tui:mouse-event-button msg)))
            (direction (when (tui:mouse-scroll-event-p msg)
                         (tui:mouse-scroll-direction msg))))
        (format nil "(X: ~D, Y: ~D) ~A~A~A~A~A~A"
                x y event-type
                (if button (format nil " ~A" button) "")
                (if direction (format nil " ~A" direction) "")
                (if shift " +shift" "")
                (if alt " +alt" "")
                (if ctrl " +ctrl" "")))
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

(eval-when (:load-toplevel :execute)
  (main))
