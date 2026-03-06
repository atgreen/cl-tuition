;;; mouse.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Mouse example - demonstrates mouse tracking

(asdf:load-system :tuition)

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
(defmethod tui:update-message ((model mouse-model) (msg tui:key-press-msg))
  (let ((key (tui:key-event-code msg)))
    (cond
      ((and (characterp key) (char= key #\q)) (values model (tui:quit-cmd)))
      ((and (tui:mod-contains (tui:key-event-mod msg) tui:+mod-ctrl+) (characterp key) (char= key #\c)) (values model (tui:quit-cmd)))
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
            (shift (tui:mod-contains (tui:mouse-event-mod msg) tui:+mod-shift+))
            (alt (tui:mod-contains (tui:mouse-event-mod msg) tui:+mod-alt+))
            (ctrl (tui:mod-contains (tui:mouse-event-mod msg) tui:+mod-ctrl+))
            (event-type (cond
                          ((tui:mouse-click-msg-p msg) "press")
                          ((tui:mouse-release-msg-p msg) "release")
                          ((tui:mouse-motion-msg-p msg) "motion")
                          ((tui:mouse-wheel-msg-p msg) "scroll")
                          (t "unknown")))
            (button (when (not (tui:mouse-wheel-msg-p msg))
                      (tui:mouse-event-button msg)))
            (direction (when (tui:mouse-wheel-msg-p msg)
                         (tui:mouse-wheel-direction msg))))
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
  (tui:make-view
   (format nil "~%Do mouse stuff. When you're done press q to quit.~%~%~
               Last event: ~A~%~%"
           (format-mouse-event (mouse-last-event model)))
   :mouse-mode :all-motion))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'mouse-model))))
    (tui:run program)))

(eval-when (:load-toplevel :execute)
  (main))
