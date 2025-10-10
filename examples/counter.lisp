;;; counter.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Interactive counter example - demonstrates key handling

(defpackage #:tuition-example-counter
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-counter)

;;; Model - counter state
(defclass counter-model ()
  ((counter :initarg :counter :accessor counter :initform 0)))

;;; Init - no initial command needed
(defmethod tui:init ((model counter-model))
  nil)

;;; Update (CLOS style) - handle key presses via update-message
(defmethod tui:update-message ((model counter-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Quit on q
      ((and (characterp key) (char= key #\q))
       (values model (tui:quit-cmd)))

      ;; Quit on ctrl+c
      ((and (tui:key-msg-ctrl msg)
            (characterp key)
            (char= key #\c))
       (values model (tui:quit-cmd)))

      ;; Increment on up arrow or +
      ((or (eq key :up)
           (and (characterp key) (char= key #\+)))
       (incf (counter model))
       (values model nil))

      ;; Decrement on down arrow or -
      ((or (eq key :down)
           (and (characterp key) (char= key #\-)))
       (decf (counter model))
       (values model nil))

      ;; Reset on r
      ((and (characterp key) (char= key #\r))
       (setf (counter model) 0)
       (values model nil))

      (t (values model nil)))))

;;; View - render the counter UI
(defmethod tui:view ((model counter-model))
  (format nil "~%  Counter: ~D~%~%~
               Controls:~%~
               ↑/+  Increment~%~
               ↓/-  Decrement~%~
               r    Reset~%~
               q    Quit~%~%"
          (counter model)))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'counter-model))))
    (tui:run program)))

#+nil
(main)
