;;; spinner.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Spinner example - demonstrates tick-based animation

(defpackage #:tuition-example-spinner
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-spinner)

;;; Spinner frames - different animation styles
(defparameter *spinner-dot* #("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
(defparameter *spinner-line* #("|" "/" "-" "\\"))
(defparameter *spinner-dots* #("⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷"))

;;; Tick message
(tui:defmessage tick-msg ())

;;; Model
(defclass spinner-model ()
  ((frame :initform 0 :accessor spinner-frame)
   (frames :initform *spinner-dot* :accessor spinner-frames)
   (quitting :initform nil :accessor spinner-quitting)))

;;; Init - start the ticker
(defmethod tui:init ((model spinner-model))
  (lambda ()
    (sleep 0.1)
    (make-instance 'tick-msg)))

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model spinner-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((and (characterp key) (char= key #\q))
       (setf (spinner-quitting model) t)
       (values model (tui:quit-cmd)))
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c))
       (setf (spinner-quitting model) t)
       (values model (tui:quit-cmd)))
      (t (values model nil)))))

(defmethod tui:update-message ((model spinner-model) (msg tick-msg))
  (setf (spinner-frame model)
        (mod (1+ (spinner-frame model))
             (length (spinner-frames model))))
  ;; Return a command to tick again
  (values model
          (lambda ()
            (sleep 0.1)
            (make-instance 'tick-msg))))

;;; View
(defmethod tui:view ((model spinner-model))
  (let ((frame (aref (spinner-frames model) (spinner-frame model))))
    (format nil "~%~%   ~A Loading forever...press q to quit~%~%~A"
            frame
            (if (spinner-quitting model) "~%" ""))))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'spinner-model))))
    (tui:run program)))

#+nil
(main)
