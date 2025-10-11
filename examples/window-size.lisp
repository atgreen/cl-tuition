;;; window-size.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Window size example - demonstrates terminal size detection

(defpackage #:tuition-example-window-size
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-window-size)

(eval-when (:load-toplevel :execute)
  (asdf:load-system :tuition))

;;; Model
(defclass window-size-model ()
  ((size :initform nil :accessor window-size)))

;;; Init
(defmethod tui:init ((model window-size-model))
  nil)

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model window-size-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Quit on q, ctrl+c, or escape
      ((and (characterp key) (char= key #\q))
       (values model (tui:quit-cmd)))
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c))
       (values model (tui:quit-cmd)))
      ((eq key :escape)
       (values model (tui:quit-cmd)))
      ;; Any other key - query window size
      (t
       (let ((size (get-terminal-size)))
         (setf (window-size model)
               (format nil "~Dx~D" (car size) (cdr size)))
         (values model nil))))))

;;; View
(defmethod tui:view ((model window-size-model))
  (format nil "~%When you're done press q to quit. Press any other key to query the window-size.~%~%~
               ~@[Current size: ~A~%~%~]"
          (window-size model)))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'window-size-model))))
    (tui:run program)))

(eval-when (:load-toplevel :execute)
  (main))

#+nil
(main)
