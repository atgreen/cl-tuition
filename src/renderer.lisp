;;; renderer.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Rendering engine for efficient terminal output

(in-package #:tuition)

(defclass renderer ()
  ((last-output :initform "" :accessor last-output)
   (output-stream :initarg :output-stream
                  :initform *standard-output*
                  :accessor output-stream))
  (:documentation "Manages rendering to the terminal."))

(defmethod render ((r renderer) view-string)
  "Render the view string to the terminal.
   Only redraws if the output has changed."
  (unless (string= view-string (last-output r))
    (setf (last-output r) view-string)
    ;; Clear screen and move to home before rendering
    (clear-screen)
    (move-cursor-home)
    (write-string view-string (output-stream r))
    (force-output (output-stream r))))

(defun move-cursor-home ()
  "Move cursor to home position (1,1)."
  (format t "~C[H" #\Escape))

(defun clear-to-end-of-screen ()
  "Clear from cursor to end of screen."
  (format t "~C[J" #\Escape))

(defun move-cursor (row col)
  "Move cursor to specific position (1-indexed)."
  (format t "~C[~D;~DH" #\Escape row col))
