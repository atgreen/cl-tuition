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
    ;; Move home, overwrite content, then clear any leftover lines.
    ;; This avoids the visible flash caused by ESC[2J (full clear).
    (let ((stream (output-stream r)))
      (move-cursor-home stream)
      (write-string view-string stream)
      (clear-to-end-of-screen stream)
      (force-output stream))))

(defun move-cursor-home (&optional (stream *standard-output*))
  "Move cursor to home position (1,1)."
  (format stream "~C[H" #\Escape))

(defun clear-to-end-of-screen (&optional (stream *standard-output*))
  "Clear from cursor to end of screen."
  (format stream "~C[J" #\Escape))

(defun move-cursor (row col &optional (stream *standard-output*))
  "Move cursor to specific position (1-indexed)."
  (format stream "~C[~D;~DH" #\Escape row col))
