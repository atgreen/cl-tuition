;;; renderer.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Cell-based rendering engine (the "Cursed Renderer")
;;;;
;;;; Parses ANSI-styled view strings into cell buffers, diffs against the
;;;; previous buffer, and emits minimal escape sequences.  Wraps output in
;;;; synchronized output mode to prevent tearing.

(in-package #:tuition)

(defclass renderer ()
  ((last-buffer :initform nil :accessor last-buffer)
   (current-buffer :initform nil :accessor current-buffer)
   (last-view-state :initform nil :accessor last-view-state)
   (output-stream :initarg :output-stream
                  :initform *standard-output*
                  :accessor output-stream)
   (width :initform 80 :accessor renderer-width)
   (height :initform 24 :accessor renderer-height)
   (cursor-x :initform 0 :accessor renderer-cursor-x)
   (cursor-y :initform 0 :accessor renderer-cursor-y)
   (current-style :initform nil :accessor renderer-current-style))
  (:documentation "Cell-based rendering engine.  Diffs screen buffers and
emits minimal ANSI escape sequences."))

(defmethod render ((r renderer) view)
  "Render VIEW to the terminal using cell-based diffing.
VIEW may be a string or a view-state object."
  (let* ((content (if (typep view 'view-state)
                      (view-state-content view)
                      view))
         (stream (output-stream r))
         (w (renderer-width r))
         (h (renderer-height r)))
    ;; Handle view-state terminal transitions
    (when (typep view 'view-state)
      (apply-terminal-transitions r (last-view-state r) view stream)
      (setf (last-view-state r) view))
    ;; Parse content into cell buffer
    (let ((new-buf (parse-styled-string (or content "") w h)))
      ;; Synchronized output begin
      (write-syncd-begin stream)
      ;; Home cursor so render-diff's assumed (0,0) start is correct
      (format stream "~C[H" #\Escape)
      ;; Diff render against previous buffer
      (render-diff (last-buffer r) new-buf stream)
      ;; Synchronized output end
      (write-syncd-end stream)
      ;; Handle cursor positioning from view-state
      (when (typep view 'view-state)
        (let ((cursor (view-state-cursor view)))
          (if cursor
              (progn
                (format stream "~C[~D;~DH" #\Escape
                        (1+ (cursor-y cursor))
                        (1+ (cursor-x cursor)))
                (format stream "~C[?25h" #\Escape)) ; show cursor
              (format stream "~C[?25l" #\Escape))))  ; hide cursor
      (force-output stream)
      ;; Swap buffers
      (setf (last-buffer r) new-buf))))

(defun move-cursor-home (&optional (stream *standard-output*))
  "Move cursor to home position (1,1)."
  (format stream "~C[H" #\Escape))

(defun clear-to-end-of-screen (&optional (stream *standard-output*))
  "Clear from cursor to end of screen."
  (format stream "~C[J" #\Escape))

(defun move-cursor (row col &optional (stream *standard-output*))
  "Move cursor to specific position (1-indexed)."
  (format stream "~C[~D;~DH" #\Escape row col))
