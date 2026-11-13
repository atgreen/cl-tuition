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
   (origin-y :initform 0 :accessor renderer-origin-y
             :documentation "Screen row (0-based) the managed view is drawn
from.  Normally 0 (view at screen top).  PRINT-ABOVE-VIEW advances it so
permanent lines printed above the view are not overwritten by the next frame.")
   (current-style :initform nil :accessor renderer-current-style))
  (:documentation "Cell-based rendering engine.  Diffs screen buffers and
emits minimal ANSI escape sequences."))

(defmethod render ((r renderer) view)
  "Render VIEW to the terminal using cell-based diffing.
VIEW may be a string or a view-state object.

On the alternate screen the frame fills the whole terminal.  For an
inline (non-alt-screen) view the frame is only as tall as its content,
exactly as bubbletea's renderer sizes its cell buffer: the rows below
the frame are left untouched, so permanent lines emitted by INSERT-ABOVE
flow up into the terminal's scrollback instead of being painted over."
  (let* ((content (if (typep view 'view-state)
                      (view-state-content view)
                      view))
         (stream (output-stream r))
         (w (renderer-width r))
         (h (renderer-height r))
         (alt-screen (and (typep view 'view-state) (view-state-alt-screen view))))
    ;; Handle view-state terminal transitions
    (when (typep view 'view-state)
      (apply-terminal-transitions r (last-view-state r) view stream)
      (setf (last-view-state r) view))
    ;; Alt-screen frames fill the terminal; inline frames are only as tall as
    ;; their content (clamped to the terminal height).
    (let* ((view-h (if alt-screen
                       h
                       (max 1 (min h (length (split-string-by-newline
                                              (or content "")))))))
           (new-buf (parse-styled-string (or content "") w view-h))
           (origin (renderer-origin-y r))
           (prev (last-buffer r))
           (prev-h (if prev (screen-buffer-height prev) view-h)))
      ;; Synchronized output begin
      (write-syncd-begin stream)
      ;; If the inline frame shrank, clear the rows it no longer occupies so a
      ;; shorter view leaves no stale cells behind.
      (when (> prev-h view-h)
        (loop for y from view-h below prev-h
              do (format stream "~C[~D;1H~C[K" #\Escape (+ 1 y origin) #\Escape)))
      ;; A height change invalidates the diff baseline; repaint the frame fully.
      (when (/= prev-h view-h)
        (setf prev nil))
      ;; Home cursor so render-diff's assumed (0,origin) start is correct.
      ;; When ORIGIN is 0 this is exactly ESC[H (unchanged behavior).
      (if (zerop origin)
          (format stream "~C[H" #\Escape)
          (format stream "~C[~D;1H" #\Escape (1+ origin)))
      ;; Diff render against previous buffer
      (render-diff prev new-buf stream origin)
      ;; Synchronized output end
      (write-syncd-end stream)
      ;; Handle cursor positioning from view-state
      (when (typep view 'view-state)
        (let ((cursor (view-state-cursor view)))
          (if cursor
              (progn
                (format stream "~C[~D;~DH" #\Escape
                        (+ 1 (cursor-y cursor) origin)
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

(defmethod insert-above ((r renderer) content)
  "Print CONTENT as permanent, unmanaged line(s) above the managed view.
CONTENT is a string (newlines split it into lines) or a list of lines.  The
lines are written above the view and the view's draw origin is advanced so the
next frame repaints below them; as the view is pushed toward the bottom of the
screen the oldest lines scroll into the terminal's scrollback.  No-op when the
alternate screen is active (there is no scrollback to print into)."
  (let ((vs (last-view-state r)))
    (when (and vs (view-state-alt-screen vs))
      (return-from insert-above nil)))
  (let* ((stream (output-stream r))
         (lines (if (listp content) content (split-string-by-newline content)))
         (n (length lines))
         (frame-h (if (last-buffer r) (screen-buffer-height (last-buffer r)) 0))
         (screen-h (renderer-height r))
         (origin (renderer-origin-y r)))
    (when (plusp n)
      (write-syncd-begin stream)
      ;; Write the message lines at the current top of the frame, each clearing
      ;; to end of line.  This overwrites the frame's top rows (the frame is
      ;; fully repainted afterward) and advances the cursor down one row per line.
      (format stream "~C[~D;1H" #\Escape (1+ origin))
      (dolist (line lines)
        (format stream "~A~C[K~C~C" line #\Escape #\Return #\Linefeed))
      ;; The message now occupies rows [origin+1, origin+n]; draw the frame below.
      (let ((new-origin (+ origin n)))
        ;; If the frame no longer fits, scroll the screen up so the oldest rows
        ;; move into scrollback, and pin the frame to the bottom.
        (when (> (+ new-origin frame-h) screen-h)
          (let ((overflow (- (+ new-origin frame-h) screen-h)))
            (format stream "~C[~D;1H" #\Escape screen-h)
            (dotimes (i overflow) (write-char #\Linefeed stream))
            (decf new-origin overflow)))
        (setf (renderer-origin-y r) (max 0 new-origin)))
      ;; Force a full repaint of the view at its new origin.
      (setf (last-buffer r) nil)
      (write-syncd-end stream)
      (force-output stream))
    n))
