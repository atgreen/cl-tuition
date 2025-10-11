;;; layout.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Layout utilities - joining and positioning text blocks

(in-package #:tuition)

;;; Position constants
(defconstant +top+ :top)
(defconstant +middle+ :middle)
(defconstant +bottom+ :bottom)
(defconstant +left+ :left)
(defconstant +center+ :center)
(defconstant +right+ :right)

(defun join-horizontal (position &rest blocks)
  "Join text blocks horizontally at the given vertical position.
   Position can be :top, :middle, :bottom, or a number 0.0-1.0."
  (let* ((block-lines (mapcar #'split-string-by-newline blocks))
         (block-widths (mapcar (lambda (ls)
                                 (if ls (apply #'max (mapcar #'visible-length ls)) 0))
                               block-lines))
         (max-height (apply #'max (mapcar #'length block-lines)))
         (result '()))

    (dotimes (i max-height)
      (let ((line ""))
        (loop for lines in block-lines
              for bw in block-widths do
              (let* ((height (length lines))
                     (offset (calculate-offset position i max-height height))
                     (idx (- i offset)))
                (if (and (>= idx 0) (< idx height))
                    ;; Pad the line to block width before concatenating
                    (let* ((current-line (nth idx lines))
                           (line-width (visible-length current-line))
                           (padding (make-string (max 0 (- bw line-width)) :initial-element #\Space)))
                      (setf line (concatenate 'string line current-line padding)))
                    (setf line (concatenate 'string line
                                            (make-string bw :initial-element #\Space))))))
        (push line result)))

    (format nil "~{~A~^~%~}" (nreverse result))))

(defun join-vertical (position &rest blocks)
  "Join text blocks vertically at the given horizontal position.
   Position can be :left, :center, :right, or a number 0.0-1.0."
  (let* ((block-lines (mapcar #'split-string-by-newline blocks))
         (max-width (apply #'max (mapcar (lambda (lines)
                                          (apply #'max (mapcar #'visible-length lines)))
                                        block-lines)))
         (result '()))

    (dolist (lines block-lines)
      (dolist (line lines)
        (let* ((width (visible-length line))
               (padding (- max-width width))
               (aligned (align-text line max-width position)))
          (push aligned result))))

    (format nil "~{~A~^~%~}" (nreverse result))))

(defun place-horizontal (width position text &key whitespace-char whitespace-fg)
  "Place text horizontally in a space of given width.
   Position can be :left, :center, :right, or a number 0.0-1.0.
   Optional WHITESPACE-CHAR and WHITESPACE-FG control whitespace styling."
  (let ((lines (split-string-by-newline text)))
    (format nil "~{~A~^~%~}"
            (mapcar (lambda (line)
                     (align-text line width position
                                :whitespace-char whitespace-char
                                :whitespace-fg whitespace-fg))
                   lines))))

(defun place-vertical (height position text)
  "Place text vertically in a space of given height.
   Position can be :top, :middle, :bottom, or a number 0.0-1.0."
  (let* ((lines (split-string-by-newline text))
         (text-height (length lines))
         (padding (- height text-height))
         (offset (calculate-offset position 0 height text-height))
         (top-padding (make-list offset :initial-element ""))
         (bottom-padding (make-list (- padding offset) :initial-element "")))
    (format nil "~{~A~^~%~}"
            (append top-padding lines bottom-padding))))

(defun place (width height h-pos v-pos text &key whitespace-char whitespace-fg)
  "Place text in a width x height space at the given positions.
   Optional WHITESPACE-CHAR and WHITESPACE-FG control whitespace styling."
  (place-horizontal width h-pos
                   (place-vertical height v-pos text)
                   :whitespace-char whitespace-char
                   :whitespace-fg whitespace-fg))

;;; Helper functions

(defun calculate-offset (position current-idx max-size content-size)
  "Calculate offset for positioning."
  (declare (ignore current-idx))
  (let ((space (- max-size content-size)))
    (cond
      ((eq position :top) 0)
      ((eq position :middle) (floor space 2))
      ((eq position :bottom) space)
      ((eq position :left) 0)
      ((eq position :center) (floor space 2))
      ((eq position :right) space)
      ((numberp position) (floor (* space position)))
      (t 0))))

(defun align-text (text width position &key whitespace-char whitespace-fg)
  "Align text within the given width.
   Optional WHITESPACE-CHAR specifies the character to use for padding (default #\\Space).
   Optional WHITESPACE-FG specifies the foreground color for whitespace."
  (let* ((visible-len (visible-length text))
         (padding-cols (max 0 (- width visible-len)))
         (ws-char (or whitespace-char #\Space))
         (ws-width (%char-display-width ws-char))
         ;; Calculate number of characters needed to fill padding-cols columns
         (padding-chars (if (> ws-width 0) (floor padding-cols ws-width) 0))
         (pad-str (if whitespace-fg
                      (colored (make-string padding-chars :initial-element ws-char) :fg whitespace-fg)
                      (make-string padding-chars :initial-element ws-char))))
    (cond
      ((or (eq position :left) (eq position :top))
       (format nil "~A~A" text pad-str))

      ((or (eq position :right) (eq position :bottom))
       (format nil "~A~A" pad-str text))

      ((or (eq position :center) (eq position :middle))
       (let* ((left-pad-cols (floor padding-cols 2))
              (right-pad-cols (- padding-cols left-pad-cols))
              (left-pad-chars (if (> ws-width 0) (floor left-pad-cols ws-width) 0))
              (right-pad-chars (if (> ws-width 0) (floor right-pad-cols ws-width) 0))
              (left-str (if whitespace-fg
                            (colored (make-string left-pad-chars :initial-element ws-char) :fg whitespace-fg)
                            (make-string left-pad-chars :initial-element ws-char)))
              (right-str (if whitespace-fg
                             (colored (make-string right-pad-chars :initial-element ws-char) :fg whitespace-fg)
                             (make-string right-pad-chars :initial-element ws-char))))
         (format nil "~A~A~A" left-str text right-str)))

      ((numberp position)
       (let* ((left-pad-cols (floor (* padding-cols position)))
              (right-pad-cols (- padding-cols left-pad-cols))
              (left-pad-chars (if (> ws-width 0) (floor left-pad-cols ws-width) 0))
              (right-pad-chars (if (> ws-width 0) (floor right-pad-cols ws-width) 0))
              (left-str (if whitespace-fg
                            (colored (make-string left-pad-chars :initial-element ws-char) :fg whitespace-fg)
                            (make-string left-pad-chars :initial-element ws-char)))
              (right-str (if whitespace-fg
                             (colored (make-string right-pad-chars :initial-element ws-char) :fg whitespace-fg)
                             (make-string right-pad-chars :initial-element ws-char))))
         (format nil "~A~A~A" left-str text right-str)))

      (t text))))

(defun block-width (text)
  "Get the maximum visible width of a text block."
  (cond
    ((stringp text)
     (let ((lines (split-string-by-newline text)))
       (if lines (apply #'max (mapcar #'visible-length lines)) 0)))
    (t 0)))

(defun block-height (text)
  "Get the height of a text block (number of lines)."
  (length (split-string-by-newline text)))
