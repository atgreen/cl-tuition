;;;; SPDX-License-Identifier: MIT
;;;; Layout utilities - joining and positioning text blocks

(in-package #:tuition)

;;; Position constants
(defparameter +top+ :top)
(defparameter +middle+ :middle)
(defparameter +bottom+ :bottom)
(defparameter +left+ :left)
(defparameter +center+ :center)
(defparameter +right+ :right)

(defun join-horizontal (position &rest blocks)
  "Join text blocks horizontally at the given vertical position.
   Position can be :top, :middle, :bottom, or a number 0.0-1.0."
  (let* ((block-lines (mapcar #'split-string-by-newline blocks))
         (max-height (apply #'max (mapcar #'length block-lines)))
         (result '()))

    (dotimes (i max-height)
      (let ((line ""))
        (dolist (lines block-lines)
          (let* ((height (length lines))
                 (offset (calculate-offset position i max-height height))
                 (idx (- i offset)))
            (if (and (>= idx 0) (< idx height))
                (setf line (concatenate 'string line (nth idx lines)))
                (setf line (concatenate 'string line
                                       (make-string (block-width (first lines))
                                                   :initial-element #\Space))))))
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

(defun place-horizontal (width position text)
  "Place text horizontally in a space of given width.
   Position can be :left, :center, :right, or a number 0.0-1.0."
  (let ((lines (split-string-by-newline text)))
    (format nil "~{~A~^~%~}"
            (mapcar (lambda (line)
                     (align-text line width position))
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

(defun place (width height h-pos v-pos text)
  "Place text in a width x height space at the given positions."
  (place-horizontal width h-pos
                   (place-vertical height v-pos text)))

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

(defun align-text (text width position)
  "Align text within the given width."
  (let* ((visible-len (visible-length text))
         (padding (max 0 (- width visible-len))))
    (cond
      ((or (eq position :left) (eq position :top))
       (format nil "~A~A" text (make-string padding :initial-element #\Space)))

      ((or (eq position :right) (eq position :bottom))
       (format nil "~A~A" (make-string padding :initial-element #\Space) text))

      ((or (eq position :center) (eq position :middle))
       (let* ((left-pad (floor padding 2))
              (right-pad (- padding left-pad)))
         (format nil "~A~A~A"
                (make-string left-pad :initial-element #\Space)
                text
                (make-string right-pad :initial-element #\Space))))

      ((numberp position)
       (let* ((left-pad (floor (* padding position)))
              (right-pad (- padding left-pad)))
         (format nil "~A~A~A"
                (make-string left-pad :initial-element #\Space)
                text
                (make-string right-pad :initial-element #\Space))))

      (t text))))

(defun block-width (text)
  "Get the width of a text block (first line)."
  (if (stringp text)
      (visible-length text)
      0))

(defun block-height (text)
  "Get the height of a text block (number of lines)."
  (length (split-string-by-newline text)))
