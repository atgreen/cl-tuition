;;; components/progress.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Progress bar component - reusable progress indicator

(defpackage #:tuition.components.progress
  (:use #:cl)
  (:nicknames #:tui.progress)
  (:documentation "Progress bar component: textual progress visualization.")
  (:export
   ;; Model
   #:progress
   #:make-progress

   ;; Accessors
   #:progress-percent
   #:progress-width
   #:progress-show-percentage
   #:progress-full-char
   #:progress-empty-char
   #:progress-colors
   #:progress-empty-color

   ;; Operations
   #:progress-init
   #:progress-update
   #:progress-view
   #:progress-set-percent
   #:progress-increment))

(in-package #:tuition.components.progress)

;;; Progress bar model
(defclass progress ()
  ((percent :initarg :percent
            :initform 0.0
            :accessor progress-percent
            :documentation "Current progress (0.0 to 1.0)")
   (width :initarg :width
          :initform 40
          :accessor progress-width
          :documentation "Width of the progress bar in characters")
   (show-percentage :initarg :show-percentage
                    :initform t
                    :accessor progress-show-percentage
                    :documentation "Whether to show percentage")
   (full-char :initarg :full-char
              :initform #\█
              :accessor progress-full-char
              :documentation "Character for filled portion")
   (empty-char :initarg :empty-char
               :initform #\░
               :accessor progress-empty-char
               :documentation "Character for empty portion")
   (colors :initarg :colors
           :initform nil
           :accessor progress-colors
           :documentation "List of hex color stops (#RRGGBB). nil = plain;
0/1 entry = solid fill; 2+ entries = a gradient blend across the fill")
   (empty-color :initarg :empty-color
                :initform nil
                :accessor progress-empty-color
                :documentation "Hex color for the empty portion, or nil for plain"))
  (:documentation "A progress bar component."))

(defun make-progress (&key (percent 0.0) (width 40) (show-percentage t)
                          (full-char #\█) (empty-char #\░)
                          colors empty-color)
  "Create a new progress bar.

COLORS, when given, controls the fill color: a single hex string fills with a
solid color; two or more produce a gradient blend across the filled portion
(ported from bubbles progress #838).  EMPTY-COLOR similarly colors the empty
portion.  Both default to nil (plain, uncolored) for backward compatibility."
  (make-instance 'progress
                 :percent (max 0.0 (min 1.0 percent))
                 :width width
                 :show-percentage show-percentage
                 :full-char full-char
                 :empty-char empty-char
                 :colors colors
                 :empty-color empty-color))

;;; Component operations

(defun progress-init (progress-bar)
  "Initialize the progress bar. Returns nil (no command needed)."
  (declare (ignore progress-bar))
  nil)

(defun progress-update (progress-bar msg)
  "Update the progress bar. Currently no messages to handle.
   Returns (values new-progress cmd)."
  (declare (ignore msg))
  (values progress-bar nil))

(defun progress-view (progress-bar)
  "Render the progress bar."
  (let* ((percent (max 0.0 (min 1.0 (progress-percent progress-bar))))
         (width (progress-width progress-bar))
         (filled (max 0 (min width (round (* percent width)))))
         (empty (max 0 (- width filled)))
         (full-char (progress-full-char progress-bar))
         (empty-char (progress-empty-char progress-bar))
         (colors (progress-colors progress-bar))
         (empty-color (progress-empty-color progress-bar))
         (filled-str (progress-filled-run full-char filled colors))
         (empty-str (if (and empty-color (plusp empty))
                        (progress-solid-run empty-char empty empty-color)
                        (make-string empty :initial-element empty-char)))
         (bar (format nil "[~A~A]" filled-str empty-str)))
    (if (progress-show-percentage progress-bar)
        (format nil "~A ~3D%" bar (floor (* percent 100)))
        bar)))

;;; Color blending (ported from bubbles progress #838 / lipgloss Blend1D).

(defun %prog-hex-to-rgb (hex)
  "Parse a #RRGGBB or #RGB hex string to a list (R G B) of 0-255 integers."
  (let ((clean (string-trim '(#\#) hex)))
    (flet ((h (s) (parse-integer s :radix 16)))
      (cond
        ((= (length clean) 6)
         (list (h (subseq clean 0 2)) (h (subseq clean 2 4)) (h (subseq clean 4 6))))
        ((= (length clean) 3)
         (list (* 17 (h (subseq clean 0 1)))
               (* 17 (h (subseq clean 1 2)))
               (* 17 (h (subseq clean 2 3)))))
        (t (error "Invalid hex color: ~A" hex))))))

(defun %prog-lerp (a b factor)
  "Linearly interpolate between integers A and B by FACTOR in [0,1]."
  (round (+ a (* (- b a) factor))))

(defun %prog-blend-1d (steps stops)
  "Return a list of STEPS (R G B) triples interpolated evenly across STOPS.
STOPS is a list of (R G B) triples.  Mirrors lipgloss Blend1D's distribution."
  (let* ((stops (remove nil stops))
         (n (length stops)))
    (cond
      ((<= steps 0) nil)
      ((<= steps n) (subseq stops 0 steps))
      ((= n 0) nil)
      ((= n 1) (make-list steps :initial-element (first stops)))
      (t (let* ((num-seg (1- n))
                (default-size (floor steps num-seg))
                (remaining (mod steps num-seg))
                (result nil))
           (loop for i below num-seg
                 for seg-size = (+ default-size (if (< i remaining) 1 0))
                 do (let* ((from (nth i stops))
                           (to (nth (1+ i) stops))
                           (divisor (- seg-size 1)))
                      (loop for j below seg-size
                            for factor = (if (> seg-size 1)
                                             (/ (float j) (float divisor))
                                             0.0)
                            do (push (list (%prog-lerp (first from) (first to) factor)
                                           (%prog-lerp (second from) (second to) factor)
                                           (%prog-lerp (third from) (third to) factor))
                                     result))))
           (nreverse result))))))

(defun progress-solid-run (char count hex)
  "Render CHAR repeated COUNT times in a single solid color (HEX)."
  (let ((rgb (%prog-hex-to-rgb hex)))
    (format nil "~C[~Am~A~C[39m"
            #\Escape
            (tuition:color-rgb (first rgb) (second rgb) (third rgb))
            (make-string count :initial-element char)
            #\Escape)))

(defun progress-filled-run (char count colors)
  "Render the filled run of CHAR across COUNT cells, colored per COLORS.
COLORS is nil (plain), a single hex string (solid), or a list of hex strings
(gradient blend)."
  (cond
    ((or (null colors) (zerop count))
     (make-string count :initial-element char))
    ((and (stringp colors) (not (listp colors)))
     ;; Single hex string passed bare.
     (progress-solid-run char count colors))
    ((= (length colors) 1)
     (progress-solid-run char count (first colors)))
    (t
     (let* ((rgb-stops (mapcar #'%prog-hex-to-rgb colors))
            (blend (%prog-blend-1d count rgb-stops)))
       (with-output-to-string (s)
         (let (prev)
           (dolist (rgb blend)
             (unless (equal rgb prev)
               (format s "~C[~Am" #\Escape
                       (tuition:color-rgb (first rgb) (second rgb) (third rgb)))
               (setf prev rgb))
             (write-char char s))
           (when prev (format s "~C[39m" #\Escape))))))))

;;; Helper functions

(defun progress-set-percent (progress-bar percent)
  "Set the progress percentage (0.0 to 1.0)."
  (setf (progress-percent progress-bar)
        (max 0.0 (min 1.0 percent))))

(defun progress-increment (progress-bar amount)
  "Increment the progress by amount."
  (progress-set-percent progress-bar
                       (+ (progress-percent progress-bar) amount)))
