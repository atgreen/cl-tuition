;;; borders.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Border definitions and rendering

(in-package #:tuition)

;;; Border structure
(defclass border ()
  ((top :initarg :top :accessor border-top :initform "─")
   (bottom :initarg :bottom :accessor border-bottom :initform "─")
   (left :initarg :left :accessor border-left :initform "│")
   (right :initarg :right :accessor border-right :initform "│")
   (top-left :initarg :top-left :accessor border-top-left :initform "┌")
   (top-right :initarg :top-right :accessor border-top-right :initform "┐")
   (bottom-left :initarg :bottom-left :accessor border-bottom-left :initform "└")
   (bottom-right :initarg :bottom-right :accessor border-bottom-right :initform "┘")
   (middle-left :initarg :middle-left :accessor border-middle-left :initform "├")
   (middle-right :initarg :middle-right :accessor border-middle-right :initform "┤")
   (middle :initarg :middle :accessor border-middle :initform "┼")
   (middle-top :initarg :middle-top :accessor border-middle-top :initform "┬")
   (middle-bottom :initarg :middle-bottom :accessor border-middle-bottom :initform "┴"))
  (:documentation "Border character definitions."))

;;; Predefined border styles

(defparameter *border-normal*
  (make-instance 'border
                 :top "─" :bottom "─" :left "│" :right "│"
                 :top-left "┌" :top-right "┐"
                 :bottom-left "└" :bottom-right "┘"
                 :middle-left "├" :middle-right "┤"
                 :middle "┼" :middle-top "┬" :middle-bottom "┴")
  "Standard border with 90 degree corners.")

(defparameter *border-rounded*
  (make-instance 'border
                 :top "─" :bottom "─" :left "│" :right "│"
                 :top-left "╭" :top-right "╮"
                 :bottom-left "╰" :bottom-right "╯"
                 :middle-left "├" :middle-right "┤"
                 :middle "┼" :middle-top "┬" :middle-bottom "┴")
  "Border with rounded corners.")

(defparameter *border-thick*
  (make-instance 'border
                 :top "━" :bottom "━" :left "┃" :right "┃"
                 :top-left "┏" :top-right "┓"
                 :bottom-left "┗" :bottom-right "┛"
                 :middle-left "┣" :middle-right "┫"
                 :middle "╋" :middle-top "┳" :middle-bottom "┻")
  "Thick border.")

(defparameter *border-double*
  (make-instance 'border
                 :top "═" :bottom "═" :left "║" :right "║"
                 :top-left "╔" :top-right "╗"
                 :bottom-left "╚" :bottom-right "╝"
                 :middle-left "╠" :middle-right "╣"
                 :middle "╬" :middle-top "╦" :middle-bottom "╩")
  "Double-line border.")

(defparameter *border-block*
  (make-instance 'border
                 :top "█" :bottom "█" :left "█" :right "█"
                 :top-left "█" :top-right "█"
                 :bottom-left "█" :bottom-right "█"
                 :middle-left "█" :middle-right "█"
                 :middle "█" :middle-top "█" :middle-bottom "█")
  "Solid block border.")

(defparameter *border-hidden*
  (make-instance 'border
                 :top " " :bottom " " :left " " :right " "
                 :top-left " " :top-right " "
                 :bottom-left " " :bottom-right " "
                 :middle-left " " :middle-right " "
                 :middle " " :middle-top " " :middle-bottom " ")
  "Hidden border (spaces).")

(defparameter *border-ascii*
  (make-instance 'border
                 :top "-" :bottom "-" :left "|" :right "|"
                 :top-left "+" :top-right "+"
                 :bottom-left "+" :bottom-right "+"
                 :middle-left "+" :middle-right "+"
                 :middle "+" :middle-top "+" :middle-bottom "+")
  "ASCII-only border.")

(defparameter *border-markdown*
  (make-instance 'border
                 :top "-" :bottom "-" :left "|" :right "|"
                 :top-left "|" :top-right "|"
                 :bottom-left "|" :bottom-right "|"
                 :middle-left "|" :middle-right "|"
                 :middle "|" :middle-top "|" :middle-bottom "|")
  "Markdown table border.")

;;; Border rendering

(defun render-border (text border &key
                             (top t) (bottom t) (left t) (right t)
                             fg-color bg-color
                             title (title-position :center))
  "Render text with a border around it.
   Colors are applied per-border-character to avoid being cleared by content resets.
   TITLE, if provided, is placed on the top border line.
   TITLE-POSITION is :LEFT, :CENTER, or :RIGHT (default :CENTER)."
  (let* ((lines (split-string-by-newline text))
         (max-width (apply #'max (mapcar #'visible-length lines)))
         (result nil)
         ;; Helper to color a border character if color is specified
         (color-border (lambda (str)
                        (if (or fg-color bg-color)
                            (colored str :fg fg-color :bg bg-color)
                            str))))

    ;; Top border
    (when top
      (let ((top-line
              (if title
                  ;; Build top border with title embedded
                  (let* ((title-vis-len (visible-length title))
                         (available (- max-width 2)) ; at least 1 border char each side
                         (truncated-title (if (> title-vis-len available)
                                              (serapeum:take (min (length title) available) title)
                                              title))
                         (t-len (visible-length truncated-title))
                         (border-char (char (border-top border) 0))
                         (left-pad (case title-position
                                     (:left 1)
                                     (:right (max 1 (- max-width t-len 1)))
                                     (otherwise (max 1 (floor (- max-width t-len) 2)))))
                         (right-pad (max 1 (- max-width t-len left-pad))))
                    (format nil "~A~A~A~A~A"
                            (if left (border-top-left border) "")
                            (make-string left-pad :initial-element border-char)
                            truncated-title
                            (make-string right-pad :initial-element border-char)
                            (if right (border-top-right border) "")))
                  ;; Normal top border without title
                  (format nil "~A~A~A"
                          (if left (border-top-left border) "")
                          (make-string max-width :initial-element
                                       (char (border-top border) 0))
                          (if right (border-top-right border) "")))))
        (push (funcall color-border top-line) result)))

    ;; Content with left/right borders
    (dolist (line lines)
      (let* ((visible-len (visible-length line))
             (padding (- max-width visible-len))
             ;; Color the border characters and padding separately from content
             (left-part (if left (funcall color-border (border-left border)) ""))
             (pad-part (funcall color-border (make-string padding :initial-element #\Space)))
             (right-part (if right (funcall color-border (border-right border)) "")))
        (push (format nil "~A~A~A~A"
                     left-part
                     line
                     pad-part
                     right-part)
              result)))

    ;; Bottom border
    (when bottom
      (let ((bottom-line (format nil "~A~A~A"
                                (if left (border-bottom-left border) "")
                                (make-string max-width :initial-element
                                            (char (border-bottom border) 0))
                                (if right (border-bottom-right border) ""))))
        (push (funcall color-border bottom-line) result)))

    ;; Return the formatted result
    (format nil "~{~A~^~%~}" (nreverse result))))

(defun make-border (&key top bottom left right
                        top-left top-right
                        bottom-left bottom-right
                        middle-left middle-right
                        middle middle-top middle-bottom)
  "Create a custom border."
  (make-instance 'border
                 :top (or top "─")
                 :bottom (or bottom "─")
                 :left (or left "│")
                 :right (or right "│")
                 :top-left (or top-left "┌")
                 :top-right (or top-right "┐")
                 :bottom-left (or bottom-left "└")
                 :bottom-right (or bottom-right "┘")
                 :middle-left (or middle-left "├")
                 :middle-right (or middle-right "┤")
                 :middle (or middle "┼")
                 :middle-top (or middle-top "┬")
                 :middle-bottom (or middle-bottom "┴")))

;;; Shadow rendering

(defun shadow-chars (style)
  "Return (right-char . bottom-char) for the given shadow style keyword."
  (case style
    (:solid  (cons "█" "█"))
    (:light  (cons "░" "░"))
    (:medium (cons "▒" "▒"))
    (:heavy  (cons "▓" "▓"))
    (otherwise (cons "█" "▀")))) ; :dark default

(defun render-shadow (text &key (width 2) (offset 1)
                                (color *fg-bright-black*) (style :dark))
  "Add a drop shadow (right + bottom) to a text block.
   WIDTH is the shadow thickness in characters (default 2).
   OFFSET is the rows/cols skipped from the top-left before the shadow starts (default 1).
   COLOR is the shadow color (default bright-black/dark gray).
   STYLE is a shadow character preset: :DARK, :SOLID, :LIGHT, :MEDIUM, or :HEAVY."
  (let* ((chars (shadow-chars style))
         (right-char (first chars))
         (bottom-char (rest chars))
         (lines (split-string-by-newline text))
         (num-lines (length lines))
         (max-width (if lines
                        (apply #'max (mapcar #'visible-length lines))
                        0))
         (shadow-str (colored (make-string width :initial-element
                                           (char right-char 0))
                              :fg color))
         (pad-str (make-string width :initial-element #\Space))
         (result nil))
    ;; Process each content line, appending right shadow where applicable
    (loop for line in lines
          for i from 0
          do (if (>= i offset)
                 ;; This line gets a shadow on the right
                 (push (format nil "~A~A" line shadow-str) result)
                 ;; Lines before offset get padding to maintain alignment
                 (push (format nil "~A~A" line pad-str) result)))
    ;; Bottom shadow row
    (let* ((bottom-width (+ max-width width (- offset)))
           (bottom-str (colored (make-string (max 0 bottom-width)
                                             :initial-element
                                             (char bottom-char 0))
                                :fg color))
           (leading-spaces (make-string offset :initial-element #\Space)))
      (push (format nil "~A~A" leading-spaces bottom-str) result))
    (format nil "~{~A~^~%~}" (nreverse result))))
