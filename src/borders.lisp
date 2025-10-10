;;;; SPDX-License-Identifier: MIT
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
                             fg-color bg-color)
  "Render text with a border around it."
  (let* ((lines (split-string-by-newline text))
         (max-width (apply #'max (mapcar #'visible-length lines)))
         (result nil))

    ;; Top border
    (when top
      (push (format nil "~A~A~A"
                   (if left (border-top-left border) "")
                   (make-string max-width :initial-element
                               (char (border-top border) 0))
                   (if right (border-top-right border) ""))
            result))

    ;; Content with left/right borders
    (dolist (line lines)
      (let* ((visible-len (visible-length line))
             (padding (- max-width visible-len)))
        (push (format nil "~A~A~A~A"
                     (if left (border-left border) "")
                     line
                     (make-string padding :initial-element #\Space)
                     (if right (border-right border) ""))
              result)))

    ;; Bottom border
    (when bottom
      (push (format nil "~A~A~A"
                   (if left (border-bottom-left border) "")
                   (make-string max-width :initial-element
                               (char (border-bottom border) 0))
                   (if right (border-bottom-right border) ""))
            result))

    ;; Apply colors if specified
    (let ((output (format nil "~{~A~^~%~}" (nreverse result))))
      (if (or fg-color bg-color)
          (colored output :fg fg-color :bg bg-color)
          output))))

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
