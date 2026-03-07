;;; list.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; List rendering utilities for formatted text output

(defpackage #:tuition.render.list
  (:use #:cl)
  (:nicknames #:tui.render.list #:tui.r.list)
  (:documentation "List rendering utilities with various enumeration styles.")
  (:export
   ;; List creation
   #:new-list
   #:list-item

   ;; Enumerators
   #:bullet-enumerator
   #:arabic-enumerator
   #:alphabet-enumerator
   #:roman-enumerator
   #:tree-enumerator

   ;; Rendering
   #:list-render))

(in-package #:tuition.render.list)

;;; List structure
(defclass tui-list ()
  ((items :initform nil :accessor list-items)
   (enumerator :initform #'bullet-enumerator :accessor list-enumerator)
   (enumerator-style :initform nil :accessor list-enumerator-style)
   (item-style :initform nil :accessor list-item-style)
   (indent :initform 0 :accessor list-indent))
  (:documentation "A renderable list."))

(defun new-list (&rest items)
  "Create a new list with the given items."
  (let ((lst (make-instance 'tui-list)))
    (setf (list-items lst) items)
    lst))

(defun list-item (list item)
  "Add an item to the list."
  (alexandria:appendf (list-items list) (list item))
  list)

;;; Built-in enumerators
(defun bullet-enumerator (items index)
  "Bullet point enumerator."
  (declare (ignore items index))
  "• ")

(defun arabic-enumerator (items index)
  "Arabic numeral enumerator (1. 2. 3. ...)."
  (declare (ignore items))
  (format nil "~D. " (1+ index)))

(defun alphabet-enumerator (items index)
  "Alphabetic enumerator (A. B. C. ...)."
  (declare (ignore items))
  (let ((letter (code-char (+ (char-code #\A) index))))
    (format nil "~A. " letter)))

(defun roman-enumerator (items index)
  "Roman numeral enumerator (i. ii. iii. ...)."
  (declare (ignore items))
  (let ((numerals '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
                    (100 . "C") (90 . "XC") (50 . "L") (40 . "XL")
                    (10 . "X") (9 . "IX") (5 . "V") (4 . "IV") (1 . "I")))
        (num (1+ index))
        (result ""))
    (dolist (pair numerals)
      (loop while (>= num (first pair))
            do (setf result (concatenate 'string result (rest pair)))
               (decf num (first pair))))
    (format nil "~A. " result)))

(defun tree-enumerator (items index)
  "Tree-style enumerator with branches."
  (if (= index (1- (length items))) "└── " "├── "))

;;; Rendering
(defun list-render (list)
  "Render a list to a string."
  (let* ((items (list-items list))
         (enum-func (list-enumerator list))
         (enum-style (list-enumerator-style list))
         (item-style (list-item-style list))
         (indent (list-indent list))
         ;; Pre-compute all enumerator strings and find max width for alignment
         (enum-texts (loop for item in items
                           for idx from 0
                           collect (funcall enum-func items idx)))
         (max-enum-width (if enum-texts
                             (apply #'max (mapcar #'tuition:visible-length enum-texts))
                             0))
         (result nil))

    (loop for item in items
          for idx from 0
          for enum-text in enum-texts
          do (let* (;; Right-align enumerator to max width
                    (enum-width (tuition:visible-length enum-text))
                    (enum-padding (- max-enum-width enum-width))
                    (padded-enum (if (> enum-padding 0)
                                     (concatenate 'string
                                                  (make-string enum-padding :initial-element #\Space)
                                                  enum-text)
                                     enum-text))
                    (styled-enum (if enum-style
                                     (tuition:render-styled enum-style padded-enum)
                                     padded-enum))
                    (nested-p (typep item 'tui-list))
                    (indent-str (make-string indent :initial-element #\Space)))
               (cond
                 ;; Nested list: render and indent, no parent enumerator
                 (nested-p
                  (let ((nested (list-render item)))
                    (push (format nil "~A~A"
                                 indent-str
                                 (tuition:indent-lines nested max-enum-width))
                          result)))
                 ;; String item (possibly multiline)
                 (t
                  (let* ((text (if item-style
                                   (tuition:render-styled item-style (format nil "~A" item))
                                   (format nil "~A" item)))
                         (lines (tuition:split-string-by-newline text))
                         (continuation-indent (make-string max-enum-width :initial-element #\Space))
                         ;; Pad all lines to the max line width within this item
                         (max-line-width (apply #'max (mapcar #'tuition:visible-length lines)))
                         (padded-lines (mapcar (lambda (line)
                                                 (let ((pad (- max-line-width (tuition:visible-length line))))
                                                   (if (> pad 0)
                                                       (concatenate 'string line (make-string pad :initial-element #\Space))
                                                       line)))
                                               lines)))
                    ;; First line with enumerator
                    (push (format nil "~A~A~A" indent-str styled-enum (first padded-lines)) result)
                    ;; Continuation lines with indentation
                    (dolist (line (rest padded-lines))
                      (push (format nil "~A~A~A" indent-str continuation-indent line) result)))))))

    (format nil "~{~A~^~%~}" (nreverse result))))
