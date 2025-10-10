;;; list.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; List rendering sub-package inspired by Lipgloss

(defpackage #:tuition.list
  (:use #:cl)
  (:nicknames #:tui.list)
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

(in-package #:tuition.list)

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
  "Alphabetic enumerator (a. b. c. ...)."
  (declare (ignore items))
  (let ((letter (code-char (+ (char-code #\a) index))))
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
    (format nil "~A. " (string-downcase result))))

(defun tree-enumerator (items index)
  "Tree-style enumerator with branches."
  (if (= index (1- (length items))) "└── " "├── "))

;;; Rendering
(defun list-render (list)
  "Render a list to a string."
  (let ((items (list-items list))
        (enum-func (list-enumerator list))
        (enum-style (list-enumerator-style list))
        (item-style (list-item-style list))
        (indent (list-indent list))
        (result nil))

    (loop for item in items
          for idx from 0
          do (let* ((enum-text (funcall enum-func items idx))
                    (styled-enum (if enum-style
                                     (tuition:render-styled enum-style enum-text)
                                     enum-text))
                    (item-text (cond
                                 ;; Nested list
                                 ((typep item 'tui-list)
                                  (let ((nested (list-render item)))
                                    (tuition:indent-lines nested
                                                         (tuition:visible-length enum-text))))
                                 ;; String item
                                 (t (if item-style
                                        (tuition:render-styled item-style (format nil "~A" item))
                                        (format nil "~A" item)))))
                    (indent-str (make-string indent :initial-element #\Space)))
               (push (format nil "~A~A~A" indent-str styled-enum item-text) result)))

    (format nil "~{~A~^~%~}" (nreverse result))))
