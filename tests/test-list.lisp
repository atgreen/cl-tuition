;;; test-list.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Golden file tests for list rendering, ported from lipgloss/list/list_test.go

(in-package #:tuition-tests)

(def-suite list-tests
  :description "Tests for list rendering (ported from lipgloss)."
  :in tuition-tests)

(in-suite list-tests)

;;; --- TestList (bullet enumerator, default) ---
(test list-basic
  "Basic list with bullet enumerator (lipgloss TestList)."
  (let ((lst (tuition.render.list:new-list "Foo" "Bar" "Baz")))
    (is (golden-equal "list" "TestList"
                      (tuition.render.list:list-render lst)))))

;;; --- TestListItems ---
(test list-items
  "List created via Items (lipgloss TestListItems)."
  (let ((lst (tuition.render.list:new-list "Foo" "Bar" "Baz")))
    (is (golden-equal "list" "TestListItems"
                      (tuition.render.list:list-render lst)))))

;;; --- TestListIntegers ---
(test list-integers
  "List with numeric string items (lipgloss TestListIntegers)."
  (let ((lst (tuition.render.list:new-list "1" "2" "3")))
    (is (golden-equal "list" "TestListIntegers"
                      (tuition.render.list:list-render lst)))))

;;; --- TestSublist ---
(test list-sublist
  "List with nested Roman-enumerated sublist (lipgloss TestSublist)."
  (let ((sub (tuition.render.list:new-list "Hi" "Hello" "Halo")))
    (setf (tuition.render.list::list-enumerator sub) #'tuition.render.list:roman-enumerator)
    (let ((lst (tuition.render.list:new-list "Foo" "Bar")))
      (tuition.render.list:list-item lst sub)
      (tuition.render.list:list-item lst "Qux")
      (is (golden-equal "list" "TestSublist"
                        (tuition.render.list:list-render lst))))))

;;; --- TestSublistItems ---
(test list-sublist-items
  "List with nested Roman sublist via constructor (lipgloss TestSublistItems)."
  (let ((sub (tuition.render.list:new-list "D" "E" "F")))
    (setf (tuition.render.list::list-enumerator sub) #'tuition.render.list:roman-enumerator)
    (let ((lst (tuition.render.list:new-list "A" "B" "C")))
      (tuition.render.list:list-item lst sub)
      (tuition.render.list:list-item lst "G")
      (is (golden-equal "list" "TestSublistItems"
                        (tuition.render.list:list-render lst))))))

;;; --- TestMultiline ---
(test list-multiline
  "List with multi-line items (lipgloss TestMultiline)."
  (let ((lst (tuition.render.list:new-list
              (format nil "Item1~%line 2~%line 3")
              (format nil "Item2~%line 2~%line 3")
              "3")))
    (is (golden-equal "list" "TestMultiline"
                      (tuition.render.list:list-render lst)))))

;;; --- TestEnumerators ---

(test list-enumerator-bullet
  "List with bullet enumerator (lipgloss TestEnumerators/bullet)."
  (let ((lst (tuition.render.list:new-list "Foo" "Bar" "Baz")))
    (is (golden-equal "list/TestEnumerators" "bullet"
                      (tuition.render.list:list-render lst)))))

(test list-enumerator-arabic
  "List with arabic enumerator (lipgloss TestEnumerators/arabic)."
  (let ((lst (tuition.render.list:new-list "Foo" "Bar" "Baz")))
    (setf (tuition.render.list::list-enumerator lst) #'tuition.render.list:arabic-enumerator)
    (is (golden-equal "list/TestEnumerators" "arabic"
                      (tuition.render.list:list-render lst)))))

(test list-enumerator-alphabet
  "List with alphabet enumerator (lipgloss TestEnumerators/alphabet)."
  (let ((lst (tuition.render.list:new-list "Foo" "Bar" "Baz")))
    (setf (tuition.render.list::list-enumerator lst) #'tuition.render.list:alphabet-enumerator)
    (is (golden-equal "list/TestEnumerators" "alphabet"
                      (tuition.render.list:list-render lst)))))

(test list-enumerator-roman
  "List with roman enumerator (lipgloss TestEnumerators/roman)."
  (let ((lst (tuition.render.list:new-list "Foo" "Bar" "Baz")))
    (setf (tuition.render.list::list-enumerator lst) #'tuition.render.list:roman-enumerator)
    (is (golden-equal "list/TestEnumerators" "roman"
                      (tuition.render.list:list-render lst)))))

;;; --- TestEnumeratorsAlign ---
(test list-enumerators-align
  "100 items with Roman numerals for alignment (lipgloss TestEnumeratorsAlign)."
  (let ((lst (apply #'tuition.render.list:new-list
                    (loop for i from 1 to 100
                          collect "Foo"))))
    (setf (tuition.render.list::list-enumerator lst) #'tuition.render.list:roman-enumerator)
    (is (golden-equal "list" "TestEnumeratorsAlign"
                      (tuition.render.list:list-render lst)))))
