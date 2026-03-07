;;; basic.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Basic smoke tests for Tuition (FiveAM)

(defpackage #:tuition-tests
  (:use #:cl #:tuition)
  (:import-from #:fiveam
                #:def-suite #:in-suite #:test #:is #:signals #:finishes)
  (:export #:run-tests)
  (:documentation "Test suite for Tuition TUI library."))

(in-package #:tuition-tests)

(def-suite tuition-tests
  :description "Top-level test suite for Tuition.")

(def-suite basic-tests
  :description "Basic smoke tests."
  :in tuition-tests)

(in-suite basic-tests)

(test visible-length-plain-text
  "visible-length returns correct length for plain text."
  (is (= 5 (visible-length "hello"))))

(test visible-length-with-ansi
  "visible-length ignores ANSI escape codes."
  (is (= 5 (visible-length (format nil "~C[31mhello~C[0m" #\Escape #\Escape)))))

(test wrap-text-basic
  "wrap-text breaks text at word boundaries."
  (is (string= (format nil "hello~%world")
               (wrap-text "hello world" 5))))

(test border-exists
  "Predefined border objects exist."
  (is (not (null *border-normal*))))

(test make-style-returns-style
  "make-style returns a style object."
  (is (typep (make-style :bold t) 'style)))

(defun run-tests ()
  "Run all Tuition tests. Signals error on failure."
  (let ((results (5am:run 'tuition-tests)))
    (5am:explain! results)
    (unless (5am:results-status results)
      (error "Tests failed"))
    t))
