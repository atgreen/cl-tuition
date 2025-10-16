;;; basic.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Basic smoke tests for Tuition

(defpackage #:tuition-tests
  (:use #:cl #:tuition)
  (:export #:run-tests)
  (:documentation "Basic smoke tests for Tuition TUI library."))

(in-package #:tuition-tests)

(defun run-tests ()
  "Run basic smoke tests. Returns T if all pass, signals error otherwise."
  (let ((failures 0))
    (flet ((test (name form expected)
             (handler-case
                 (let ((result form))
                   (unless (equal result expected)
                     (format t "FAIL: ~A~%  Expected: ~S~%  Got: ~S~%" name expected result)
                     (incf failures)))
               (error (e)
                 (format t "ERROR in ~A: ~A~%" name e)
                 (incf failures)))))

      ;; Test visible-length with plain text
      (test "visible-length plain text"
            (visible-length "hello")
            5)

      ;; Test visible-length with ANSI codes
      (test "visible-length with ANSI"
            (visible-length (format nil "~C[31mhello~C[0m" #\Escape #\Escape))
            5)

      ;; Test wrap-text basic
      (test "wrap-text basic"
            (wrap-text "hello world" 5)
            (format nil "hello~%world"))

      ;; Test border rendering
      (test "border exists"
            (not (null *border-normal*))
            t)

      ;; Test style creation
      (test "make-style returns style"
            (typep (make-style :bold t) 'style)
            t)

      ;; Summary
      (if (zerop failures)
          (progn
            (format t "~%All tests passed!~%")
            t)
          (progn
            (format t "~%~D test(s) failed.~%" failures)
            (error "Tests failed"))))))
