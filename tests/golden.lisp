;;; golden.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Golden file test helper (charmbracelet-style visual regression testing)

(in-package #:tuition-tests)

(defvar *golden-update* nil
  "When true, golden files are written instead of compared.")

(defvar *golden-base-dir*
  (asdf:system-relative-pathname :tuition/tests "tests/testdata/")
  "Base directory for golden test data files.")

(defun golden-file-path (suite name)
  "Return the pathname for a golden file: tests/testdata/SUITE/NAME.golden"
  (merge-pathnames (make-pathname :directory `(:relative ,suite)
                                  :name name
                                  :type "golden")
                   *golden-base-dir*))

(defun ensure-golden-dir (path)
  "Ensure the directory for PATH exists."
  (ensure-directories-exist path))

(defun golden-equal (suite name actual)
  "Compare ACTUAL string against golden file tests/testdata/SUITE/NAME.golden.
When TUITION_UPDATE_GOLDEN=1 env var is set (or *golden-update* is true),
writes ACTUAL to the golden file instead of comparing.
Returns T if equal (or updated), NIL with explanation on mismatch."
  (let ((path (golden-file-path suite name))
        (update (or *golden-update*
                    (equal (uiop:getenv "TUITION_UPDATE_GOLDEN") "1"))))
    (if update
        (progn
          (ensure-golden-dir path)
          (with-open-file (s path :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
            (write-string actual s))
          t)
        (progn
          (unless (probe-file path)
            (5am:fail "Golden file not found: ~A~%Run with TUITION_UPDATE_GOLDEN=1 to create." path)
            (return-from golden-equal nil))
          (let ((expected (uiop:read-file-string path)))
            (if (string= actual expected)
                t
                (progn
                  (5am:fail "Golden mismatch for ~A/~A~%~
                             --- Expected (from ~A):~%~A~%~
                             --- Actual:~%~A"
                            suite name path expected actual)
                  nil)))))))
