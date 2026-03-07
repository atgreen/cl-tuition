;;; test-text.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Tests for text utilities (wrap-text, truncate-text, ellipsize, visible-length)

(in-package #:tuition-tests)

(def-suite text-tests
  :description "Tests for text reflow and measurement utilities."
  :in tuition-tests)

(in-suite text-tests)

;;; --- wrap-text ---

(test wrap-basic-words
  "wrap-text breaks at word boundaries."
  (is (string= (format nil "hello~%world")
               (wrap-text "hello world" 5))))

(test wrap-exact-width
  "wrap-text doesn't break when text fits exactly."
  (is (string= "hello" (wrap-text "hello" 5))))

(test wrap-long-word-no-break
  "wrap-text with long word and no break-words puts word on its own line."
  (let ((result (wrap-text "hi abcdefghij" 5)))
    ;; "hi" on first line, the long word on second
    (is (= 2 (height result)))))

(test wrap-long-word-with-break
  "wrap-text with break-words splits long words."
  (let ((result (wrap-text "abcdefghij" 5 :break-words t)))
    (is (>= (height result) 2))
    ;; Every line should be at most 5 columns
    (dolist (line (split-string-by-newline result))
      (is (<= (visible-length line) 5)))))

(test wrap-preserves-newlines
  "wrap-text preserves existing newlines."
  (let ((result (wrap-text (format nil "line1~%line2") 20)))
    (is (= 2 (height result)))))

(test wrap-with-indent
  "wrap-text applies indent to first line."
  (let ((result (wrap-text "hello world" 15 :indent 3)))
    (let ((first-line (first (split-string-by-newline result))))
      (is (char= #\Space (char first-line 0))))))

(test wrap-with-continuation-indent
  "wrap-text applies continuation-indent to wrapped lines."
  (let ((result (wrap-text "aaa bbb ccc ddd" 8 :continuation-indent 2)))
    (let ((lines (split-string-by-newline result)))
      (when (> (length lines) 1)
        (let ((second-line (second lines)))
          (is (char= #\Space (char second-line 0))))))))

(test wrap-empty-string
  "wrap-text handles empty string."
  (is (string= "" (wrap-text "" 10))))

(test wrap-width-1
  "wrap-text handles width of 1."
  (let ((result (wrap-text "ab" 1 :break-words t)))
    (is (= 2 (height result)))))

(test wrap-ansi-preserved
  "wrap-text preserves ANSI escape sequences."
  (let* ((styled (format nil "~C[31mhello world~C[0m" #\Escape #\Escape))
         (result (wrap-text styled 6)))
    ;; ANSI codes should survive wrapping
    (is (search (format nil "~C[31m" #\Escape) result))))

;;; --- truncate-text ---

(test truncate-no-change
  "truncate-text doesn't modify text shorter than width."
  (is (string= "hello" (truncate-text "hello" 10))))

(test truncate-at-width
  "truncate-text with text exactly at width (no-ellipsis)."
  (is (string= "hello" (truncate-text "hello" 5 :ellipsis ""))))

(test truncate-with-ellipsis
  "truncate-text adds ellipsis when truncating."
  (let ((result (truncate-text "hello world" 8)))
    (is (<= (visible-length result) 8))
    (is (search "…" result))))

(test truncate-custom-ellipsis
  "truncate-text with custom ellipsis."
  (let ((result (truncate-text "hello world" 8 :ellipsis "...")))
    (is (<= (visible-length result) 8))
    (is (search "..." result))))

(test truncate-no-ellipsis
  "truncate-text with empty ellipsis just cuts."
  (let ((result (truncate-text "hello world" 5 :ellipsis "")))
    (is (= 5 (visible-length result)))))

(test truncate-very-short
  "truncate-text at width 1."
  (let ((result (truncate-text "hello" 1 :ellipsis "")))
    (is (<= (visible-length result) 1))))

(test truncate-preserves-ansi
  "truncate-text preserves ANSI codes in output."
  (let* ((styled (format nil "~C[31mhello~C[0m" #\Escape #\Escape))
         (result (truncate-text styled 3 :ellipsis "")))
    (is (<= (visible-length result) 3))
    (is (search (format nil "~C[" #\Escape) result))))

;;; --- ellipsize ---

(test ellipsize-short
  "ellipsize doesn't modify short text."
  (is (string= "hi" (ellipsize "hi" 10))))

(test ellipsize-long
  "ellipsize truncates with default ellipsis."
  (let ((result (ellipsize "hello world" 8)))
    (is (<= (visible-length result) 8))
    (is (search "…" result))))

;;; --- visible-length edge cases ---

(test visible-length-nil-safe
  "visible-length of empty string is 0."
  (is (= 0 (visible-length ""))))

(test visible-length-spaces
  "visible-length counts spaces."
  (is (= 3 (visible-length "   "))))

(test visible-length-tabs
  "visible-length counts tab as 0 width (control char)."
  (is (= 0 (visible-length (string #\Tab)))))

(test visible-length-nested-ansi
  "visible-length handles nested/multiple ANSI codes."
  (let ((s (format nil "~C[1m~C[31mbold red~C[0m~C[0m"
                   #\Escape #\Escape #\Escape #\Escape)))
    (is (= 8 (visible-length s)))))

(test visible-length-ansi-no-text
  "visible-length of only ANSI codes is 0."
  (let ((s (format nil "~C[31m~C[0m" #\Escape #\Escape)))
    (is (= 0 (visible-length s)))))
