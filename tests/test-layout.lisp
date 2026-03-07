;;; test-layout.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Tests for layout utilities, ported from lipgloss join_test.go

(in-package #:tuition-tests)

(def-suite layout-tests
  :description "Tests for layout utilities (ported from lipgloss)."
  :in tuition-tests)

(in-suite layout-tests)

;;; --- TestJoinVertical (from lipgloss join_test.go) ---

(test join-vertical-left
  "JoinVertical Left: short string padded right (lipgloss pos0)."
  (is (string= (format nil "A   ~%BBBB")
               (join-vertical :left "A" "BBBB"))))

(test join-vertical-right
  "JoinVertical Right: short string padded left (lipgloss pos1)."
  (is (string= (format nil "   A~%BBBB")
               (join-vertical :right "A" "BBBB"))))

(test join-vertical-fractional
  "JoinVertical 0.25: fractional position (lipgloss pos0.25)."
  (is (string= (format nil " A  ~%BBBB")
               (join-vertical 0.25 "A" "BBBB"))))

;;; --- TestJoinHorizontal (from lipgloss join_test.go) ---

(test join-horizontal-top
  "JoinHorizontal Top: single char aligned top (lipgloss pos0)."
  (is (string= (format nil "AB~% B~% B~% B")
               (join-horizontal :top "A" (format nil "B~%B~%B~%B")))))

(test join-horizontal-bottom
  "JoinHorizontal Bottom: single char aligned bottom (lipgloss pos1)."
  (is (string= (format nil " B~% B~% B~%AB")
               (join-horizontal :bottom "A" (format nil "B~%B~%B~%B")))))

(test join-horizontal-fractional
  "JoinHorizontal 0.25: fractional position (lipgloss pos0.25)."
  (is (string= (format nil " B~%AB~% B~% B")
               (join-horizontal 0.25 "A" (format nil "B~%B~%B~%B")))))

;;; --- Additional layout tests ---

(test join-horizontal-empty
  "join-horizontal with no blocks returns empty string."
  (is (string= "" (join-horizontal :top))))

(test join-vertical-empty
  "join-vertical with no blocks returns empty string."
  (is (string= "" (join-vertical :left))))

;;; --- align-text ---

(test align-text-left
  "align-text left pads right."
  (let ((result (tuition::align-text "hi" 10 :left)))
    (is (= 10 (visible-length result)))
    (is (string= "hi" (string-trim " " result)))))

(test align-text-right
  "align-text right pads left."
  (let ((result (tuition::align-text "hi" 10 :right)))
    (is (= 10 (visible-length result)))
    (is (char= #\Space (char result 0)))))

(test align-text-center
  "align-text center pads both sides."
  (let ((result (tuition::align-text "hi" 10 :center)))
    (is (= 10 (visible-length result)))))

;;; --- place ---

(test place-horizontal-left
  "place-horizontal aligns left within width."
  (let ((result (place-horizontal 20 :left "hello")))
    (is (= 20 (visible-length result)))))

(test place-horizontal-center
  "place-horizontal aligns center within width."
  (let ((result (place-horizontal 20 :center "hello")))
    (is (= 20 (visible-length result)))))

(test place-horizontal-right
  "place-horizontal aligns right within width."
  (let ((result (place-horizontal 20 :right "hello")))
    (is (= 20 (visible-length result)))))
