;;; test-borders.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Unit tests for border rendering (src/borders.lisp)
;;;; Border golden file tests are covered via table tests (TestBorderStyles).

(in-package #:tuition-tests)

(def-suite border-tests
  :description "Tests for border definitions and rendering."
  :in tuition-tests)

(in-suite border-tests)

;;; --- Border objects exist ---

(test border-normal-exists
  "Predefined *border-normal* exists."
  (is (typep *border-normal* 'border)))

(test border-rounded-exists
  "Predefined *border-rounded* exists."
  (is (typep *border-rounded* 'border)))

(test border-thick-exists
  "Predefined *border-thick* exists."
  (is (typep *border-thick* 'border)))

(test border-double-exists
  "Predefined *border-double* exists."
  (is (typep *border-double* 'border)))

(test border-block-exists
  "Predefined *border-block* exists."
  (is (typep *border-block* 'border)))

(test border-hidden-exists
  "Predefined *border-hidden* exists."
  (is (typep *border-hidden* 'border)))

(test border-ascii-exists
  "Predefined *border-ascii* exists."
  (is (typep *border-ascii* 'border)))

(test border-markdown-exists
  "Predefined *border-markdown* exists."
  (is (typep *border-markdown* 'border)))

;;; --- Border character access ---

(test border-normal-corners
  "Normal border has correct corner characters."
  (is (string= "┌" (tuition::border-top-left *border-normal*)))
  (is (string= "┐" (tuition::border-top-right *border-normal*)))
  (is (string= "└" (tuition::border-bottom-left *border-normal*)))
  (is (string= "┘" (tuition::border-bottom-right *border-normal*))))

(test border-rounded-corners
  "Rounded border has rounded corner characters."
  (is (string= "╭" (tuition::border-top-left *border-rounded*)))
  (is (string= "╮" (tuition::border-top-right *border-rounded*)))
  (is (string= "╰" (tuition::border-bottom-left *border-rounded*)))
  (is (string= "╯" (tuition::border-bottom-right *border-rounded*))))

(test border-double-corners
  "Double border has double-line corner characters."
  (is (string= "╔" (tuition::border-top-left *border-double*)))
  (is (string= "╗" (tuition::border-top-right *border-double*)))
  (is (string= "╚" (tuition::border-bottom-left *border-double*)))
  (is (string= "╝" (tuition::border-bottom-right *border-double*))))

;;; --- make-border ---

(test make-border-custom
  "make-border creates custom border."
  (let ((b (make-border :top "*" :bottom "*" :left "*" :right "*"
                        :top-left "+" :top-right "+"
                        :bottom-left "+" :bottom-right "+")))
    (is (string= "*" (tuition::border-top b)))
    (is (string= "+" (tuition::border-top-left b)))))

;;; --- render-border basic ---

(test render-border-simple
  "render-border produces bordered output with correct dimensions."
  (let ((result (render-border "hello" *border-normal*)))
    ;; Should have 3 lines (top border, content, bottom border)
    (is (= 3 (height result)))
    ;; Content "hello" is 5 chars, plus 2 border chars = 7 wide
    (is (= 7 (width result)))))

(test render-border-multiline
  "render-border with multi-line content."
  (let ((result (render-border (format nil "ab~%cdef") *border-normal*)))
    ;; 4 lines: top border, 2 content lines, bottom border
    (is (= 4 (height result)))))

(test render-border-partial-sides
  "render-border with some sides disabled."
  (let ((result (render-border "hi" *border-normal* :top nil :bottom nil)))
    ;; Only left+right borders on content, no top/bottom
    (is (= 1 (height result)))))

;;; --- render-shadow ---

(test render-shadow-basic
  "render-shadow adds shadow to content."
  (let ((result (render-shadow "hello")))
    ;; Shadow adds bottom row and right padding
    (is (> (height result) 1))))
