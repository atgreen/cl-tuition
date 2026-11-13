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

;;; --- print-above-view (cl-tuition-fmd) ---
(test println-printf-commands
  "println/printf return commands producing a print-line-msg with the text."
  (let ((m1 (funcall (println "a" "b" 3))))
    (is (print-line-msg-p m1))
    (is (string= "ab3" (print-line-msg-content m1))))
  (let ((m2 (funcall (printf "~D-~A" 7 "x"))))
    (is (print-line-msg-p m2))
    (is (string= "7-x" (print-line-msg-content m2)))))

(test insert-above-advances-origin
  "insert-above writes the line, advances the draw origin, and invalidates the buffer."
  (let* ((out (make-string-output-stream))
         (r (make-instance 'tuition::renderer :output-stream out)))
    (setf (tuition::renderer-height r) 24
          (tuition::last-buffer r) (tuition::parse-styled-string "x" 10 3))
    (let ((n (tuition::insert-above r "hello")))
      (is (= 1 n))
      (is (= 1 (tuition::renderer-origin-y r)))   ; origin moved down one row
      (is (null (tuition::last-buffer r)))          ; forces a full repaint
      (let ((s (get-output-stream-string out)))
        (is (search "hello" s))
        (is (search (format nil "~C[K" #\Escape) s))))   ; clears to end of line
    ;; A multi-line message advances the origin by its line count.
    (setf (tuition::last-buffer r) (tuition::parse-styled-string "x" 10 3))
    (tuition::insert-above r (format nil "a~%b~%c"))
    (is (= 4 (tuition::renderer-origin-y r)))))

(test insert-above-noop-under-altscreen
  "insert-above is a no-op when the alternate screen is active."
  (let* ((out (make-string-output-stream))
         (r (make-instance 'tuition::renderer :output-stream out)))
    (setf (tuition::last-view-state r)
          (make-instance 'tuition::view-state :alt-screen t))
    (is (null (tuition::insert-above r "nope")))
    (is (= 0 (tuition::renderer-origin-y r)))
    (is (string= "" (get-output-stream-string out)))))

(test render-origin-offset
  "Rendering homes to ESC[H at origin 0 (unchanged) and to the origin row otherwise."
  (let* ((out (make-string-output-stream))
         (r (make-instance 'tuition::renderer :output-stream out)))
    (tuition::render r "hi")
    (is (search (format nil "~C[H" #\Escape) (get-output-stream-string out))))
  (let* ((out (make-string-output-stream))
         (r (make-instance 'tuition::renderer :output-stream out)))
    (setf (tuition::renderer-origin-y r) 2)
    (tuition::render r (format nil "line1~%line2"))
    (let ((s (get-output-stream-string out)))
      ;; Homed to the origin row, not the top of the screen.
      (is (search (format nil "~C[3;1H" #\Escape) s)))))

;;; --- get-terminal-size hardening (cl-tuition-2uz) ---
(test terminal-size-sanitize
  "Terminal size never resolves to 0x0, a missing dimension, or an implausible size."
  (flet ((usable (s) (and (consp s) (integerp (car s)) (plusp (car s))
                          (integerp (cdr s)) (plusp (cdr s)))))
    ;; A plausible size passes through unchanged.
    (is (equal '(100 . 40) (tuition::%sanitize-terminal-size 100 40)))
    ;; 0x0 (pipes/CI), a missing dimension, and a non-integer all fall back.
    (is (usable (tuition::%sanitize-terminal-size 0 0)))
    (is (usable (tuition::%sanitize-terminal-size nil 24)))
    (is (usable (tuition::%sanitize-terminal-size 80 nil)))
    ;; Plausibility bounds.
    (is (tuition::%plausible-terminal-size-p 80 24))
    (is (not (tuition::%plausible-terminal-size-p 0 0)))
    (is (not (tuition::%plausible-terminal-size-p 5000 10)))      ; exceeds max dimension
    (is (not (tuition::%plausible-terminal-size-p 999999 999999))) ; exceeds max area
    ;; The public entry point always returns a usable cons.
    (is (usable (tuition::get-terminal-size)))))

(defun run-tests ()
  "Run all Tuition tests. Signals error on failure."
  (let ((results (5am:run 'tuition-tests)))
    (5am:explain! results)
    (unless (5am:results-status results)
      (error "Tests failed"))
    t))
