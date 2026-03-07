;;; test-input.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Tests for input parsing (src/input.lisp)

(in-package #:tuition-tests)

(def-suite input-tests
  :description "Tests for input parsing."
  :in tuition-tests)

(in-suite input-tests)

;;; --- ctrl-char-to-key ---

(test ctrl-char-backspace
  "ctrl-char-to-key maps code 8 to :backspace."
  (is (eq :backspace (tuition::ctrl-char-to-key (code-char 8)))))

(test ctrl-char-tab
  "ctrl-char-to-key maps code 9 to :tab."
  (is (eq :tab (tuition::ctrl-char-to-key (code-char 9)))))

(test ctrl-char-enter-lf
  "ctrl-char-to-key maps code 10 to :enter."
  (is (eq :enter (tuition::ctrl-char-to-key (code-char 10)))))

(test ctrl-char-enter-cr
  "ctrl-char-to-key maps code 13 to :enter."
  (is (eq :enter (tuition::ctrl-char-to-key (code-char 13)))))

(test ctrl-char-ctrl-a
  "ctrl-char-to-key maps code 1 (Ctrl-A) to character a."
  (is (char= #\a (tuition::ctrl-char-to-key (code-char 1)))))

(test ctrl-char-ctrl-c
  "ctrl-char-to-key maps code 3 (Ctrl-C) to character c."
  (is (char= #\c (tuition::ctrl-char-to-key (code-char 3)))))

(test ctrl-char-ctrl-z
  "ctrl-char-to-key maps code 26 (Ctrl-Z) to character z."
  (is (char= #\z (tuition::ctrl-char-to-key (code-char 26)))))

;;; --- read-key with mocked input ---

(test read-key-regular-char
  "read-key returns key-press-msg for regular characters."
  (let ((tuition::*input-stream* (make-string-input-stream "a")))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (char= #\a (key-event-code msg)))
      (is (string= "a" (key-event-text msg))))))

(test read-key-del
  "read-key maps DEL (127) to :backspace."
  (let ((tuition::*input-stream* (make-string-input-stream (string (code-char 127)))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eq :backspace (key-event-code msg))))))

(test read-key-ctrl-char
  "read-key maps control characters to key + mod-ctrl."
  (let ((tuition::*input-stream* (make-string-input-stream (string (code-char 1)))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (char= #\a (key-event-code msg)))
      (is (mod-contains (key-event-mod msg) +mod-ctrl+)))))

(test read-key-no-input
  "read-key returns nil when no input available."
  (let ((tuition::*input-stream* (make-string-input-stream "")))
    (is (null (tuition::read-key)))))

;;; --- CSI arrow key sequences ---

(test read-key-arrow-up
  "read-key parses ESC [ A as :up."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[A" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eq :up (key-event-code msg))))))

(test read-key-arrow-down
  "read-key parses ESC [ B as :down."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[B" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eq :down (key-event-code msg))))))

(test read-key-arrow-right
  "read-key parses ESC [ C as :right."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[C" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eq :right (key-event-code msg))))))

(test read-key-arrow-left
  "read-key parses ESC [ D as :left."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[D" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eq :left (key-event-code msg))))))

;;; --- CSI special keys ---

(test read-key-home
  "read-key parses ESC [ H as :home."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[H" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eq :home (key-event-code msg))))))

(test read-key-end
  "read-key parses ESC [ F as :end."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[F" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eq :end (key-event-code msg))))))

(test read-key-delete
  "read-key parses ESC [ 3 ~ as :delete."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[3~~" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eq :delete (key-event-code msg))))))

(test read-key-page-up
  "read-key parses ESC [ 5 ~ as :page-up."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[5~~" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eq :page-up (key-event-code msg))))))

(test read-key-page-down
  "read-key parses ESC [ 6 ~ as :page-down."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[6~~" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eq :page-down (key-event-code msg))))))

;;; --- Modified keys ---

(test read-key-shift-up
  "read-key parses ESC [ 1 ; 2 A as shift+up."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[1;2A" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eq :up (key-event-code msg)))
      (is (mod-contains (key-event-mod msg) +mod-shift+)))))

(test read-key-ctrl-right
  "read-key parses ESC [ 1 ; 5 C as ctrl+right."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[1;5C" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eq :right (key-event-code msg)))
      (is (mod-contains (key-event-mod msg) +mod-ctrl+)))))

;;; --- Backtab ---

(test read-key-backtab
  "read-key parses ESC [ Z as shift+tab."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[Z" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eq :tab (key-event-code msg)))
      (is (mod-contains (key-event-mod msg) +mod-shift+)))))

;;; --- Focus events ---

(test read-key-focus-in
  "read-key parses ESC [ I as focus event."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[I" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (focus-msg-p msg)))))

(test read-key-focus-out
  "read-key parses ESC [ O as blur event."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[O" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (blur-msg-p msg)))))

;;; --- SS3 sequences ---

(test read-key-ss3-up
  "read-key parses ESC O A as :up."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~COA" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eq :up (key-event-code msg))))))

;;; --- key-string ---

(test key-string-char
  "key-string returns character as string."
  (is (string= "a" (key-string (make-key-press-msg :code #\a)))))

(test key-string-keyword
  "key-string returns keyword as lowercase string."
  (is (string= "enter" (key-string (make-key-press-msg :code :enter)))))
