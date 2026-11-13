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

;;; --- Kitty keyboard protocol (CSI u) ---

(test read-key-kitty-plain
  "read-key parses ESC [ 97 u as an 'a' press."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[97u" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eql #\a (key-event-code msg))))))

(test read-key-kitty-modifier-and-event
  "ESC [ 97 ; 5:3 u decodes ctrl (mod 5) and release (event 3)."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[97;5:3u" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-release-msg-p msg))
      (is (eql #\a (key-event-code msg)))
      (is (mod-contains (key-event-mod msg) +mod-ctrl+)))))

(test read-key-kitty-modifier-press
  "ESC [ 97 ; 5 u decodes ctrl and defaults to a press (event type 1)."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[97;5u" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (mod-contains (key-event-mod msg) +mod-ctrl+)))))

(test read-key-kitty-shifted-alternate-key
  "ESC [ 97:65 ; 2 u (flag 4) reads base key 'a' and exposes shifted 'A'."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[97:65;2u" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (eql #\a (key-event-code msg)))
      (is (eql #\A (key-event-shifted-code msg)))
      (is (null (key-event-base-code msg)))
      (is (mod-contains (key-event-mod msg) +mod-shift+)))))

(test read-key-kitty-shifted-and-base-alternate-keys
  "ESC [ 97:65:113 ; 1 u exposes both the shifted and base-layout keys."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[97:65:113;1u" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (eql #\a (key-event-code msg)))
      (is (eql #\A (key-event-shifted-code msg)))
      (is (eql #\q (key-event-base-code msg))))))       ; 113 = q (e.g. AZERTY base)

(test read-key-kitty-no-alternate-keys
  "A plain Kitty sequence leaves shifted/base codes NIL."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[97;5u" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (null (key-event-shifted-code msg)))
      (is (null (key-event-base-code msg))))))

(test read-key-kitty-associated-text
  "ESC [ 97 ; 1 ; 233 u (flag 16) uses the associated text codepoint."
  (let ((tuition::*input-stream*
          (make-string-input-stream (format nil "~C[97;1;233u" #\Escape))))
    (let ((msg (tuition::read-key)))
      (is (key-press-msg-p msg))
      (is (string= (string (code-char 233)) (key-event-text msg))))))

(test kitty-enhancement-flags-emitted
  "Requesting all keyboard enhancements pushes flags 1|2|4|8|16 = 31."
  (let* ((vs (tuition:make-view "x"
                                :keyboard-enhancements
                                '(:report-event-types t
                                  :report-alternate-keys t
                                  :report-all-keys-as-escapes t
                                  :report-associated-text t)))
         (out (with-output-to-string (s)
                (tuition::apply-terminal-transitions nil nil vs s))))
    (is (search (format nil "~C[>31u" #\Escape) out))))

(test kitty-enhancement-flags-default-disambiguate-only
  "With no options, only the disambiguate flag (1) is pushed."
  (let* ((vs (tuition:make-view "x" :keyboard-enhancements '(:enabled t)))
         (out (with-output-to-string (s)
                (tuition::apply-terminal-transitions nil nil vs s))))
    (is (search (format nil "~C[>1u" #\Escape) out))))

;;; --- Terminal progress bar (OSC 9;4) ---

(defun %pb-transition (prev cur)
  "Return the escape output emitted transitioning progress bar PREV -> CUR."
  (with-output-to-string (s)
    (tuition::apply-terminal-transitions
     nil
     (and prev (tuition:make-view "x" :progress-bar prev))
     (tuition:make-view "x" :progress-bar cur)
     s)))

(test progress-bar-default-sets-percentage
  "A default progress bar emits OSC 9;4;1;<value>."
  (is (search (format nil "~C]9;4;1;42~C" (code-char 27) (code-char 7))
              (%pb-transition nil (tuition:make-progress-bar :state :default :value 42)))))

(test progress-bar-indeterminate
  "An indeterminate progress bar emits OSC 9;4;3 with no value."
  (is (search (format nil "~C]9;4;3~C" (code-char 27) (code-char 7))
              (%pb-transition nil (tuition:make-progress-bar :state :indeterminate)))))

(test progress-bar-clamps-value
  "Values above 100 are clamped."
  (is (search (format nil "~C]9;4;2;100~C" (code-char 27) (code-char 7))
              (%pb-transition nil (tuition:make-progress-bar :state :error :value 250)))))

(test progress-bar-removal-resets
  "Removing the progress bar (NIL) emits the reset sequence OSC 9;4;0."
  (is (search (format nil "~C]9;4;0~C" (code-char 27) (code-char 7))
              (%pb-transition (tuition:make-progress-bar :state :default :value 10) nil))))

(test progress-bar-unchanged-emits-nothing
  "An unchanged progress bar emits no escape sequence."
  (let ((pb (tuition:make-progress-bar :state :default :value 10)))
    (is (string= "" (%pb-transition pb pb)))))

;;; --- key-string ---

(test key-string-char
  "key-string returns character as string."
  (is (string= "a" (key-string (make-key-press-msg :code #\a)))))

(test key-string-keyword
  "key-string returns keyword as lowercase string."
  (is (string= "enter" (key-string (make-key-press-msg :code :enter)))))

;;; --- incomplete-input-p (batched input, e.g. the Windows console) ---

(defun %esc (fmt &rest args)
  "FORMAT with ~C bound to ESC first."
  (apply #'format nil fmt #\Escape args))

(test incomplete-input-plain-text
  "Plain text is complete."
  (is (not (tuition::incomplete-input-p "abc"))))

(test incomplete-input-lone-esc
  "A trailing ESC may start a sequence."
  (is (tuition::incomplete-input-p (%esc "a~C"))))

(test incomplete-input-csi
  "A CSI sequence is complete once its final byte arrives."
  (is (tuition::incomplete-input-p (%esc "~C[")))
  (is (tuition::incomplete-input-p (%esc "~C[1;5")))
  (is (not (tuition::incomplete-input-p (%esc "~C[A"))))
  (is (not (tuition::incomplete-input-p (%esc "~C[1;5C")))))

(test incomplete-input-sgr-mouse
  "An SGR mouse report is incomplete until its M/m."
  (is (tuition::incomplete-input-p (%esc "~C[<0;12;3")))
  (is (not (tuition::incomplete-input-p (%esc "~C[<0;12;3M")))))

(test incomplete-input-ss3
  "SS3 needs one character after the O."
  (is (tuition::incomplete-input-p (%esc "~CO")))
  (is (not (tuition::incomplete-input-p (%esc "~COP")))))

(test incomplete-input-osc
  "OSC is complete once terminated by BEL or ST."
  (is (tuition::incomplete-input-p (%esc "~C]11;rgb:0/0/0")))
  (is (not (tuition::incomplete-input-p
            (%esc "~C]11;rgb:0/0/0~C" (code-char 7)))))
  (is (not (tuition::incomplete-input-p (%esc "~C]11;rgb:0/0/0~C\\" #\Escape)))))

(test incomplete-input-alt-key
  "ESC followed by a plain character (Alt+key) is complete."
  (is (not (tuition::incomplete-input-p (%esc "~Cx")))))

(test incomplete-input-paste
  "A bracketed paste is incomplete until its end marker."
  (is (eq :paste (tuition::incomplete-input-p (%esc "~C[200~~hello"))))
  (is (not (tuition::incomplete-input-p (%esc "~C[200~~hello~C[201~~" #\Escape)))))

(test batched-input-parses-every-event
  "A batch holding several keys, an arrow and a mouse report parses into one
event each, as the Windows console input loop does."
  (let* ((tuition::*input-stream*
           (make-string-input-stream
            (%esc "a~C[D~C[<0;5;7Mb" #\Escape)))
         (events (tuition::read-all-available-events)))
    (is (= 4 (length events)))
    (is (eql #\a (key-event-code (first events))))
    (is (eq :left (key-event-code (second events))))
    (is (typep (third events) 'tuition::mouse-click-msg))
    (is (eql #\b (key-event-code (fourth events))))))
