;;; test-textarea.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Tests for the textarea component (src/components/textarea.lisp)

(in-package #:tuition-tests)

(def-suite textarea-tests
  :description "Tests for the textarea component."
  :in tuition-tests)

(in-suite textarea-tests)

;;; --- test helpers ---

(defun ta-make (value &rest make-args)
  "Make a textarea, set VALUE, leaving the cursor at (0,0)."
  (let ((ta (apply #'tui.textarea:make-textarea make-args)))
    (tui.textarea:textarea-set-value ta value)
    ta))

(defun ta-cursor (ta)
  "Return the cursor position as a list (row col)."
  (multiple-value-list (tui.textarea:textarea-cursor-position ta)))

(defun ta-set-cursor (ta row col)
  "Force the cursor to ROW, COL."
  (setf (tui.textarea::textarea-row ta) row
        (tui.textarea::textarea-col ta) col)
  ta)

(defun ta-key (ta code &optional mod)
  "Feed a key-press message to TA (focused).  Returns TA."
  (tui.textarea:textarea-update ta
                                (make-key-press-msg :code code :mod (or mod 0)))
  ta)

;;; --- document-level movement (#809) ---

(test textarea-move-to-end
  "Move-to-end lands on the last row, past its last character."
  (let ((ta (ta-make (format nil "aaa~%bbb~%ccc"))))
    (tui.textarea:textarea-move-to-end ta)
    (is (equal '(2 3) (ta-cursor ta)))))

(test textarea-move-to-begin
  "Move-to-begin lands on (0,0) even from the end."
  (let ((ta (ta-make (format nil "aaa~%bbb~%ccc"))))
    (tui.textarea:textarea-move-to-end ta)
    (tui.textarea:textarea-move-to-begin ta)
    (is (equal '(0 0) (ta-cursor ta)))))

;;; --- word under cursor (#814) ---

(test textarea-word-finds-word
  "Word() returns the word under the cursor."
  (let ((ta (ta-make "hello world")))
    (ta-set-cursor ta 0 1)
    (is (string= "hello" (tui.textarea:textarea-word ta)))
    (ta-set-cursor ta 0 7)
    (is (string= "world" (tui.textarea:textarea-word ta)))))

(test textarea-word-empty-when-not-on-word
  "Word() is empty on whitespace or at the very start."
  (let ((ta (ta-make "hello world")))
    (ta-set-cursor ta 0 6)                       ; on the space
    (is (string= "" (tui.textarea:textarea-word ta)))
    (ta-set-cursor ta 0 0)                       ; before the first char
    (is (string= "" (tui.textarea:textarea-word ta)))))

;;; --- word navigation ---

(test textarea-word-forward-walks
  "Alt-f walks forward word by word, skipping whitespace."
  (let ((ta (ta-make "alpha beta gamma")))
    (ta-set-cursor ta 0 0)
    (tui.textarea:textarea-cursor-word-forward ta)
    (is (equal '(0 6) (ta-cursor ta)))           ; start of "beta"
    (tui.textarea:textarea-cursor-word-forward ta)
    (is (equal '(0 11) (ta-cursor ta)))          ; start of "gamma"
    (tui.textarea:textarea-cursor-word-forward ta)
    (is (equal '(0 16) (ta-cursor ta)))))        ; end of line

(test textarea-word-backward-walks
  "Alt-b walks backward word by word, skipping whitespace."
  (let ((ta (ta-make "alpha beta gamma")))
    (ta-set-cursor ta 0 16)                       ; end of line
    (tui.textarea:textarea-cursor-word-backward ta)
    (is (equal '(0 11) (ta-cursor ta)))          ; start of "gamma"
    (tui.textarea:textarea-cursor-word-backward ta)
    (is (equal '(0 6) (ta-cursor ta)))           ; start of "beta"
    (tui.textarea:textarea-cursor-word-backward ta)
    (is (equal '(0 0) (ta-cursor ta)))))         ; start of "alpha"

(test textarea-key-alt-f-moves-word
  "Alt+f dispatched through update moves a word forward."
  (let ((ta (ta-make "alpha beta")))
    (tui.textarea:textarea-focus ta)
    (ta-key ta #\f +mod-alt+)
    (is (equal '(0 6) (ta-cursor ta)))))

;;; --- word deletion ---

(test textarea-delete-word-forward
  "Alt-d deletes from the cursor to the end of the current word."
  (let ((ta (ta-make "alpha beta")))
    (ta-set-cursor ta 0 0)
    (tui.textarea:textarea-delete-word-forward ta)
    (is (string= "beta" (tui.textarea:textarea-value ta)))
    (is (equal '(0 0) (ta-cursor ta)))))

(test textarea-delete-word-backward
  "Alt-Backspace deletes the word (and trailing space) before the cursor."
  (let ((ta (ta-make "alpha beta")))
    (ta-set-cursor ta 0 6)                        ; on "beta"
    (tui.textarea:textarea-delete-word-backward ta)
    (is (string= "beta" (tui.textarea:textarea-value ta)))
    (is (equal '(0 0) (ta-cursor ta)))))

(test textarea-key-ctrl-w-deletes-word
  "Ctrl+w dispatched through update deletes a word backward."
  (let ((ta (ta-make "foo bar baz")))
    (tui.textarea:textarea-focus ta)
    (ta-set-cursor ta 0 11)                       ; end
    (ta-key ta #\w +mod-ctrl+)
    (is (string= "foo bar " (tui.textarea:textarea-value ta)))))

;;; --- transpose & word case (readline niceties) ---

(test textarea-transpose-at-end
  "Ctrl-T at end of line swaps the last two characters."
  (let ((ta (ta-make "abc")))
    (ta-set-cursor ta 0 3)
    (tui.textarea:textarea-transpose-chars ta)
    (is (string= "acb" (tui.textarea:textarea-value ta)))))

(test textarea-transpose-mid-advances
  "Ctrl-T mid-line swaps around the cursor and advances it."
  (let ((ta (ta-make "abc")))
    (ta-set-cursor ta 0 1)
    (tui.textarea:textarea-transpose-chars ta)
    (is (string= "bac" (tui.textarea:textarea-value ta)))
    (is (equal '(0 2) (ta-cursor ta)))))

(test textarea-transpose-at-start-noop
  "Ctrl-T at the start of a line does nothing."
  (let ((ta (ta-make "abc")))
    (tui.textarea:textarea-transpose-chars ta)
    (is (string= "abc" (tui.textarea:textarea-value ta)))))

(test textarea-capitalize-word
  "Alt-c capitalizes the word at the cursor."
  (let ((ta (ta-make "hello world")))
    (tui.textarea:textarea-capitalize-word ta)
    (is (string= "Hello world" (tui.textarea:textarea-value ta)))
    (is (equal '(0 5) (ta-cursor ta)))))

(test textarea-uppercase-word
  "Alt-u uppercases the word at the cursor."
  (let ((ta (ta-make "hello world")))
    (tui.textarea:textarea-uppercase-word ta)
    (is (string= "HELLO world" (tui.textarea:textarea-value ta)))))

(test textarea-lowercase-word
  "Alt-l lowercases the word at the cursor."
  (let ((ta (ta-make "HELLO WORLD")))
    (tui.textarea:textarea-lowercase-word ta)
    (is (string= "hello WORLD" (tui.textarea:textarea-value ta)))))

(test textarea-key-ctrl-t-transposes
  "Ctrl+t dispatched through update transposes."
  (let ((ta (ta-make "abc")))
    (tui.textarea:textarea-focus ta)
    (ta-set-cursor ta 0 3)
    (ta-key ta #\t +mod-ctrl+)
    (is (string= "acb" (tui.textarea:textarea-value ta)))))

(test textarea-key-alt-u-uppercases
  "Alt+u dispatched through update uppercases a word."
  (let ((ta (ta-make "hello")))
    (tui.textarea:textarea-focus ta)
    (ta-key ta #\u +mod-alt+)
    (is (string= "HELLO" (tui.textarea:textarea-value ta)))))

;;; --- char-limit / max-lines enforcement ---

(test textarea-char-limit-trims-insert
  "char-limit trims inserts and then blocks further input."
  (let ((ta (tui.textarea:make-textarea :char-limit 5)))
    (tui.textarea:textarea-insert-string ta "abc")       ; 3 runes
    (tui.textarea:textarea-insert-string ta "defgh")     ; only 2 more fit
    (is (= 5 (tui.textarea:textarea-length ta)))
    (is (string= "abcde" (tui.textarea:textarea-value ta)))
    (tui.textarea:textarea-insert-string ta "xyz")       ; fully blocked
    (is (= 5 (tui.textarea:textarea-length ta)))
    (is (string= "abcde" (tui.textarea:textarea-value ta)))))

(test textarea-max-lines-blocks-newline
  "max-lines blocks newlines once the line budget is exhausted."
  (let ((ta (tui.textarea:make-textarea :max-lines 3)))
    (tui.textarea:textarea-insert-string ta "one")
    (tui.textarea::textarea-newline ta)
    (tui.textarea:textarea-insert-string ta "two")
    (tui.textarea::textarea-newline ta)
    (tui.textarea:textarea-insert-string ta "three")
    (is (= 3 (tui.textarea:textarea-line-count ta)))
    (tui.textarea::textarea-newline ta)                  ; would be a 4th line
    (is (= 3 (tui.textarea:textarea-line-count ta)))
    (is (string= (format nil "one~%two~%three")
                 (tui.textarea:textarea-value ta)))))

(test textarea-max-lines-trims-multiline-paste
  "A multi-line insert is trimmed to the remaining line budget."
  (let ((ta (tui.textarea:make-textarea :max-lines 2)))
    (tui.textarea:textarea-insert-string ta (format nil "a~%b~%c~%d"))
    (is (= 2 (tui.textarea:textarea-line-count ta)))
    (is (string= (format nil "a~%b") (tui.textarea:textarea-value ta)))))

(test textarea-paste-inserts-text
  "A paste message inserts its text."
  (let ((ta (tui.textarea:make-textarea)))
    (tui.textarea:textarea-focus ta)
    (tui.textarea:textarea-update ta (make-paste-msg :text "pasted"))
    (is (string= "pasted" (tui.textarea:textarea-value ta)))))

;;; --- scroll offset / keep-cursor-in-view (#840) ---

(test textarea-scroll-follows-cursor
  "ensure-visible advances the offset so the cursor row is on screen."
  (let ((ta (tui.textarea:make-textarea :height 3 :show-line-numbers nil)))
    (tui.textarea:textarea-set-value ta (format nil "l0~%l1~%l2~%l3~%l4~%l5"))
    (is (= 0 (tui.textarea:textarea-scroll-position ta)))
    (dotimes (i 5) (tui.textarea::textarea-cursor-down ta))
    (tui.textarea::textarea-ensure-visible ta)
    (is (= 5 (tui.textarea::textarea-row ta)))
    (is (= 3 (tui.textarea:textarea-scroll-position ta))))) ; row 5, height 3

(test textarea-scroll-clamps-to-content
  "The offset never scrolls past the last page of content."
  (let ((ta (tui.textarea:make-textarea :height 4 :show-line-numbers nil)))
    (tui.textarea:textarea-set-value ta (format nil "l0~%l1~%l2")) ; 3 lines
    (ta-set-cursor ta 2 0)
    (tui.textarea::textarea-ensure-visible ta)
    (tui.textarea::textarea-clamp-yoffset ta)
    (is (= 0 (tui.textarea:textarea-scroll-position ta)))))       ; nothing to scroll

(test textarea-scroll-percent
  "scroll-percent reports progress through the document."
  (let ((ta (tui.textarea:make-textarea :height 3 :show-line-numbers nil)))
    (tui.textarea:textarea-set-value ta (format nil "l0~%l1~%l2~%l3~%l4~%l5"))
    (ta-set-cursor ta 5 0)
    (tui.textarea::textarea-ensure-visible ta)
    (is (= 1.0 (tui.textarea:textarea-scroll-percent ta)))
    (ta-set-cursor ta 0 0)
    (tui.textarea::textarea-ensure-visible ta)
    (is (= 0.0 (tui.textarea:textarea-scroll-percent ta)))))

(test textarea-view-renders-scrolled-window
  "View renders the window around the cursor, not always from line 0."
  (let ((ta (tui.textarea:make-textarea :height 2 :show-line-numbers nil :prompt "")))
    (tui.textarea:textarea-set-value ta (format nil "A~%B~%C~%D~%E"))
    (ta-set-cursor ta 4 0)                          ; row 4 ("E")
    (let ((out (tui.textarea:textarea-view ta)))
      (is (search "D" out))
      (is (search "E" out))
      (is (not (search "A" out))))))

;;; --- paging (#844) ---

(test textarea-page-down-then-up
  "Page-down moves by a viewport height and stays visible; page-up reverses."
  (let ((ta (tui.textarea:make-textarea :height 3 :show-line-numbers nil)))
    (tui.textarea:textarea-set-value ta
                                     (format nil "l0~%l1~%l2~%l3~%l4~%l5~%l6"))
    (tui.textarea:textarea-page-down ta)
    (is (= 3 (tui.textarea::textarea-row ta)))
    (tui.textarea::textarea-ensure-visible ta)
    (is (= 1 (tui.textarea:textarea-scroll-position ta)))
    (tui.textarea:textarea-page-up ta)
    (is (= 0 (tui.textarea::textarea-row ta)))))

(test textarea-key-page-down
  "Page-down dispatched through update advances cursor and offset."
  (let ((ta (tui.textarea:make-textarea :height 3 :show-line-numbers nil)))
    (tui.textarea:textarea-set-value ta
                                     (format nil "l0~%l1~%l2~%l3~%l4~%l5~%l6"))
    (tui.textarea:textarea-focus ta)
    (ta-key ta :page-down)
    (is (= 3 (tui.textarea::textarea-row ta)))
    (is (= 1 (tui.textarea:textarea-scroll-position ta)))))

;;; --- soft-wrapping ---

(defun ta-wrap (value &rest args)
  "Make a soft-wrapping textarea (no line numbers, no prompt) with VALUE."
  (apply #'ta-make value :soft-wrap t :show-line-numbers nil :prompt "" args))

(test textarea-soft-wrap-counts-visual-lines
  "A line wider than WIDTH occupies multiple visual lines."
  (let ((ta (tui.textarea:make-textarea :width 5 :height 10 :soft-wrap t
                                        :show-line-numbers nil :prompt "")))
    (tui.textarea:textarea-set-value ta "abcdefghij")     ; 10 chars at width 5
    (is (= 2 (tui.textarea:textarea-visual-line-count ta)))))

(test textarea-soft-wrap-empty-line
  "An empty line still counts as one visual line."
  (let ((ta (tui.textarea:make-textarea :width 5 :height 10 :soft-wrap t
                                        :show-line-numbers nil :prompt "")))
    (tui.textarea:textarea-set-value ta "")
    (is (= 1 (tui.textarea:textarea-visual-line-count ta)))))

(test textarea-no-wrap-counts-logical-lines
  "Without soft-wrap, visual-line-count equals the logical line count."
  (let ((ta (tui.textarea:make-textarea :width 5 :height 10
                                        :show-line-numbers nil :prompt "")))
    (tui.textarea:textarea-set-value ta "abcdefghij")
    (is (= 1 (tui.textarea:textarea-visual-line-count ta)))))

(test textarea-soft-wrap-renders-segments
  "View renders a long line as consecutive width-bounded segments."
  (let ((ta (tui.textarea:make-textarea :width 5 :height 5 :soft-wrap t
                                        :show-line-numbers nil :prompt "")))
    (tui.textarea:textarea-set-value ta "abcdefghij")
    (let ((out (tui.textarea:textarea-view ta)))
      (is (search "abcde" out))
      (is (search "fghij" out)))))

(test textarea-soft-wrap-cursor-on-segment
  "The cursor highlight lands on the wrapped segment that contains it."
  (let ((ta (tui.textarea:make-textarea :width 5 :height 5 :soft-wrap t
                                        :show-line-numbers nil :prompt "")))
    (tui.textarea:textarea-set-value ta "abcdefghij")
    (tui.textarea:textarea-focus ta)
    (ta-set-cursor ta 0 7)                                ; on 'h', in 2nd segment
    (let ((out (tui.textarea:textarea-view ta)))
      (is (search "abcde" out))
      (is (search "fg[h]ij" out)))))

(test textarea-soft-wrap-scroll-follows-cursor
  "Scrolling tracks the cursor in visual lines when soft-wrapping."
  (let ((ta (tui.textarea:make-textarea :width 5 :height 3 :soft-wrap t
                                        :show-line-numbers nil :prompt "")))
    ;; 4 logical lines of 10 chars each -> 8 visual lines at width 5.
    (tui.textarea:textarea-set-value ta
                                     (format nil "aaaaaaaaaa~%bbbbbbbbbb~%cccccccccc~%dddddddddd"))
    (ta-set-cursor ta 3 0)                                ; first segment of row 3 -> vi 6
    (tui.textarea::textarea-ensure-visible ta)
    (is (= 4 (tui.textarea:textarea-scroll-position ta)))))

(test textarea-soft-wrap-scroll-percent
  "scroll-percent reaches 1.0 at the bottom of wrapped content."
  (let ((ta (tui.textarea:make-textarea :width 5 :height 3 :soft-wrap t
                                        :show-line-numbers nil :prompt "")))
    (tui.textarea:textarea-set-value ta
                                     (format nil "aaaaaaaaaa~%bbbbbbbbbb~%cccccccccc~%dddddddddd"))
    (ta-set-cursor ta 3 10)                               ; very end -> last visual line
    (tui.textarea::textarea-ensure-visible ta)
    (is (= 1.0 (tui.textarea:textarea-scroll-percent ta)))))

;;; --- dynamic height (#910) ---

(test textarea-dynamic-height-grows-to-content
  "Dynamic height grows the viewport to fit the content."
  (let ((ta (tui.textarea:make-textarea :dynamic-height t :min-height 1 :max-height 0
                                        :show-line-numbers nil :prompt "")))
    (tui.textarea:textarea-set-value ta (format nil "a~%b~%c"))
    (tui.textarea:textarea-recalculate-height ta)
    (is (= 3 (tui.textarea:textarea-height ta)))))

(test textarea-dynamic-height-respects-min
  "Dynamic height never drops below min-height."
  (let ((ta (tui.textarea:make-textarea :dynamic-height t :min-height 4 :max-height 0
                                        :show-line-numbers nil :prompt "")))
    (tui.textarea:textarea-set-value ta "a")
    (tui.textarea:textarea-recalculate-height ta)
    (is (= 4 (tui.textarea:textarea-height ta)))))

(test textarea-dynamic-height-respects-max
  "Dynamic height is capped at max-height."
  (let ((ta (tui.textarea:make-textarea :dynamic-height t :min-height 1 :max-height 5
                                        :show-line-numbers nil :prompt "")))
    (tui.textarea:textarea-set-value ta (format nil "1~%2~%3~%4~%5~%6~%7~%8~%9~%10"))
    (tui.textarea:textarea-recalculate-height ta)
    (is (= 5 (tui.textarea:textarea-height ta)))))

(test textarea-dynamic-height-counts-visual-lines
  "Dynamic height counts visual lines when soft-wrap is on."
  (let ((ta (tui.textarea:make-textarea :width 5 :dynamic-height t :min-height 1 :max-height 0
                                        :soft-wrap t :show-line-numbers nil :prompt "")))
    (tui.textarea:textarea-set-value ta "abcdefghij")     ; 2 visual lines at width 5
    (tui.textarea:textarea-recalculate-height ta)
    (is (= 2 (tui.textarea:textarea-height ta)))))

(test textarea-dynamic-height-on-update
  "Typing a newline recalculates the viewport height through update."
  (let ((ta (tui.textarea:make-textarea :dynamic-height t :min-height 1 :max-height 0
                                        :show-line-numbers nil :prompt "")))
    (tui.textarea:textarea-focus ta)
    (ta-key ta :enter)                                   ; empty -> 2 lines
    (is (= 2 (tui.textarea:textarea-height ta)))))

;;; --- regression: pre-existing behaviour still works ---

(test textarea-backspace-merges-lines
  "Backspace at the start of a line merges it with the previous line."
  (let ((ta (ta-make (format nil "ab~%cd"))))
    (ta-set-cursor ta 1 0)
    (tui.textarea::textarea-delete-char-backward ta)
    (is (= 1 (tui.textarea:textarea-line-count ta)))
    (is (string= "abcd" (tui.textarea:textarea-value ta)))))

(test textarea-ignores-input-when-blurred
  "A blurred textarea ignores key messages."
  (let ((ta (ta-make "abc")))
    (ta-key ta :backspace)
    (is (string= "abc" (tui.textarea:textarea-value ta)))))
