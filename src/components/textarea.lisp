;;; components/textarea.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Textarea component for multi-line text editing

(defpackage #:tuition.components.textarea
  (:use #:cl)
  (:nicknames #:tui.textarea)
  (:export
   ;; Textarea creation
   #:textarea
   #:make-textarea

   ;; Accessors
   #:textarea-width
   #:textarea-height
   #:textarea-value
   #:textarea-placeholder
   #:textarea-focused
   #:textarea-show-line-numbers
   #:textarea-prompt
   #:textarea-char-limit
   #:textarea-max-lines
   #:textarea-yoffset
   #:textarea-soft-wrap
   #:textarea-dynamic-height
   #:textarea-min-height
   #:textarea-max-height
   #:textarea-max-content-height
   #:textarea-at-content-limit-p
   #:textarea-line-numbers

   ;; Operations
   #:textarea-init
   #:textarea-update
   #:textarea-view
   #:textarea-focus
   #:textarea-blur
   #:textarea-reset
   #:textarea-insert-string
   #:textarea-insert-rune
   #:textarea-set-value

   ;; Scroll
   #:textarea-scroll-position
   #:textarea-scroll-percent
   #:textarea-page-up
   #:textarea-page-down
   #:textarea-visual-line-count
   #:textarea-recalculate-height

   ;; Cursor operations
   #:textarea-cursor-position
   #:textarea-line
   #:textarea-column
   #:textarea-line-count
   #:textarea-length
   #:textarea-move-to-begin
   #:textarea-move-to-end
   #:textarea-cursor-word-backward
   #:textarea-cursor-word-forward
   #:textarea-delete-word-backward
   #:textarea-delete-word-forward
   #:textarea-word
   #:textarea-transpose-chars
   #:textarea-capitalize-word
   #:textarea-lowercase-word
   #:textarea-uppercase-word

   ;; Selection (ports bubbles #1029)
   #:textarea-selection-style
   #:textarea-position-at
   #:textarea-begin-selection
   #:textarea-extend-selection
   #:textarea-end-selection
   #:textarea-select-all
   #:textarea-clear-selection
   #:textarea-has-selection-p
   #:textarea-selection
   #:textarea-selected-text
   #:textarea-delete-selection
   #:textarea-copy-selection))

(in-package #:tuition.components.textarea)

;;; Textarea model
(defclass textarea ()
  ((width :initarg :width :accessor textarea-width
          :initform 40
          :documentation "Width of the textarea")
   (height :initarg :height :accessor textarea-height
           :initform 6
           :documentation "Height of the textarea")
   (lines :initform (vector "") :accessor textarea-lines
          :documentation "Content lines (vector of strings)")
   (row :initform 0 :accessor textarea-row
        :documentation "Current cursor row")
   (col :initform 0 :accessor textarea-col
        :documentation "Current cursor column")
   (focused :initform nil :accessor textarea-focused
            :documentation "Whether the textarea has focus")
   (placeholder :initarg :placeholder :accessor textarea-placeholder
                :initform ""
                :documentation "Placeholder text when empty")
   (show-line-numbers :initarg :show-line-numbers
                      :accessor textarea-show-line-numbers
                      :initform t
                      :documentation "Whether to show line numbers")
   (prompt :initarg :prompt :accessor textarea-prompt
           :initform "> "
           :documentation "Prompt string for each line")
   (char-limit :initarg :char-limit :accessor textarea-char-limit
               :initform 0
               :documentation "Maximum character count (0 = unlimited)")
   (max-lines :initarg :max-lines :accessor textarea-max-lines
              :initform 1000
              :documentation "Maximum number of lines")
   (cursor-blink :initform t :accessor textarea-cursor-blink
                 :documentation "Whether cursor is visible (for blinking)")
   (yoffset :initform 0 :accessor textarea-yoffset
            :documentation "Vertical scroll offset: index of the top visible line")
   (soft-wrap :initarg :soft-wrap :accessor textarea-soft-wrap
              :initform nil
              :documentation "When true, long lines soft-wrap to WIDTH display columns (visual lines)")
   (dynamic-height :initarg :dynamic-height :accessor textarea-dynamic-height
                   :initform nil
                   :documentation "When true, the viewport height grows/shrinks to fit the content")
   (min-height :initarg :min-height :accessor textarea-min-height
               :initform 1
               :documentation "Minimum viewport height when DYNAMIC-HEIGHT is on")
   (max-height :initarg :max-height :accessor textarea-max-height
               :initform 0
               :documentation "Maximum viewport height cap when DYNAMIC-HEIGHT is on (0 = no cap)")
   (max-content-height :initarg :max-content-height :accessor textarea-max-content-height
                       :initform 0
                       :documentation "Maximum content height in visual (wrapped) rows.  When
set (> 0), inserts and newlines are blocked once the content reaches this many
visual lines (0 = unlimited).")
   (sel-anchor :initform nil :accessor textarea-sel-anchor
               :documentation "Selection anchor as (row . col), or nil when no
selection is set.  The anchor is where the selection began; either end may
sort before the other -- use TEXTAREA-SELECTION for a normalized range.")
   (sel-head :initform nil :accessor textarea-sel-head
             :documentation "Selection head as (row . col), or nil.  The head
follows the cursor/pointer as the selection extends.")
   (selecting :initform nil :accessor textarea-selecting
              :documentation "Whether a pointer drag is in progress")
   (selection-style :initarg :selection-style :accessor textarea-selection-style
                    :initform (tuition:make-style :reverse t)
                    :documentation "Style applied to selected text"))
  (:documentation "A multi-line text input component."))

(defun make-textarea (&key (width 40) (height 6) (placeholder "")
                        (char-limit 0) (max-lines 1000)
                        (show-line-numbers t) (prompt "> ")
                        soft-wrap dynamic-height (min-height 1) (max-height 0)
                        (max-content-height 0))
  "Create a new textarea with the given dimensions."
  (make-instance 'textarea
                 :width width :height height
                 :char-limit char-limit :max-lines max-lines
                 :show-line-numbers show-line-numbers :prompt prompt
                 :placeholder placeholder
                 :soft-wrap soft-wrap :dynamic-height dynamic-height
                 :min-height min-height :max-height max-height
                 :max-content-height max-content-height))

;;; Content management

(defun textarea-set-value (textarea text)
  "Set the textarea's content and reset cursor/scroll to the top."
  (let* ((parts (tuition:split-string-by-newline text))
         (parts (if (null parts) (list "") parts))
         (n (length parts)))
    ;; Build an adjustable vector with a fill pointer so that the
    ;; merge/shrink paths (delete-char-backward/forward) can adjust-array it.
    (setf (textarea-lines textarea)
          (make-array n :initial-contents parts :adjustable t :fill-pointer n)))
  (setf (textarea-row textarea) 0
        (textarea-col textarea) 0
        (textarea-yoffset textarea) 0)
  (textarea-clear-selection textarea)
  textarea)

(defun textarea-value (textarea)
  "Get the textarea's full content as a string."
  (let ((lines (textarea-lines textarea)))
    (if (zerop (length lines))
        ""
        (format nil "~{~A~^~%~}" (coerce lines 'list)))))

(defun textarea-insert-string (textarea str)
  "Insert a string at the cursor position, respecting char-limit and max-lines."
  (let ((insert (%ta-fit-to-limits textarea str)))
    (when (string= insert "")
      (return-from textarea-insert-string textarea))
    (let* ((lines (textarea-lines textarea))
           (row (textarea-row textarea))
           (col (textarea-col textarea))
           (current-line (aref lines row))
           (new-lines (tuition:split-string-by-newline insert)))
      (if (= (length new-lines) 1)
          ;; Single line insert
          (let ((new-line (concatenate 'string
                                       (subseq current-line 0 col)
                                       (first new-lines)
                                       (subseq current-line col))))
            (setf (aref lines row) new-line)
            (setf (textarea-col textarea)
                  (+ col (length (first new-lines)))))
          ;; Multi-line insert
          (let* ((first-part (concatenate 'string
                                          (subseq current-line 0 col)
                                          (first new-lines)))
                 (last-part (concatenate 'string
                                         (car (last new-lines))
                                         (subseq current-line col)))
                 (middle-parts (subseq new-lines 1 (1- (length new-lines))))
                 (total-new-lines (1- (length new-lines))))
            ;; Update the current line with the first part
            (setf (aref lines row) first-part)
            ;; Insert new lines
            (let ((new-vector (make-array (+ (length lines) total-new-lines)
                                          :fill-pointer (+ (length lines) total-new-lines)
                                          :adjustable t)))
              ;; Copy lines before insertion point
              (loop for i from 0 below row
                    do (setf (aref new-vector i) (aref lines i)))
              ;; Add first line (already set above)
              (setf (aref new-vector row) first-part)
              ;; Add middle lines
              (loop for line in middle-parts
                    for i from (1+ row)
                    do (setf (aref new-vector i) line))
              ;; Add last line
              (setf (aref new-vector (+ row total-new-lines)) last-part)
              ;; Copy remaining lines
              (loop for i from (1+ row) below (length lines)
                    do (setf (aref new-vector (+ i total-new-lines))
                             (aref lines i)))
              (setf (textarea-lines textarea) new-vector)
              (setf (textarea-row textarea) (+ row total-new-lines))
              (setf (textarea-col textarea) (length (car (last new-lines)))))))
      textarea)))

(defun textarea-insert-rune (textarea char)
  "Insert a single character at the cursor position."
  (textarea-insert-string textarea (string char)))

(defun textarea-length (textarea)
  "Get the total character count."
  (let ((lines (textarea-lines textarea)))
    (loop for line across lines
          sum (length line))))

(defun textarea-line-count (textarea)
  "Get the number of lines."
  (length (textarea-lines textarea)))

(defun textarea-cursor-position (textarea)
  "Get cursor position as (row col)."
  (values (textarea-row textarea) (textarea-col textarea)))

(defun textarea-line (textarea)
  "Return the zero-indexed logical line the cursor is currently on."
  (textarea-row textarea))

(defun textarea-column (textarea)
  "Return the zero-indexed column (character offset) of the cursor on its
current logical line."
  (textarea-col textarea))

(defun textarea-reset (textarea)
  "Reset the textarea to empty state."
  (setf (textarea-lines textarea) (vector "")
        (textarea-row textarea) 0
        (textarea-col textarea) 0
        (textarea-yoffset textarea) 0)
  (textarea-clear-selection textarea)
  textarea)

(defun textarea-focus (textarea)
  "Give focus to the textarea."
  (setf (textarea-focused textarea) t)
  textarea)

(defun textarea-blur (textarea)
  "Remove focus from the textarea."
  (setf (textarea-focused textarea) nil)
  textarea)

;;; Helper functions

(defun textarea-current-line (textarea)
  "Get the current line text."
  (aref (textarea-lines textarea) (textarea-row textarea)))

(defun textarea-set-cursor (textarea col)
  "Set cursor column, clamping to line length."
  (let* ((lines (textarea-lines textarea))
         (row (textarea-row textarea))
         (line (aref lines row)))
    (setf (textarea-col textarea)
          (max 0 (min col (length line))))))

(defun textarea-cursor-start (textarea)
  "Move cursor to start of line."
  (setf (textarea-col textarea) 0)
  textarea)

(defun textarea-cursor-end (textarea)
  "Move cursor to end of line."
  (textarea-set-cursor textarea (length (textarea-current-line textarea)))
  textarea)

(defun textarea-cursor-up (textarea)
  "Move cursor up one line."
  (when (> (textarea-row textarea) 0)
    (decf (textarea-row textarea))
    (textarea-set-cursor textarea (textarea-col textarea)))
  textarea)

(defun textarea-cursor-down (textarea)
  "Move cursor down one line."
  (when (< (textarea-row textarea) (1- (textarea-line-count textarea)))
    (incf (textarea-row textarea))
    (textarea-set-cursor textarea (textarea-col textarea)))
  textarea)

(defun textarea-cursor-left (textarea)
  "Move cursor left one character."
  (if (> (textarea-col textarea) 0)
      (decf (textarea-col textarea))
      (when (> (textarea-row textarea) 0)
        (decf (textarea-row textarea))
        (textarea-cursor-end textarea)))
  textarea)

(defun textarea-cursor-right (textarea)
  "Move cursor right one character."
  (let ((line-len (length (textarea-current-line textarea))))
    (if (< (textarea-col textarea) line-len)
        (incf (textarea-col textarea))
        (when (< (textarea-row textarea) (1- (textarea-line-count textarea)))
          (incf (textarea-row textarea))
          (setf (textarea-col textarea) 0))))
  textarea)

(defun textarea-delete-char-backward (textarea)
  "Delete character before cursor (backspace)."
  (let ((lines (textarea-lines textarea))
        (row (textarea-row textarea))
        (col (textarea-col textarea)))
    (cond
      ;; At start of line - merge with previous line
      ((and (zerop col) (> row 0))
       (let* ((prev-line (aref lines (1- row)))
              (curr-line (aref lines row))
              (merged (concatenate 'string prev-line curr-line))
              (new-col (length prev-line)))
         ;; Set merged line
         (setf (aref lines (1- row)) merged)
         ;; Shift lines up
         (loop for i from row below (1- (length lines))
               do (setf (aref lines i) (aref lines (1+ i))))
         ;; Resize vector
         (setf (textarea-lines textarea)
               (adjust-array lines (1- (length lines)) :fill-pointer (1- (length lines))))
         ;; Move cursor
         (setf (textarea-row textarea) (1- row)
               (textarea-col textarea) new-col)))

      ;; In middle of line - delete character
      ((> col 0)
       (let* ((line (aref lines row))
              (new-line (concatenate 'string
                                     (subseq line 0 (1- col))
                                     (subseq line col))))
         (setf (aref lines row) new-line)
         (decf (textarea-col textarea))))))
  textarea)

(defun textarea-delete-char-forward (textarea)
  "Delete character at cursor (delete)."
  (let ((lines (textarea-lines textarea))
        (row (textarea-row textarea))
        (col (textarea-col textarea)))
    (cond
      ;; At end of line - merge with next line
      ((and (= col (length (aref lines row)))
            (< row (1- (length lines))))
       (let* ((curr-line (aref lines row))
              (next-line (aref lines (1+ row)))
              (merged (concatenate 'string curr-line next-line)))
         ;; Set merged line
         (setf (aref lines row) merged)
         ;; Shift lines up
         (loop for i from (1+ row) below (1- (length lines))
               do (setf (aref lines i) (aref lines (1+ i))))
         ;; Resize vector
         (setf (textarea-lines textarea)
               (adjust-array lines (1- (length lines)) :fill-pointer (1- (length lines))))))

      ;; In middle of line - delete character
      ((< col (length (aref lines row)))
       (let* ((line (aref lines row))
              (new-line (concatenate 'string
                                     (subseq line 0 col)
                                     (subseq line (1+ col)))))
         (setf (aref lines row) new-line)))))
  textarea)

(defun textarea-newline (textarea)
  "Insert a newline at cursor position, respecting max-lines and
max-content-height."
  (let ((max-lines (textarea-max-lines textarea)))
    (when (and (plusp max-lines)
               (>= (textarea-line-count textarea) max-lines))
      (return-from textarea-newline textarea)))
  (when (textarea-at-content-limit-p textarea)
    (return-from textarea-newline textarea))
  (let* ((lines (textarea-lines textarea))
         (row (textarea-row textarea))
         (col (textarea-col textarea))
         (line (aref lines row))
         (before (subseq line 0 col))
         (after (subseq line col)))

    ;; Create new vector with one more line
    (let ((new-lines (make-array (1+ (length lines))
                                 :fill-pointer (1+ (length lines))
                                 :adjustable t)))
      ;; Copy lines before split
      (loop for i from 0 below row
            do (setf (aref new-lines i) (aref lines i)))

      ;; Add split lines
      (setf (aref new-lines row) before
            (aref new-lines (1+ row)) after)

      ;; Copy lines after split
      (loop for i from (1+ row) below (length lines)
            do (setf (aref new-lines (1+ i)) (aref lines i)))

      (setf (textarea-lines textarea) new-lines)
      (setf (textarea-row textarea) (1+ row)
            (textarea-col textarea) 0)))
  textarea)

;;; Word helpers.  A "word char" is alphanumeric, matching textinput's
;;; %ti-prev/next-word-boundary convention so both inputs behave the same.

(defun %ta-word-char-p (ch)
  "Non-nil if CH is part of a word (alphanumeric)."
  (alphanumericp ch))

(defun %ta-prev-word-boundary (s pos)
  "Return the index of the start of the word at or before POS in S.
Whitespace is skipped, so repeated calls walk backwards word by word."
  (let ((i (min (max 0 pos) (length s))))
    ;; Skip non-word chars to the left.
    (loop while (and (> i 0) (not (%ta-word-char-p (char s (1- i)))))
          do (decf i))
    ;; Skip word chars to the left.
    (loop while (and (> i 0) (%ta-word-char-p (char s (1- i))))
          do (decf i))
    i))

(defun %ta-next-word-boundary (s pos)
  "Return the index of the start of the next word after POS in S, or the end.
If POS is inside a word, first moves past that word, then skips whitespace."
  (let ((i (min (max 0 pos) (length s))) (n (length s)))
    ;; Skip the current word.
    (loop while (and (< i n) (%ta-word-char-p (char s i))) do (incf i))
    ;; Skip following whitespace.
    (loop while (and (< i n) (not (%ta-word-char-p (char s i)))) do (incf i))
    i))

(defun %ta-word-at (line col)
  "Return the alphanumeric word containing position COL in LINE, or empty."
  (cond
    ((or (minusp col) (>= col (length line))) "")
    ((not (%ta-word-char-p (char line col))) "")
    (t (let ((start col) (end col))
         (loop while (and (> start 0) (%ta-word-char-p (char line (1- start))))
               do (decf start))
         (loop while (and (< end (length line)) (%ta-word-char-p (char line end)))
               do (incf end))
         (subseq line start end)))))

(defun textarea-word (textarea)
  "Return the word under the cursor: the run of word-chars touching the
character immediately left of the cursor.  Empty when not on a word."
  (let* ((lines (textarea-lines textarea))
         (row (textarea-row textarea))
         (line (if (< row (length lines)) (aref lines row) "")))
    (%ta-word-at line (1- (textarea-col textarea)))))

(defun textarea-cursor-word-backward (textarea)
  "Move the cursor to the start of the previous word, crossing line
boundaries like upstream bubbles wordLeft, and stopping at the start of
the input (bubbles #1036)."
  (loop
    (let* ((line (textarea-current-line textarea))
           (col (textarea-col textarea))
           (b (%ta-prev-word-boundary line col)))
      (cond
        ;; A word starts strictly left of the cursor on this line.
        ((and (< b col) (< b (length line)) (%ta-word-char-p (char line b)))
         (setf (textarea-col textarea) b)
         (return))
        ;; No word to the left: hop to the end of the previous line.
        ((> (textarea-row textarea) 0)
         (decf (textarea-row textarea))
         (setf (textarea-col textarea)
               (length (textarea-current-line textarea))))
        ;; Start of the input.
        (t
         (setf (textarea-col textarea) 0)
         (return)))))
  textarea)

(defun textarea-cursor-word-forward (textarea)
  "Move the cursor past the current word, crossing line boundaries like
upstream bubbles wordRight, and stopping at the end of the input."
  (loop
    (let* ((line (textarea-current-line textarea))
           (col (textarea-col textarea))
           (f (%ta-next-word-boundary line col)))
      (cond
        ;; The cursor advances on this line, either onto the start of the
        ;; next word or past the last word to the end of the line.
        ((and (> f col)
              (or (< f (length line))
                  (position-if #'%ta-word-char-p line :start col)))
         (setf (textarea-col textarea) f)
         (return))
        ;; Rest of the line holds no word: hop to the next line start.
        ((< (textarea-row textarea) (1- (textarea-line-count textarea)))
         (incf (textarea-row textarea))
         (setf (textarea-col textarea) 0)
         ;; A word may start at column 0; landing there is the target.
         (let ((next (textarea-current-line textarea)))
           (when (and (plusp (length next)) (%ta-word-char-p (char next 0)))
             (return))))
        ;; End of the input.
        (t
         (setf (textarea-col textarea) (length line))
         (return)))))
  textarea)

(defun textarea-delete-word-backward (textarea)
  "Delete the word before the cursor."
  (let* ((lines (textarea-lines textarea))
         (row (textarea-row textarea))
         (line (aref lines row))
         (col (textarea-col textarea))
         (b (%ta-prev-word-boundary line col)))
    (when (and b (< b col))
      (setf (aref lines row)
            (concatenate 'string (subseq line 0 b) (subseq line col)))
      (setf (textarea-col textarea) b)))
  textarea)

(defun textarea-delete-word-forward (textarea)
  "Delete the word after the cursor."
  (let* ((lines (textarea-lines textarea))
         (row (textarea-row textarea))
         (line (aref lines row))
         (col (textarea-col textarea))
         (f (%ta-next-word-boundary line col)))
    (when (and f (> f col))
      (setf (aref lines row)
            (concatenate 'string (subseq line 0 col) (subseq line f)))))
  textarea)

;;; Character transpose and word-case commands (readline niceties)

(defun textarea-transpose-chars (textarea)
  "Transpose the characters around the cursor (readline Ctrl-T).
At end of line, swaps the last two characters; at the start, does nothing."
  (let* ((lines (textarea-lines textarea))
         (row (textarea-row textarea))
         (col (textarea-col textarea))
         (line (aref lines row))
         (len (length line)))
    (cond
      ((< len 2) textarea)                        ; nothing to swap
      ((>= col len)                               ; cursor at/past end
       (setf (aref lines row)
             (concatenate 'string
                           (subseq line 0 (- len 2))
                           (string (char line (1- len)))
                           (string (char line (- len 2)))))
       textarea)
      ((> col 0)                                  ; cursor mid-line
       (setf (aref lines row)
             (concatenate 'string
                           (subseq line 0 (1- col))
                           (string (char line col))
                           (string (char line (1- col)))
                           (subseq line (1+ col))))
       (incf (textarea-col textarea))
       textarea)
      (t textarea))))                             ; col 0: nothing before cursor

(defun %ta-skip-to-word (line col)
  "Return the index of the first word char at or after COL in LINE."
  (let ((i col) (n (length line)))
    (loop while (and (< i n) (not (%ta-word-char-p (char line i)))) do (incf i))
    i))

(defun %ta-word-end-after (line start)
  "Return the index just past the word that begins at START."
  (let ((i start) (n (length line)))
    (loop while (and (< i n) (%ta-word-char-p (char line i))) do (incf i))
    i))

(defun %ta-capitalize-string (s)
  "Upcase the first character of S and downcase the rest."
  (if (plusp (length s))
      (concatenate 'string
                   (string (char-upcase (char s 0)))
                   (string-downcase (subseq s 1)))
      s))

(defun %ta-case-word (textarea fn)
  "Apply FN (string->string) to the word at/after the cursor, then move past it."
  (let* ((lines (textarea-lines textarea))
         (row (textarea-row textarea))
         (line (aref lines row))
         (start (%ta-skip-to-word line (textarea-col textarea))))
    (when (< start (length line))
      (let* ((end (%ta-word-end-after line start))
             (cased (funcall fn (subseq line start end))))
        (setf (aref lines row)
              (concatenate 'string (subseq line 0 start) cased (subseq line end)))
        (setf (textarea-col textarea) end))))
  textarea)

(defun textarea-capitalize-word (textarea)
  "Capitalize the word at/after the cursor (Alt+c)."
  (%ta-case-word textarea #'%ta-capitalize-string))

(defun textarea-lowercase-word (textarea)
  "Lowercase the word at/after the cursor (Alt+l)."
  (%ta-case-word textarea #'string-downcase))

(defun textarea-uppercase-word (textarea)
  "Uppercase the word at/after the cursor (Alt+u)."
  (%ta-case-word textarea #'string-upcase))

;;; Document-level cursor movement

(defun textarea-move-to-begin (textarea)
  "Move the cursor to the very beginning of the text."
  (setf (textarea-row textarea) 0
        (textarea-col textarea) 0)
  textarea)

(defun textarea-move-to-end (textarea)
  "Move the cursor to the very end of the text."
  (let ((last (1- (textarea-line-count textarea))))
    (setf (textarea-row textarea) (max 0 last))
    (setf (textarea-col textarea)
          (length (textarea-current-line textarea))))
  textarea)

;;; Selection (ports the bubbles textarea selection feature, #1029).
;;; A selection is a pair of buffer positions -- an anchor where it began and
;;; a head that follows the cursor or pointer.  Positions are (row . col)
;;; conses; col may equal the line length, meaning "just past the last char".

(defun %ta-pos-before-p (a b)
  "True when position A=(row . col) sorts before B in the buffer."
  (or (< (car a) (car b))
      (and (= (car a) (car b)) (< (cdr a) (cdr b)))))

(defun textarea-has-selection-p (textarea)
  "True when a non-empty selection is active."
  (let ((a (textarea-sel-anchor textarea))
        (h (textarea-sel-head textarea)))
    (and a h (not (equal a h)))))

(defun textarea-selection (textarea)
  "Return the selected range as (VALUES START-ROW START-COL END-ROW END-COL),
normalized so start sorts before end, or NIL when nothing is selected."
  (when (textarea-has-selection-p textarea)
    (let ((a (textarea-sel-anchor textarea))
          (h (textarea-sel-head textarea)))
      (when (%ta-pos-before-p h a) (rotatef a h))
      (values (car a) (cdr a) (car h) (cdr h)))))

(defun textarea-clear-selection (textarea)
  "Remove the current selection, if any."
  (setf (textarea-sel-anchor textarea) nil
        (textarea-sel-head textarea) nil
        (textarea-selecting textarea) nil)
  textarea)

(defun textarea-select-all (textarea)
  "Select the entire buffer."
  (let ((last (1- (textarea-line-count textarea))))
    (when (>= last 0)
      (setf (textarea-sel-anchor textarea) (cons 0 0)
            (textarea-sel-head textarea)
            (cons last (length (aref (textarea-lines textarea) last)))
            (textarea-selecting textarea) nil)))
  textarea)

(defun textarea-selected-text (textarea)
  "Return the selected text, with lines joined by newlines.  Empty when
nothing is selected."
  (multiple-value-bind (sr sc er ec) (textarea-selection textarea)
    (if (null sr)
        ""
        (let ((lines (textarea-lines textarea)))
          (if (= sr er)
              (let ((line (aref lines sr)))
                (subseq line (min sc (length line)) (min ec (length line))))
              (with-output-to-string (s)
                (loop for row from sr to er
                      for line = (aref lines row)
                      do (cond
                           ((= row sr)
                            (write-string line s :start (min sc (length line))))
                           ((= row er)
                            (write-string line s :end (min ec (length line))))
                           (t (write-string line s)))
                         (when (< row er) (write-char #\Newline s)))))))))

(defun textarea-delete-selection (textarea)
  "Delete the selected text, put the cursor at the start of the former
selection, and clear the selection state.  A no-op when nothing is selected."
  (multiple-value-bind (sr sc er ec) (textarea-selection textarea)
    (when sr
      (let ((lines (textarea-lines textarea)))
        (if (= sr er)
            (let* ((line (aref lines sr))
                   (end-col (min ec (length line)))
                   (start-col (min sc end-col)))
              (setf (aref lines sr)
                    (concatenate 'string
                                 (subseq line 0 start-col)
                                 (subseq line end-col)))
              (setf (textarea-row textarea) sr)
              (textarea-set-cursor textarea start-col))
            (let* ((head-line (aref lines sr))
                   (tail-line (aref lines er))
                   (end-col (min ec (length tail-line)))
                   (start-col (min sc (length head-line)))
                   (merged (concatenate 'string
                                        (subseq head-line 0 start-col)
                                        (subseq tail-line end-col)))
                   (removed (- er sr))
                   (n (length lines))
                   (new-lines (make-array (- n removed)
                                          :adjustable t
                                          :fill-pointer (- n removed))))
              (loop for i from 0 below sr
                    do (setf (aref new-lines i) (aref lines i)))
              (setf (aref new-lines sr) merged)
              (loop for i from (1+ er) below n
                    do (setf (aref new-lines (- i removed)) (aref lines i)))
              (setf (textarea-lines textarea) new-lines)
              (setf (textarea-row textarea) sr)
              (textarea-set-cursor textarea start-col))))
      (textarea-clear-selection textarea)))
  textarea)

(defun textarea-copy-selection (textarea)
  "Return a command that copies the selected text to the system clipboard via
OSC 52, or NIL when nothing is selected."
  (when (textarea-has-selection-p textarea)
    (tuition:set-clipboard-cmd (textarea-selected-text textarea))))

(defun %ta-start-keyboard-selection (textarea)
  "Anchor a keyboard-driven selection at the cursor unless one is active."
  (unless (textarea-sel-anchor textarea)
    (setf (textarea-sel-anchor textarea)
          (cons (textarea-row textarea) (textarea-col textarea))))
  (setf (textarea-sel-head textarea)
        (cons (textarea-row textarea) (textarea-col textarea))))

(defun %ta-update-keyboard-selection (textarea)
  "Move the selection head to the cursor after a keyboard movement."
  (setf (textarea-sel-head textarea)
        (cons (textarea-row textarea) (textarea-col textarea))))

(defun %ta-gutter-width (textarea)
  "Columns rendered left of the text content: the prompt, plus the
line-number column when enabled.  Pointer coordinates are offset by this
much before being mapped to a column."
  (+ (tuition:visible-length (textarea-prompt textarea))
     (if (textarea-show-line-numbers textarea)
         ;; The view pads numbers to ln-width (digits + 2) plus one space.
         (+ 3 (length (write-to-string (textarea-line-count textarea))))
         0)))

(defun %ta-char-index-for-column (line start end target-col)
  "Index in LINE of the character occupying display column TARGET-COL within
the segment [START, END), accounting for wide characters.  A column past the
segment's width yields END."
  (let ((w 0))
    (loop for i from start below end
          for cw = (tuition:visible-length (string (char line i)))
          do (when (> (+ w cw) target-col)
               (return-from %ta-char-index-for-column i))
             (incf w cw))
    end))

(defun textarea-position-at (textarea x y)
  "Map coordinates within the textarea's rendered area to a buffer position,
returned as (VALUES ROW COL).  (0, 0) is the textarea's top-left cell,
including the prompt/line-number gutter; callers rendering the textarea at an
offset must subtract that offset first.  Coordinates outside the content
resolve to the nearest position."
  (let* ((lines (textarea-lines textarea))
         (target-line (+ y (textarea-yoffset textarea)))
         (content-x (max 0 (- x (%ta-gutter-width textarea)))))
    (when (or (zerop (length lines)) (< target-line 0))
      (return-from textarea-position-at (values 0 0)))
    (flet ((end-of-buffer ()
             (let ((last (1- (length lines))))
               (values last (length (aref lines last))))))
      (if (textarea-soft-wrap textarea)
          (let ((visuals (%ta-visual-lines textarea)))
            (if (< target-line (length visuals))
                (let* ((v (aref visuals target-line))
                       (row (first v)) (vstart (second v)) (vend (third v))
                       (line (aref lines row)))
                  (values row (%ta-char-index-for-column line vstart vend content-x)))
                (end-of-buffer)))
          (if (< target-line (length lines))
              (let ((line (aref lines target-line)))
                (values target-line
                        (%ta-char-index-for-column line 0 (length line) content-x)))
              (end-of-buffer))))))

(defun textarea-begin-selection (textarea x y)
  "Start a pointer selection at textarea-relative X, Y, discarding any
previous selection, and move the cursor there.  Pair with
TEXTAREA-EXTEND-SELECTION as the pointer moves and TEXTAREA-END-SELECTION
when the drag finishes.  See TEXTAREA-POSITION-AT for the coordinates."
  (multiple-value-bind (row col) (textarea-position-at textarea x y)
    (setf (textarea-sel-anchor textarea) (cons row col)
          (textarea-sel-head textarea) (cons row col)
          (textarea-selecting textarea) t)
    (setf (textarea-row textarea) row)
    (textarea-set-cursor textarea col))
  textarea)

(defun textarea-extend-selection (textarea x y)
  "Extend an in-progress pointer selection to X, Y and move the cursor there.
A no-op unless TEXTAREA-BEGIN-SELECTION started a drag."
  (when (textarea-selecting textarea)
    (multiple-value-bind (row col) (textarea-position-at textarea x y)
      (setf (textarea-sel-head textarea) (cons row col))
      (setf (textarea-row textarea) row)
      (textarea-set-cursor textarea col)))
  textarea)

(defun textarea-end-selection (textarea)
  "Complete an in-progress drag.  The selection itself is retained so it can
be read with TEXTAREA-SELECTED-TEXT; a zero-width selection (a plain click)
is discarded."
  (setf (textarea-selecting textarea) nil)
  (when (equal (textarea-sel-anchor textarea) (textarea-sel-head textarea))
    (textarea-clear-selection textarea))
  textarea)

(defun %ta-selection-span (textarea row start end)
  "Half-open char range of logical ROW selected within the segment
[START, END), returned as (VALUES FROM TO) relative to START, or NIL when the
segment holds no selected characters."
  (multiple-value-bind (sr sc er ec) (textarea-selection textarea)
    (when (and sr (<= sr row er))
      (let* ((line-len (length (aref (textarea-lines textarea) row)))
             (row-from (if (= row sr) (min sc line-len) 0))
             (row-to (if (= row er) (min ec line-len) line-len))
             (from (max row-from start))
             (to (min row-to end)))
        (when (< from to)
          (values (- from start) (- to start)))))))

;;; Soft-wrapping: mapping logical lines to visual (wrapped) lines.
;;; When SOFT-WRAP is on, the viewport, scroll offset, and cursor-in-view
;;; tracking all operate in visual lines (a logical line wider than WIDTH
;;; occupies several visual lines).

(defun %ta-wrap-line (line width)
  "Split LINE into char-offset segments, each no wider than WIDTH display
columns (greedy, character wrapping).  Returns a list of (start . end) conses."
  (let ((n (length line)))
    (cond
      ((<= width 0) (list (cons 0 n)))            ; no wrapping: whole line
      ((zerop n) (list (cons 0 0)))               ; empty line: one empty segment
      (t (let ((segments '()) (start 0))
           (loop while (< start n) do
             (let ((end start))
               (loop while (< end n) do
                 (if (> (tuition:visible-length (subseq line start (1+ end))) width)
                     (loop-finish)
                     (incf end)))
               ;; Force at least one character per segment so a single wide
               ;; character (wider than WIDTH) still makes progress.
               (when (= end start) (incf end))
               (push (cons start end) segments)
               (setf start end)))
           (nreverse segments))))))

(defun %ta-visual-lines (textarea)
  "Return a vector of visual lines, each a list (LOGICAL-ROW START END)."
  (let ((lines (textarea-lines textarea))
        (width (max 1 (textarea-width textarea)))
        (result '()))
    (loop for row from 0 below (length lines) do
      (dolist (seg (%ta-wrap-line (aref lines row) width))
        (push (list row (car seg) (cdr seg)) result)))
    (coerce (nreverse result) 'vector)))

(defun %ta-cursor-visual-index (textarea visuals)
  "Index into VISUALS of the segment containing the cursor (row, col)."
  (let ((row (textarea-row textarea))
        (col (textarea-col textarea)))
    (or (position-if (lambda (v)
                       (and (= (first v) row)
                            (<= (second v) col (third v))))
                     visuals)
        0)))

(defun textarea-visual-line-count (textarea)
  "Number of visual (wrapped) lines.  Equals the logical line count when
SOFT-WRAP is off."
  (if (textarea-soft-wrap textarea)
      (length (%ta-visual-lines textarea))
      (textarea-line-count textarea)))

(defun textarea-at-content-limit-p (textarea)
  "True when MAX-CONTENT-HEIGHT is set and the content has reached it (measured
in visual, wrap-aware rows)."
  (let ((cap (textarea-max-content-height textarea)))
    (and (plusp cap)
         (>= (textarea-visual-line-count textarea) cap))))

;;; Paging and vertical scroll offset (keep the cursor in view)

(defun %ta-cursor-line-number (textarea)
  "The cursor's line number in the same units as the scroll offset: the visual
line index when soft-wrapping, otherwise the logical row."
  (if (textarea-soft-wrap textarea)
      (%ta-cursor-visual-index textarea (%ta-visual-lines textarea))
      (textarea-row textarea)))

(defun %ta-visual-line-row (textarea vi)
  "The logical row containing visual line index VI (clamped to valid range)."
  (if (textarea-soft-wrap textarea)
      (let ((visuals (%ta-visual-lines textarea)))
        (if (plusp (length visuals))
            (first (aref visuals (max 0 (min vi (1- (length visuals))))))
            0))
      (max 0 (min vi (max 0 (1- (textarea-line-count textarea)))))))

(defun %ta-page-to (textarea target-line)
  "Move the cursor to visual line TARGET-LINE, preserving the column."
  (setf (textarea-row textarea) (%ta-visual-line-row textarea target-line))
  (textarea-set-cursor textarea (textarea-col textarea))
  textarea)

(defun textarea-page-up (textarea)
  "Move the cursor up by one page.  The first press snaps the cursor to the
first visible line; subsequent presses move up by a full viewport height."
  (let* ((h (max 1 (textarea-height textarea)))
         (yoff (textarea-yoffset textarea))
         (cur (%ta-cursor-line-number textarea)))
    (if (> cur yoff)
        (%ta-page-to textarea yoff)
        (%ta-page-to textarea (max 0 (- cur h)))))
  textarea)

(defun textarea-page-down (textarea)
  "Move the cursor down by one page.  The first press snaps the cursor to the
last visible line; subsequent presses move down by a full viewport height."
  (let* ((h (max 1 (textarea-height textarea)))
         (yoff (textarea-yoffset textarea))
         (last (max 0 (1- (textarea-visual-line-count textarea))))
         (cur (%ta-cursor-line-number textarea)))
    (if (< cur (+ yoff (1- h)))
        (%ta-page-to textarea (min last (+ yoff (1- h))))
        (%ta-page-to textarea (min last (+ cur h)))))
  textarea)

(defun textarea-recalculate-height (textarea)
  "When DYNAMIC-HEIGHT is on, fit the viewport height to the visual content,
clamped between MIN-HEIGHT and MAX-HEIGHT.  No-op otherwise."
  (when (textarea-dynamic-height textarea)
    (let* ((total (textarea-visual-line-count textarea))
           (h (max total (max 1 (textarea-min-height textarea))))
           (cap (textarea-max-height textarea)))
      (when (plusp cap) (setf h (min h cap)))
      (setf (textarea-height textarea) h)))
  textarea)

(defun textarea-ensure-visible (textarea)
  "Adjust the scroll offset so the cursor stays within the viewport.
In soft-wrap mode the cursor is tracked in visual (wrapped) lines; otherwise in
logical rows.  Also called from TEXTAREA-VIEW so the cursor can never scroll
out of sight regardless of how state was mutated."
  (let ((h (max 1 (textarea-height textarea)))
        (yoff (textarea-yoffset textarea)))
    (let ((cur (if (textarea-soft-wrap textarea)
                   (%ta-cursor-visual-index textarea (%ta-visual-lines textarea))
                   (textarea-row textarea))))
      (cond
        ((< cur yoff) (setf (textarea-yoffset textarea) cur))
        ((>= cur (+ yoff h))
         (setf (textarea-yoffset textarea) (1+ (- cur h)))))))
  textarea)

(defun textarea-clamp-yoffset (textarea)
  "Keep the scroll offset within [0, visual-line-count - height]."
  (let* ((total (textarea-visual-line-count textarea))
         (h (max 1 (textarea-height textarea)))
         (max-off (max 0 (- total h))))
    (when (< (textarea-yoffset textarea) 0)
      (setf (textarea-yoffset textarea) 0))
    (when (> (textarea-yoffset textarea) max-off)
      (setf (textarea-yoffset textarea) max-off)))
  textarea)

(defun textarea-scroll-position (textarea)
  "Return the index of the top visible line (the scroll offset)."
  (textarea-yoffset textarea))

(defun textarea-scroll-percent (textarea)
  "Return scroll progress through the content as a float in [0.0, 1.0]."
  (let ((n (textarea-visual-line-count textarea))
        (h (max 1 (textarea-height textarea))))
    (if (or (<= n 1) (<= n h))
        0.0
        (/ (float (textarea-yoffset textarea))
           (float (- n h))))))

;;; Input limits: char-limit (total runes) and max-lines

(defun %ta-truncate-to-newlines (str max-newlines)
  "Return STR cut so it contains at most MAX-NEWLINES #\\Newline characters."
  (let ((count 0))
    (loop for ch across str
          for idx from 0
          when (char= ch #\Newline)
            do (progn
                 (incf count)
                 (when (> count max-newlines)
                   (return-from %ta-truncate-to-newlines
                     (subseq str 0 idx))))))
  str)

(defun %ta-fit-to-limits (textarea str)
  "Trim STR so an insert respects char-limit (total runes) and max-lines."
  (let ((s str))
    ;; Character limit counts runes across all lines (see TEXTAREA-LENGTH).
    (let ((limit (textarea-char-limit textarea)))
      (when (plusp limit)
        (let ((avail (max 0 (- limit (textarea-length textarea)))))
          (when (> (length s) avail)
            (setf s (subseq s 0 avail))))))
    ;; Line limit: never create more than MAX-LINES lines.
    (let ((max-lines (textarea-max-lines textarea)))
      (when (plusp max-lines)
        (let ((allowed (max 0 (- max-lines (textarea-line-count textarea)))))
          (setf s (%ta-truncate-to-newlines s allowed)))))
    ;; Content-height limit: never grow past MAX-CONTENT-HEIGHT visual rows.
    ;; Approximated by capping the number of new logical lines the insert adds;
    ;; exact for the non-wrapping case.
    (let ((cap (textarea-max-content-height textarea)))
      (when (plusp cap)
        (let ((allowed (max 0 (- cap (textarea-visual-line-count textarea)))))
          (setf s (%ta-truncate-to-newlines s allowed)))))
    s))

;;; TEA protocol implementation

(defun textarea-init (textarea)
  "Initialize the textarea. Returns nil (no command)."
  (declare (ignore textarea))
  nil)

(defun textarea-update (textarea msg)
  "Update textarea with a message. Returns (values new-textarea cmd)."
  (unless (textarea-focused textarea)
    (return-from textarea-update (values textarea nil)))
  (multiple-value-bind (_ cmd) (%textarea-dispatch-key textarea msg)
    (declare (ignore _))
    ;; Grow/shrink the viewport to fit content (when dynamic-height), then
    ;; keep the cursor inside the viewport and the offset within bounds.
    (textarea-recalculate-height textarea)
    (textarea-ensure-visible textarea)
    (textarea-clamp-yoffset textarea)
    (values textarea cmd)))

(defun %textarea-dispatch-key (textarea msg)
  "Handle one message, returning (values textarea cmd).  Mutates TEXTAREA in
place.  Only called when the textarea is focused."
  ;; Bracketed paste replaces any active selection.
  (when (typep msg 'tuition:paste-msg)
    (textarea-delete-selection textarea)
    (textarea-insert-string textarea (tuition:paste-msg-text msg))
    (return-from %textarea-dispatch-key (values textarea nil)))
  (unless (tuition:key-press-msg-p msg)
    (return-from %textarea-dispatch-key (values textarea nil)))
  (let ((key (tuition:key-event-code msg))
        (alt (tuition:mod-contains (tuition:key-event-mod msg) tuition:+mod-alt+))
        (ctrl (tuition:mod-contains (tuition:key-event-mod msg) tuition:+mod-ctrl+))
        (shift (tuition:mod-contains (tuition:key-event-mod msg) tuition:+mod-shift+)))
    (flet ((select-via (mover)
             ;; Extend (or start) a keyboard selection across a movement.
             (%ta-start-keyboard-selection textarea)
             (funcall mover textarea)
             (%ta-update-keyboard-selection textarea)
             (values textarea nil)))
      (cond
        ;; Selection: shift-modified movement extends the selection.  The
        ;; Ctrl+Shift word variants must precede both the plain-shift and the
        ;; Ctrl-only clauses below.
        ((and ctrl shift (eq key :left))
         (select-via #'textarea-cursor-word-backward))
        ((and ctrl shift (eq key :right))
         (select-via #'textarea-cursor-word-forward))
        ((and shift (eq key :left))  (select-via #'textarea-cursor-left))
        ((and shift (eq key :right)) (select-via #'textarea-cursor-right))
        ((and shift (eq key :up))    (select-via #'textarea-cursor-up))
        ((and shift (eq key :down))  (select-via #'textarea-cursor-down))

        ;; Select all (Ctrl+G, matching upstream) and copy (Ctrl+Shift+C).
        ((and ctrl (characterp key) (char-equal key #\g))
         (values (textarea-select-all textarea) nil))
        ((and ctrl shift (characterp key) (char-equal key #\c))
         (values textarea (textarea-copy-selection textarea)))

        ;; Word movement (Ctrl+Left / Ctrl+Right) - must precede plain arrows
        ((and ctrl (eq key :left))
         (values (textarea-cursor-word-backward (textarea-clear-selection textarea)) nil))
        ((and ctrl (eq key :right))
         (values (textarea-cursor-word-forward (textarea-clear-selection textarea)) nil))

        ;; Arrow navigation
        ((eq key :up)    (values (textarea-cursor-up (textarea-clear-selection textarea)) nil))
        ((eq key :down)  (values (textarea-cursor-down (textarea-clear-selection textarea)) nil))
        ((eq key :left)  (values (textarea-cursor-left (textarea-clear-selection textarea)) nil))
        ((eq key :right) (values (textarea-cursor-right (textarea-clear-selection textarea)) nil))

        ;; Paging
        ((eq key :page-up)
         (values (textarea-page-up (textarea-clear-selection textarea)) nil))
        ((eq key :page-down)
         (values (textarea-page-down (textarea-clear-selection textarea)) nil))

        ;; Document-level begin/end (must precede line start/end below)
        ((and ctrl (eq key :home))
         (values (textarea-move-to-begin (textarea-clear-selection textarea)) nil))
        ((and ctrl (eq key :end))
         (values (textarea-move-to-end (textarea-clear-selection textarea)) nil))

        ;; Word movement (Alt+b / Alt+f)
        ((and alt (characterp key) (char= key #\b))
         (values (textarea-cursor-word-backward (textarea-clear-selection textarea)) nil))
        ((and alt (characterp key) (char= key #\f))
         (values (textarea-cursor-word-forward (textarea-clear-selection textarea)) nil))

        ;; Word deletion (Ctrl+w, Ctrl/Alt+Backspace, Ctrl+Delete, Alt+d).
        ;; With an active selection, deletion removes the selection instead.
        ((and ctrl (characterp key) (char= key #\w))
         (values (if (textarea-has-selection-p textarea)
                     (textarea-delete-selection textarea)
                     (textarea-delete-word-backward textarea))
                 nil))
        ((and (or ctrl alt) (eq key :backspace))
         (values (if (textarea-has-selection-p textarea)
                     (textarea-delete-selection textarea)
                     (textarea-delete-word-backward textarea))
                 nil))
        ((and ctrl (eq key :delete))
         (values (if (textarea-has-selection-p textarea)
                     (textarea-delete-selection textarea)
                     (textarea-delete-word-forward textarea))
                 nil))
        ((and alt (characterp key) (char= key #\d))
         (values (if (textarea-has-selection-p textarea)
                     (textarea-delete-selection textarea)
                     (textarea-delete-word-forward textarea))
                 nil))

        ;; Transpose (Ctrl+t) and word case (Alt+c / Alt+l / Alt+u)
        ((and ctrl (characterp key) (char= key #\t))
         (values (textarea-transpose-chars textarea) nil))
        ((and alt (characterp key) (char= key #\c))
         (values (textarea-capitalize-word textarea) nil))
        ((and alt (characterp key) (char= key #\l))
         (values (textarea-lowercase-word textarea) nil))
        ((and alt (characterp key) (char= key #\u))
         (values (textarea-uppercase-word textarea) nil))

        ;; Line start / end (Home, Ctrl+a / End, Ctrl+e)
        ((or (eq key :home) (and ctrl (characterp key) (char= key #\a)))
         (values (textarea-cursor-start (textarea-clear-selection textarea)) nil))
        ((or (eq key :end) (and ctrl (characterp key) (char= key #\e)))
         (values (textarea-cursor-end (textarea-clear-selection textarea)) nil))

        ;; Character deletion (removes the selection instead when one is active)
        ((eq key :backspace)
         (values (if (textarea-has-selection-p textarea)
                     (textarea-delete-selection textarea)
                     (textarea-delete-char-backward textarea))
                 nil))
        ((eq key :delete)
         (values (if (textarea-has-selection-p textarea)
                     (textarea-delete-selection textarea)
                     (textarea-delete-char-forward textarea))
                 nil))

        ;; Newline (replaces any active selection)
        ((or (eq key :enter) (and ctrl (characterp key) (char= key #\m)))
         (textarea-delete-selection textarea)
         (values (textarea-newline textarea) nil))

        ;; Delete to end of line (Ctrl+K)
        ((and ctrl (characterp key) (char= key #\k))
         (if (textarea-has-selection-p textarea)
             (values (textarea-delete-selection textarea) nil)
             (let* ((lines (textarea-lines textarea))
                    (row (textarea-row textarea))
                    (col (textarea-col textarea))
                    (line (aref lines row)))
               (setf (aref lines row) (subseq line 0 col))
               (values textarea nil))))

        ;; Delete to start of line (Ctrl+U)
        ((and ctrl (characterp key) (char= key #\u))
         (if (textarea-has-selection-p textarea)
             (values (textarea-delete-selection textarea) nil)
             (let* ((lines (textarea-lines textarea))
                    (row (textarea-row textarea))
                    (col (textarea-col textarea))
                    (line (aref lines row)))
               (setf (aref lines row) (subseq line col))
               (setf (textarea-col textarea) 0)
               (values textarea nil))))

        ;; Regular character input (replaces any active selection)
        ((characterp key)
         (textarea-delete-selection textarea)
         (values (textarea-insert-string textarea (string key)) nil))

        ;; No match
        (t (values textarea nil))))))

(defun textarea-view (textarea)
  "Render the textarea to a string."
  (textarea-recalculate-height textarea)
  (textarea-ensure-visible textarea)
  (textarea-clamp-yoffset textarea)
  (if (textarea-soft-wrap textarea)
      (%ta-view-wrapped textarea)
      (%ta-view-logical textarea)))

(defun %ta-view-logical (textarea)
  "Render one logical line per visual line (no soft-wrapping)."
  (let* ((lines (textarea-lines textarea))
         (n (length lines))
         (height (textarea-height textarea))
         (yoff (textarea-yoffset textarea))
         (show-ln (textarea-show-line-numbers textarea))
         (prompt (textarea-prompt textarea))
         (row (textarea-row textarea))
         (col (textarea-col textarea))
         (focused (textarea-focused textarea))
         (placeholder (textarea-placeholder textarea))
         (result '()))

    ;; Calculate line number width
    (let ((ln-width (if show-ln
                        (+ 2 (length (write-to-string (textarea-line-count textarea))))
                        0)))

      ;; If empty and has placeholder
      (when (and (string= (textarea-value textarea) "")
                 (not (string= placeholder "")))
        (push (format nil "~A~A~A"
                     prompt
                     (if show-ln (format nil "~vA " ln-width "1") "")
                     placeholder)
              result)
        (return-from %ta-view-logical (format nil "~{~A~^~%~}" (nreverse result))))

      ;; Render visible lines, starting at the scroll offset.
      (dotimes (i height)
        (let* ((line-no (+ yoff i))
               (line-text (if (< line-no n) (aref lines line-no) "")))

          ;; Build line
          (with-output-to-string (s)
            ;; Prompt
            (format s "~A" prompt)

            ;; Line number
            (when show-ln
              (if (< line-no n)
                  (format s "~v@A " ln-width (1+ line-no))
                  (format s "~vA " ln-width "")))

            ;; Content.  A selection covering this line takes precedence over
            ;; the inline cursor: while selecting, the highlight is the
            ;; meaningful affordance.
            (multiple-value-bind (sel-from sel-to)
                (when (< line-no n)
                  (%ta-selection-span textarea line-no 0 (length line-text)))
              (cond
                (sel-from
                 (format s "~A~A~A"
                         (subseq line-text 0 sel-from)
                         (tuition:render-styled (textarea-selection-style textarea)
                                                (subseq line-text sel-from sel-to))
                         (subseq line-text sel-to)))
                ((and focused (= line-no row) (< line-no n))
                 ;; Line with cursor
                 (let ((before (subseq line-text 0 (min col (length line-text))))
                       (at-cursor (if (< col (length line-text))
                                      (string (char line-text col))
                                      " "))
                       (after (if (< col (length line-text))
                                  (subseq line-text (min (1+ col) (length line-text)))
                                  "")))
                   (format s "~A[~A]~A" before at-cursor after)))
                (t
                 ;; Line without cursor
                 (format s "~A" line-text))))

            (push (get-output-stream-string s) result)))))

    (format nil "~{~A~^~%~}" (nreverse result))))

(defun %ta-view-wrapped (textarea)
  "Render with each logical line soft-wrapped to WIDTH display columns."
  (let* ((lines (textarea-lines textarea))
         (height (textarea-height textarea))
         (yoff (textarea-yoffset textarea))
         (show-ln (textarea-show-line-numbers textarea))
         (prompt (textarea-prompt textarea))
         (col (textarea-col textarea))
         (focused (textarea-focused textarea))
         (placeholder (textarea-placeholder textarea))
         (ln-width (if show-ln
                       (+ 2 (length (write-to-string (textarea-line-count textarea))))
                       0))
         (visuals (%ta-visual-lines textarea))
         (cursor-vi (%ta-cursor-visual-index textarea visuals))
         (result '()))

    ;; If empty and has placeholder
    (when (and (string= (textarea-value textarea) "")
               (not (string= placeholder "")))
      (push (format nil "~A~A~A"
                   prompt
                   (if show-ln (format nil "~vA " ln-width "1") "")
                   placeholder)
            result)
      (return-from %ta-view-wrapped (format nil "~{~A~^~%~}" (nreverse result))))

    ;; Render visible (wrapped) lines, starting at the scroll offset.
    (dotimes (i height)
      (let ((vi (+ yoff i)))
        (push
         (cond
           ((>= vi (length visuals))
            ;; Blank line beyond content.
            (with-output-to-string (s)
              (format s "~A" prompt)
              (when show-ln (format s "~vA " ln-width ""))))
           (t
            (let* ((v (aref visuals vi))
                   (lrow (first v)) (vstart (second v)) (vend (third v))
                   (line (aref lines lrow))
                   (seg (subseq line vstart vend))
                   (seg-len (- vend vstart))
                   (first-seg-p (zerop vstart)))
              (with-output-to-string (s)
                (format s "~A" prompt)
                (when show-ln
                  (if first-seg-p
                      (format s "~v@A " ln-width (1+ lrow))
                      (format s "~vA " ln-width "")))
                (multiple-value-bind (sel-from sel-to)
                    (%ta-selection-span textarea lrow vstart vend)
                  (cond
                    (sel-from
                     ;; Selection highlight takes precedence over the cursor.
                     (format s "~A~A~A"
                             (subseq seg 0 sel-from)
                             (tuition:render-styled
                              (textarea-selection-style textarea)
                              (subseq seg sel-from sel-to))
                             (subseq seg sel-to)))
                    ((and focused (= vi cursor-vi))
                     ;; Cursor sits within this segment.
                     (let* ((local-col (- col vstart))
                            (before (subseq seg 0 (min local-col seg-len)))
                            (at-cursor (if (< local-col seg-len)
                                           (string (char seg local-col))
                                           " "))
                            (after (if (< local-col seg-len)
                                       (subseq seg (min (1+ local-col) seg-len))
                                       "")))
                       (format s "~A[~A]~A" before at-cursor after)))
                    (t (format s "~A" seg))))))))
         result)))

    (format nil "~{~A~^~%~}" (nreverse result))))
