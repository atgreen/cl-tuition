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
   #:textarea-uppercase-word))

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
               :documentation "Maximum viewport height cap when DYNAMIC-HEIGHT is on (0 = no cap)"))
  (:documentation "A multi-line text input component."))

(defun make-textarea (&key (width 40) (height 6) (placeholder "")
                        (char-limit 0) (max-lines 1000)
                        (show-line-numbers t) (prompt "> ")
                        soft-wrap dynamic-height (min-height 1) (max-height 0))
  "Create a new textarea with the given dimensions."
  (make-instance 'textarea
                 :width width :height height
                 :char-limit char-limit :max-lines max-lines
                 :show-line-numbers show-line-numbers :prompt prompt
                 :placeholder placeholder
                 :soft-wrap soft-wrap :dynamic-height dynamic-height
                 :min-height min-height :max-height max-height))

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

(defun textarea-reset (textarea)
  "Reset the textarea to empty state."
  (setf (textarea-lines textarea) (vector "")
        (textarea-row textarea) 0
        (textarea-col textarea) 0
        (textarea-yoffset textarea) 0)
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
  "Insert a newline at cursor position, respecting max-lines."
  (let ((max-lines (textarea-max-lines textarea)))
    (when (and (plusp max-lines)
               (>= (textarea-line-count textarea) max-lines))
      (return-from textarea-newline textarea)))
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
  "Move the cursor to the start of the previous word."
  (let ((b (%ta-prev-word-boundary (textarea-current-line textarea)
                                   (textarea-col textarea))))
    (when b (setf (textarea-col textarea) b)))
  textarea)

(defun textarea-cursor-word-forward (textarea)
  "Move the cursor past the current word, or to the end of the line."
  (let ((f (%ta-next-word-boundary (textarea-current-line textarea)
                                   (textarea-col textarea))))
    (when f (setf (textarea-col textarea) f)))
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

;;; Paging and vertical scroll offset (keep the cursor in view)

(defun textarea-page-up (textarea)
  "Move the cursor up by roughly one viewport height."
  (let ((delta (max 1 (textarea-height textarea))))
    (setf (textarea-row textarea)
          (max 0 (- (textarea-row textarea) delta)))
    (textarea-set-cursor textarea (textarea-col textarea)))
  textarea)

(defun textarea-page-down (textarea)
  "Move the cursor down by roughly one viewport height."
  (let ((delta (max 1 (textarea-height textarea)))
        (last (1- (textarea-line-count textarea))))
    (setf (textarea-row textarea)
          (min (max 0 last) (+ (textarea-row textarea) delta)))
    (textarea-set-cursor textarea (textarea-col textarea)))
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
  ;; Bracketed paste.
  (when (typep msg 'tuition:paste-msg)
    (textarea-insert-string textarea (tuition:paste-msg-text msg))
    (return-from %textarea-dispatch-key (values textarea nil)))
  (unless (tuition:key-press-msg-p msg)
    (return-from %textarea-dispatch-key (values textarea nil)))
  (let ((key (tuition:key-event-code msg))
        (alt (tuition:mod-contains (tuition:key-event-mod msg) tuition:+mod-alt+))
        (ctrl (tuition:mod-contains (tuition:key-event-mod msg) tuition:+mod-ctrl+)))
    (cond
      ;; Arrow navigation
      ((eq key :up)    (values (textarea-cursor-up textarea) nil))
      ((eq key :down)  (values (textarea-cursor-down textarea) nil))
      ((eq key :left)  (values (textarea-cursor-left textarea) nil))
      ((eq key :right) (values (textarea-cursor-right textarea) nil))

      ;; Paging
      ((eq key :page-up)   (values (textarea-page-up textarea) nil))
      ((eq key :page-down) (values (textarea-page-down textarea) nil))

      ;; Document-level begin/end (must precede line start/end below)
      ((and ctrl (eq key :home)) (values (textarea-move-to-begin textarea) nil))
      ((and ctrl (eq key :end))  (values (textarea-move-to-end textarea) nil))

      ;; Word movement (Alt+b / Alt+f)
      ((and alt (characterp key) (char= key #\b))
       (values (textarea-cursor-word-backward textarea) nil))
      ((and alt (characterp key) (char= key #\f))
       (values (textarea-cursor-word-forward textarea) nil))

      ;; Word deletion (Ctrl+w, Alt+Backspace, Alt+d)
      ((and ctrl (characterp key) (char= key #\w))
       (values (textarea-delete-word-backward textarea) nil))
      ((and alt (eq key :backspace))
       (values (textarea-delete-word-backward textarea) nil))
      ((and alt (characterp key) (char= key #\d))
       (values (textarea-delete-word-forward textarea) nil))

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
       (values (textarea-cursor-start textarea) nil))
      ((or (eq key :end) (and ctrl (characterp key) (char= key #\e)))
       (values (textarea-cursor-end textarea) nil))

      ;; Character deletion
      ((eq key :backspace) (values (textarea-delete-char-backward textarea) nil))
      ((eq key :delete)    (values (textarea-delete-char-forward textarea) nil))

      ;; Newline
      ((or (eq key :enter) (and ctrl (characterp key) (char= key #\m)))
       (values (textarea-newline textarea) nil))

      ;; Delete to end of line (Ctrl+K)
      ((and ctrl (characterp key) (char= key #\k))
       (let* ((lines (textarea-lines textarea))
              (row (textarea-row textarea))
              (col (textarea-col textarea))
              (line (aref lines row)))
         (setf (aref lines row) (subseq line 0 col))
         (values textarea nil)))

      ;; Delete to start of line (Ctrl+U)
      ((and ctrl (characterp key) (char= key #\u))
       (let* ((lines (textarea-lines textarea))
              (row (textarea-row textarea))
              (col (textarea-col textarea))
              (line (aref lines row)))
         (setf (aref lines row) (subseq line col))
         (setf (textarea-col textarea) 0)
         (values textarea nil)))

      ;; Regular character input
      ((characterp key)
       (values (textarea-insert-string textarea (string key)) nil))

      ;; No match
      (t (values textarea nil)))))

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

            ;; Content with cursor
            (if (and focused (= line-no row) (< line-no n))
                ;; Line with cursor
                (let ((before (subseq line-text 0 (min col (length line-text))))
                      (at-cursor (if (< col (length line-text))
                                    (string (char line-text col))
                                    " "))
                      (after (if (< col (length line-text))
                                (subseq line-text (min (1+ col) (length line-text)))
                                "")))
                  (format s "~A[~A]~A" before at-cursor after))
                ;; Line without cursor
                (format s "~A" line-text))

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
                (if (and focused (= vi cursor-vi))
                    ;; Cursor sits within this segment.
                    (let* ((local-col (- col vstart))
                           (before (subseq seg 0 (min local-col seg-len)))
                           (at-cursor (if (< local-col seg-len)
                                          (string (char seg local-col))
                                          " "))
                           (after (if (< local-col seg-len)
                                      (subseq seg (min (1+ local-col) seg-len))
                                      "")))
                      (format s "~A[~A]~A" before at-cursor after))
                    (format s "~A" seg))))))
         result)))

    (format nil "~{~A~^~%~}" (nreverse result))))
