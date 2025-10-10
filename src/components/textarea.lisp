;;;; SPDX-License-Identifier: MIT
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

   ;; Cursor operations
   #:textarea-cursor-position
   #:textarea-line-count
   #:textarea-length))

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
                 :documentation "Whether cursor is visible (for blinking)"))
  (:documentation "A multi-line text input component."))

(defun make-textarea (&key (width 40) (height 6) placeholder)
  "Create a new textarea with the given dimensions."
  (let ((ta (make-instance 'textarea :width width :height height)))
    (when placeholder
      (setf (textarea-placeholder ta) placeholder))
    ta))

;;; Content management

(defun textarea-set-value (textarea text)
  "Set the textarea's content."
  (setf (textarea-lines textarea)
        (coerce (tuition:split-string-by-newline text) 'vector))
  (setf (textarea-row textarea) 0
        (textarea-col textarea) 0)
  textarea)

(defun textarea-value (textarea)
  "Get the textarea's full content as a string."
  (let ((lines (textarea-lines textarea)))
    (if (zerop (length lines))
        ""
        (format nil "~{~A~^~%~}" (coerce lines 'list)))))

(defun textarea-insert-string (textarea str)
  "Insert a string at the cursor position."
  (let* ((lines (textarea-lines textarea))
         (row (textarea-row textarea))
         (col (textarea-col textarea))
         (current-line (aref lines row))
         (new-lines (tuition:split-string-by-newline str)))

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
    textarea))

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
        (textarea-col textarea) 0)
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
  "Insert a newline at cursor position."
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

;;; TEA protocol implementation

(defun textarea-init (textarea)
  "Initialize the textarea. Returns nil (no command)."
  (declare (ignore textarea))
  nil)

(defun textarea-update (textarea msg)
  "Update textarea with a message. Returns (values new-textarea cmd)."
  (unless (textarea-focused textarea)
    (return-from textarea-update (values textarea nil)))

  (cond
    ;; Key messages
    ((tuition:key-msg-p msg)
     (let ((key (tuition:key-msg-key msg))
           (ctrl (tuition:key-msg-ctrl msg)))
       (cond
         ;; Navigation
         ((eq key :up)
          (values (textarea-cursor-up textarea) nil))
         ((eq key :down)
          (values (textarea-cursor-down textarea) nil))
         ((eq key :left)
          (values (textarea-cursor-left textarea) nil))
         ((eq key :right)
          (values (textarea-cursor-right textarea) nil))

         ;; Home/End
         ((or (eq key :home) (and ctrl (characterp key) (char= key #\a)))
          (values (textarea-cursor-start textarea) nil))
         ((or (eq key :end) (and ctrl (characterp key) (char= key #\e)))
          (values (textarea-cursor-end textarea) nil))

         ;; Deletion
         ((eq key :backspace)
          (values (textarea-delete-char-backward textarea) nil))
         ((eq key :delete)
          (values (textarea-delete-char-forward textarea) nil))

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
          (let ((char-str (string key)))
            (values (textarea-insert-string textarea char-str) nil)))

         ;; No match
         (t (values textarea nil)))))

    ;; Default: no change
    (t (values textarea nil))))

(defun textarea-view (textarea)
  "Render the textarea to a string."
  (let* ((lines (textarea-lines textarea))
         (height (textarea-height textarea))
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
        (return-from textarea-view (format nil "~{~A~^~%~}" (nreverse result))))

      ;; Render visible lines
      (dotimes (i height)
        (let ((line-text (if (< i (length lines))
                            (aref lines i)
                            "")))

          ;; Build line
          (with-output-to-string (s)
            ;; Prompt
            (format s "~A" prompt)

            ;; Line number
            (when show-ln
              (if (< i (length lines))
                  (format s "~v@A " ln-width (1+ i))
                  (format s "~vA " ln-width "")))

            ;; Content with cursor
            (if (and focused (= i row) (< i (length lines)))
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
