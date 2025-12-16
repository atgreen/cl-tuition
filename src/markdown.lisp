;;; markdown.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Markdown renderer for terminal output with ANSI styling
;;;; Inspired by Charmbracelet's Glamour

(in-package #:tuition)

;;; Style Configuration

(defstruct markdown-style
  "Style configuration for markdown rendering."
  ;; Document
  (document-margin 2)
  (document-color nil)

  ;; Headers
  (h1-prefix "# " :type string)
  (h1-color :bright-cyan)
  (h1-bold t)

  (h2-prefix "## " :type string)
  (h2-color :bright-blue)
  (h2-bold t)

  (h3-prefix "### " :type string)
  (h3-color :blue)
  (h3-bold t)

  ;; Text styles
  (bold-color nil)
  (italic-color nil)
  (code-color :bright-yellow)
  (code-bg nil)

  ;; Code blocks
  (code-block-color :white)
  (code-block-bg :black)
  (code-block-margin 1)

  ;; Lists
  (list-bullet "• " :type string)
  (list-indent 2)
  (ordered-bullet-format "~D. " :type string)

  ;; Quotes
  (quote-prefix "│ " :type string)
  (quote-color :bright-black)
  (quote-indent 0)

  ;; Links
  (link-color :bright-blue)
  (link-underline t)

  ;; Horizontal rule
  (hr-char "─" :type string)

  ;; Emphasis
  (emph-color :italic)
  (strong-color :bold))

;;; Built-in Styles

(defun make-style-dark ()
  "Create a dark theme style (default)."
  (make-markdown-style
   :h1-color :bright-cyan
   :h2-color :bright-blue
   :h3-color :blue
   :code-color :bright-yellow
   :code-block-color :white
   :code-block-bg :black
   :link-color :bright-blue
   :quote-color :bright-black))

(defun make-style-light ()
  "Create a light theme style."
  (make-markdown-style
   :h1-color :blue
   :h2-color :cyan
   :h3-color :blue
   :code-color :magenta
   :code-block-color :black
   :code-block-bg :white
   :link-color :blue
   :quote-color :black))

(defun make-style-pink ()
  "Create a pink theme style."
  (make-markdown-style
   :h1-color :bright-magenta
   :h2-color :magenta
   :h3-color :bright-magenta
   :code-color :bright-cyan
   :link-color :bright-magenta
   :quote-color :magenta))

(defun make-style-ascii ()
  "Create an ASCII-only style (no colors)."
  (make-markdown-style
   :h1-color nil
   :h2-color nil
   :h3-color nil
   :code-color nil
   :code-block-color nil
   :code-block-bg nil
   :link-color nil
   :quote-color nil
   :list-bullet "* "
   :quote-prefix "| "
   :hr-char "-"))

;;; Markdown Renderer

(defclass markdown-renderer ()
  ((style :initarg :style :accessor renderer-style
          :initform (make-style-dark)
          :documentation "Rendering style")
   (width :initarg :width :accessor renderer-width
          :initform 80
          :documentation "Maximum line width")
   (preserve-newlines :initarg :preserve-newlines
                      :accessor renderer-preserve-newlines
                      :initform nil
                      :documentation "Preserve multiple newlines"))
  (:documentation "Markdown to ANSI terminal renderer."))

(defun make-markdown-renderer (&key (style :dark) (width 80))
  "Create a new markdown renderer with the specified style.
Styles: :dark (default), :light, :pink, :ascii, or a markdown-style struct."
  (make-instance 'markdown-renderer
                 :style (if (keywordp style)
                           (case style
                             (:dark (make-style-dark))
                             (:light (make-style-light))
                             (:pink (make-style-pink))
                             (:ascii (make-style-ascii))
                             (t (make-style-dark)))
                           style)
                 :width width))

;;; Text Processing Helpers

(defun markdown-wrap-text (text width &optional (indent 0))
  "Wrap text to fit within width, with optional indentation.
Internal helper for markdown rendering."
  (let ((words (split-string-by-whitespace text))
        (lines '())
        (current-line '())
        (current-length indent))

    (dolist (word words)
      (let ((word-len (visible-length word)))
        (if (> (+ current-length word-len (if current-line 1 0)) width)
            ;; Start new line
            (progn
              (when current-line
                (push (format nil "~vA~{~A~^ ~}"
                             indent ""
                             (nreverse current-line))
                      lines))
              (setf current-line (list word)
                    current-length (+ indent word-len)))
            ;; Add to current line
            (progn
              (push word current-line)
              (incf current-length (+ word-len (if (cdr current-line) 1 0)))))))

    ;; Add final line
    (when current-line
      (push (format nil "~vA~{~A~^ ~}"
                   indent ""
                   (nreverse current-line))
            lines))

    (format nil "~{~A~^~%~}" (nreverse lines))))

(defun split-string-by-whitespace (string)
  "Split string by whitespace."
  (let ((result '())
        (word '()))
    (loop for char across string
          do (if (member char '(#\Space #\Tab #\Newline))
                 (when word
                   (push (coerce (nreverse word) 'string) result)
                   (setf word nil))
                 (push char word)))
    (when word
      (push (coerce (nreverse word) 'string) result))
    (nreverse result)))

;;; Styling Helper

(defun resolve-markdown-color (color-spec)
  "Resolve a markdown color keyword to actual ANSI code."
  (cond
    ((null color-spec) nil)
    ((stringp color-spec) color-spec)  ; Already a code
    ((keywordp color-spec)
     (case color-spec
       (:black *fg-black*)
       (:red *fg-red*)
       (:green *fg-green*)
       (:yellow *fg-yellow*)
       (:blue *fg-blue*)
       (:magenta *fg-magenta*)
       (:cyan *fg-cyan*)
       (:white *fg-white*)
       (:bright-black *fg-bright-black*)
       (:bright-red *fg-bright-red*)
       (:bright-green *fg-bright-green*)
       (:bright-yellow *fg-bright-yellow*)
       (:bright-blue *fg-bright-blue*)
       (:bright-magenta *fg-bright-magenta*)
       (:bright-cyan *fg-bright-cyan*)
       (:bright-white *fg-bright-white*)
       ;; :bold and :italic are style attributes, not colors
       ;; Return nil so they don't get added to color codes
       (:bold nil)
       (:italic nil)
       (t nil)))
    (t color-spec)))  ; Pass through other types

(defun style-text (text &key color bg bold italic underline)
  "Apply ANSI styling to text. Helper function for markdown rendering."
  (if (or color bg bold italic underline)
      (let ((s (make-style :foreground (resolve-markdown-color color)
                          :background (resolve-markdown-color bg)
                          :bold (or bold (eql color :bold))
                          :italic (or italic (eql color :italic))
                          :underline underline)))
        (render-styled s text))
      text))

;;; Inline Parsing

(defun backtick-char-p (char)
  "Check if CHAR is a backtick or backtick-like character."
  (member (char-code char)
          '(#x0060    ; ` GRAVE ACCENT (standard backtick)
            #x2018    ; ' LEFT SINGLE QUOTATION MARK
            #x2019    ; ' RIGHT SINGLE QUOTATION MARK
            #x02CB    ; ˋ MODIFIER LETTER GRAVE ACCENT
            #xFF40))) ; ` FULLWIDTH GRAVE ACCENT

(defun find-closing-backtick (text start)
  "Find closing backtick (or backtick-like char) in TEXT starting from START."
  (loop for i from start below (length text)
        when (backtick-char-p (char text i))
        return i))

(defun parse-inline-styles (text style)
  "Parse inline markdown styles: **bold**, *italic*, `code`, [links](url)."
  (let ((result "")
        (pos 0)
        (len (length text)))

    (loop while (< pos len)
          do (let ((char (char text pos)))
               (cond
                 ;; Bold: **text**
                 ((and (< (+ pos 1) len)
                       (char= char #\*)
                       (char= (char text (+ pos 1)) #\*))
                  (let ((end (search "**" text :start2 (+ pos 2))))
                    (if end
                        (let* ((content (subseq text (+ pos 2) end))
                               ;; Recursively process inline styles (code, etc.) inside bold
                               (processed (parse-inline-styles content style)))
                          (setf result
                                (concatenate 'string result
                                            (if (markdown-style-strong-color style)
                                                (style-text processed
                                                           :color (markdown-style-strong-color style)
                                                           :bold t)
                                                processed)))
                          (setf pos (+ end 2)))
                        (progn
                          (setf result (concatenate 'string result (string char)))
                          (incf pos)))))

                 ;; Italic: *text*
                 ((char= char #\*)
                  (let ((end (position #\* text :start (+ pos 1))))
                    (if end
                        (let* ((content (subseq text (+ pos 1) end))
                               ;; Recursively process inline styles (code, etc.) inside italic
                               (processed (parse-inline-styles content style)))
                          (setf result
                                (concatenate 'string result
                                            (if (markdown-style-emph-color style)
                                                (style-text processed
                                                           :color (markdown-style-emph-color style)
                                                           :italic t)
                                                processed)))
                          (setf pos (+ end 1)))
                        (progn
                          (setf result (concatenate 'string result (string char)))
                          (incf pos)))))

                 ;; Code: `text` (handles various backtick-like characters)
                 ((backtick-char-p char)
                  (let ((end (find-closing-backtick text (+ pos 1))))
                    (if end
                        (let ((content (subseq text (+ pos 1) end)))
                          (setf result
                                (concatenate 'string result
                                            (style-text content
                                                       :color (markdown-style-code-color style)
                                                       :bg (markdown-style-code-bg style))))
                          (setf pos (+ end 1)))
                        (progn
                          (setf result (concatenate 'string result (string char)))
                          (incf pos)))))

                 ;; Links: [text](url)
                 ((char= char #\[)
                  (let ((close-bracket (position #\] text :start (+ pos 1))))
                    (if (and close-bracket
                             (< (+ close-bracket 1) len)
                             (char= (char text (+ close-bracket 1)) #\())
                        (let* ((link-text (subseq text (+ pos 1) close-bracket))
                               (url-start (+ close-bracket 2))
                               (url-end (position #\) text :start url-start)))
                          (if url-end
                              (progn
                                (setf result
                                      (concatenate 'string result
                                                  (style-text link-text
                                                             :color (markdown-style-link-color style)
                                                             :underline (markdown-style-link-underline style))))
                                (setf pos (+ url-end 1)))
                              (progn
                                (setf result (concatenate 'string result (string char)))
                                (incf pos))))
                        (progn
                          (setf result (concatenate 'string result (string char)))
                          (incf pos)))))

                 ;; Regular character
                 (t
                  (setf result (concatenate 'string result (string char)))
                  (incf pos)))))

    result))

;;; Block Parsing

(defun render-markdown-line (line renderer)
  "Render a single markdown line with appropriate styling."
  (let ((style (renderer-style renderer))
        (width (renderer-width renderer))
        (trimmed (string-trim '(#\Space #\Tab) line)))

    (cond
      ;; Empty line
      ((string= trimmed "")
       "")

      ;; Horizontal rule: --- or ***
      ((or (and (>= (length trimmed) 3)
                (every (lambda (c) (char= c #\-)) trimmed))
           (and (>= (length trimmed) 3)
                (every (lambda (c) (char= c #\*)) trimmed)))
       (let ((char (markdown-style-hr-char style)))
         (make-string width :initial-element (char char 0))))

      ;; Headers
      ((and (> (length trimmed) 0) (char= (char trimmed 0) #\#))
       (let ((level 0)
             (pos 0))
         ;; Count # characters
         (loop while (and (< pos (length trimmed))
                         (char= (char trimmed pos) #\#))
               do (incf level)
                  (incf pos))

         (when (and (< pos (length trimmed))
                   (char= (char trimmed pos) #\Space))
           (let* ((content (string-trim '(#\Space) (subseq trimmed pos)))
                  ;; Process inline styles (code, bold, etc.) in header content
                  (styled-content (parse-inline-styles content style)))
             (case level
               (1 (style-text (concatenate 'string
                                          (markdown-style-h1-prefix style)
                                          styled-content)
                             :color (markdown-style-h1-color style)
                             :bold (markdown-style-h1-bold style)))
               (2 (style-text (concatenate 'string
                                          (markdown-style-h2-prefix style)
                                          styled-content)
                             :color (markdown-style-h2-color style)
                             :bold (markdown-style-h2-bold style)))
               (3 (style-text (concatenate 'string
                                          (markdown-style-h3-prefix style)
                                          styled-content)
                             :color (markdown-style-h3-color style)
                             :bold (markdown-style-h3-bold style)))
               (t (parse-inline-styles trimmed style)))))))

      ;; Unordered list: - or *
      ((and (> (length trimmed) 1)
            (member (char trimmed 0) '(#\- #\*))
            (char= (char trimmed 1) #\Space))
       (let* ((content (string-trim '(#\Space) (subseq trimmed 2)))
              (base-indent (markdown-style-list-indent style))
              (bullet (markdown-style-list-bullet style))
              (bullet-width (visible-length bullet))
              (styled-content (parse-inline-styles content style))
              ;; Available width for text (after indent and bullet)
              (text-width (- width base-indent bullet-width))
              ;; Wrap with hanging indent: continuation lines indent by bullet-width
              (wrapped (wrap-text styled-content text-width
                                 :continuation-indent (+ base-indent bullet-width))))
         (concatenate 'string
                     (make-string base-indent :initial-element #\Space)
                     bullet
                     wrapped)))

      ;; Ordered list: 1. 2. etc
      ((and (> (length trimmed) 2)
            (digit-char-p (char trimmed 0))
            (char= (char trimmed 1) #\.)
            (char= (char trimmed 2) #\Space))
       (let* ((content (string-trim '(#\Space) (subseq trimmed 3)))
              (base-indent (markdown-style-list-indent style))
              (number (digit-char-p (char trimmed 0)))
              (bullet (format nil (markdown-style-ordered-bullet-format style) number))
              (bullet-width (visible-length bullet))
              (styled-content (parse-inline-styles content style))
              ;; Available width for text (after indent and bullet)
              (text-width (- width base-indent bullet-width))
              ;; Wrap with hanging indent: continuation lines indent by bullet-width
              (wrapped (wrap-text styled-content text-width
                                 :continuation-indent (+ base-indent bullet-width))))
         (concatenate 'string
                     (make-string base-indent :initial-element #\Space)
                     bullet
                     wrapped)))

      ;; Quote: >
      ((and (> (length trimmed) 0) (char= (char trimmed 0) #\>))
       (let* ((content (string-trim '(#\Space) (subseq trimmed 1)))
              (indent (markdown-style-quote-indent style))
              (prefix (markdown-style-quote-prefix style))
              (styled-content (parse-inline-styles content style)))
         (style-text (concatenate 'string
                                 (make-string indent :initial-element #\Space)
                                 prefix
                                 styled-content)
                    :color (markdown-style-quote-color style))))

      ;; Code block marker: ```
      ((and (>= (length trimmed) 3)
            (string= (subseq trimmed 0 3) "```"))
       nil) ;; Handled separately in render-markdown

      ;; Table row: | ... |
      ((and (> (length trimmed) 0)
            (char= (char trimmed 0) #\|))
       nil) ;; Handled separately in render-markdown

      ;; Regular paragraph text - wrap to width
      (t
       (let ((styled (parse-inline-styles trimmed style)))
         (markdown-wrap-text styled width))))))

;;; Table Rendering

(defun table-row-p (line)
  "Check if LINE is a markdown table row (starts with |)."
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (and (> (length trimmed) 0)
         (char= (char trimmed 0) #\|))))

(defun table-separator-p (line)
  "Check if LINE is a markdown table separator row (|---|---|)."
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (and (table-row-p trimmed)
         ;; Check if it only contains |, -, :, and spaces
         (every (lambda (c)
                  (member c '(#\| #\- #\: #\Space #\Tab)))
                trimmed))))

(defun parse-table-row (line)
  "Parse a table row into a list of cell contents."
  (let* ((trimmed (string-trim '(#\Space #\Tab) line))
         ;; Remove leading and trailing |
         (inner (if (and (> (length trimmed) 1)
                         (char= (char trimmed 0) #\|))
                    (subseq trimmed 1)
                    trimmed))
         (inner (if (and (> (length inner) 0)
                         (char= (char inner (1- (length inner))) #\|))
                    (subseq inner 0 (1- (length inner)))
                    inner)))
    ;; Split by | and trim each cell
    (mapcar (lambda (cell) (string-trim '(#\Space #\Tab) cell))
            (split-by-char inner #\|))))

(defun split-by-char (string char)
  "Split STRING by CHAR into a list of substrings."
  (let ((result '())
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) char)
          do (push (subseq string start i) result)
             (setf start (1+ i)))
    (push (subseq string start) result)
    (nreverse result)))

(defun render-table (table-lines style)
  "Render markdown table lines as a formatted table."
  (let* ((rows (loop for line in table-lines
                     unless (table-separator-p line)
                     collect (parse-table-row line)))
         (num-cols (if rows (length (first rows)) 0))
         ;; Pre-compute styled cells and calculate column widths based on visible length
         (styled-rows (mapcar (lambda (row)
                                (mapcar (lambda (cell)
                                          (parse-inline-styles (or cell "") style))
                                        row))
                              rows))
         (col-widths (make-list num-cols :initial-element 0)))
    (when (zerop num-cols)
      (return-from render-table ""))
    ;; Find max visible width for each column
    (dolist (row styled-rows)
      (loop for cell in row
            for i from 0
            when (< i num-cols)
            do (setf (nth i col-widths)
                     (max (nth i col-widths) (visible-length cell)))))
    ;; Build the table output
    (let ((output '())
          (border-top "")
          (border-mid "")
          (border-bot ""))
      ;; Build border strings
      (setf border-top
            (concatenate 'string "┌"
                        (format nil "~{~A~^┬~}"
                                (mapcar (lambda (w) (make-string (+ w 2) :initial-element #\─))
                                        col-widths))
                        "┐"))
      (setf border-mid
            (concatenate 'string "├"
                        (format nil "~{~A~^┼~}"
                                (mapcar (lambda (w) (make-string (+ w 2) :initial-element #\─))
                                        col-widths))
                        "┤"))
      (setf border-bot
            (concatenate 'string "└"
                        (format nil "~{~A~^┴~}"
                                (mapcar (lambda (w) (make-string (+ w 2) :initial-element #\─))
                                        col-widths))
                        "┘"))
      ;; Add top border
      (push border-top output)
      ;; Add rows
      (loop for row in styled-rows
            for row-num from 0
            do (let ((cells (loop for cell in row
                                  for w in col-widths
                                  for i from 0 below num-cols
                                  ;; Calculate padding based on visible length
                                  for vis-len = (visible-length cell)
                                  for padding = (- w vis-len)
                                  collect (format nil " ~A~A " cell
                                                 (make-string (max 0 padding) :initial-element #\Space)))))
                 (push (concatenate 'string "│"
                                   (format nil "~{~A~^│~}" cells)
                                   "│")
                       output)
                 ;; Add separator after header row
                 (when (= row-num 0)
                   (push border-mid output))))
      ;; Add bottom border
      (push border-bot output)
      ;; Return as single string
      (format nil "~{~A~^~%~}" (nreverse output)))))

;;; Main Rendering Function

(defun render-markdown (text &key (style :dark) (width 80))
  "Render markdown text to ANSI-styled output.

TEXT: Markdown source text
STYLE: One of :dark, :light, :pink, :ascii, or a markdown-style struct
WIDTH: Maximum line width for wrapping

Returns formatted string with ANSI escape codes.

Example:
  (render-markdown \"# Hello\\n\\nThis is **bold** and *italic*.\" :style :dark)"

  (let ((renderer (make-markdown-renderer :style style :width width))
        (lines (split-string-by-newline text))
        (result '())
        (in-code-block nil)
        (code-block-lines '())
        (in-table nil)
        (table-lines '()))

    (dolist (line lines)
      (cond
        ;; Check for code block markers
        ((and (not in-table)
              (>= (length (string-trim '(#\Space) line)) 3)
              (string= (subseq (string-trim '(#\Space) line) 0 3) "```"))
         (if in-code-block
             ;; End code block
             (progn
               (let* ((style (renderer-style renderer))
                      (margin (markdown-style-code-block-margin style))
                      (indent (make-string margin :initial-element #\Space)))
                 (dolist (code-line (nreverse code-block-lines))
                   (push (concatenate 'string indent
                                     (style-text code-line
                                                :color (markdown-style-code-block-color style)
                                                :bg (markdown-style-code-block-bg style)))
                         result)))
               (setf in-code-block nil
                     code-block-lines '()))
             ;; Start code block
             (setf in-code-block t)))

        ;; Inside code block
        (in-code-block
         (push line code-block-lines))

        ;; Table row handling
        ((and (not in-code-block) (table-row-p line))
         (if in-table
             ;; Continue collecting table rows
             (push line table-lines)
             ;; Start new table
             (progn
               (setf in-table t)
               (push line table-lines))))

        ;; End of table (non-table line while in-table)
        (in-table
         ;; Render the collected table
         (push (render-table (nreverse table-lines) (renderer-style renderer)) result)
         (setf in-table nil
               table-lines '())
         ;; Process the current line normally
         (let ((rendered (render-markdown-line line renderer)))
           (when rendered
             (push rendered result))))

        ;; Regular line
        (t
         (let ((rendered (render-markdown-line line renderer)))
           (when rendered
             (push rendered result))))))

    ;; Handle any remaining table at end of input
    (when in-table
      (push (render-table (nreverse table-lines) (renderer-style renderer)) result))

    (format nil "~{~A~^~%~}" (nreverse result))))

;;; Convenience function

(defun markdown (text &key (style :dark) (width 80))
  "Shorthand for render-markdown."
  (render-markdown text :style style :width width))
