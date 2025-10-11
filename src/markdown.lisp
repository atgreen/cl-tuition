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

(defun style-text (text &key color bg bold italic underline)
  "Apply ANSI styling to text. Helper function for markdown rendering."
  (if (or color bg bold italic underline)
      (let ((s (make-style :foreground color
                          :background bg
                          :bold bold
                          :italic italic
                          :underline underline)))
        (render-styled s text))
      text))

;;; Inline Parsing

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
                        (let ((content (subseq text (+ pos 2) end)))
                          (setf result
                                (concatenate 'string result
                                            (if (markdown-style-strong-color style)
                                                (style-text content
                                                           :color (markdown-style-strong-color style)
                                                           :bold t)
                                                content)))
                          (setf pos (+ end 2)))
                        (progn
                          (setf result (concatenate 'string result (string char)))
                          (incf pos)))))

                 ;; Italic: *text*
                 ((char= char #\*)
                  (let ((end (position #\* text :start (+ pos 1))))
                    (if end
                        (let ((content (subseq text (+ pos 1) end)))
                          (setf result
                                (concatenate 'string result
                                            (if (markdown-style-emph-color style)
                                                (style-text content
                                                           :color (markdown-style-emph-color style)
                                                           :italic t)
                                                content)))
                          (setf pos (+ end 1)))
                        (progn
                          (setf result (concatenate 'string result (string char)))
                          (incf pos)))))

                 ;; Code: `text`
                 ((char= char #\`)
                  (let ((end (position #\` text :start (+ pos 1))))
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
           (let ((content (string-trim '(#\Space) (subseq trimmed pos))))
             (case level
               (1 (style-text (concatenate 'string
                                          (markdown-style-h1-prefix style)
                                          content)
                             :color (markdown-style-h1-color style)
                             :bold (markdown-style-h1-bold style)))
               (2 (style-text (concatenate 'string
                                          (markdown-style-h2-prefix style)
                                          content)
                             :color (markdown-style-h2-color style)
                             :bold (markdown-style-h2-bold style)))
               (3 (style-text (concatenate 'string
                                          (markdown-style-h3-prefix style)
                                          content)
                             :color (markdown-style-h3-color style)
                             :bold (markdown-style-h3-bold style)))
               (t (parse-inline-styles trimmed style)))))))

      ;; Unordered list: - or *
      ((and (> (length trimmed) 1)
            (member (char trimmed 0) '(#\- #\*))
            (char= (char trimmed 1) #\Space))
       (let* ((content (string-trim '(#\Space) (subseq trimmed 2)))
              (indent (markdown-style-list-indent style))
              (bullet (markdown-style-list-bullet style))
              (styled-content (parse-inline-styles content style)))
         (concatenate 'string
                     (make-string indent :initial-element #\Space)
                     bullet
                     styled-content)))

      ;; Ordered list: 1. 2. etc
      ((and (> (length trimmed) 2)
            (digit-char-p (char trimmed 0))
            (char= (char trimmed 1) #\.)
            (char= (char trimmed 2) #\Space))
       (let* ((content (string-trim '(#\Space) (subseq trimmed 3)))
              (indent (markdown-style-list-indent style))
              (number (digit-char-p (char trimmed 0)))
              (bullet (format nil (markdown-style-ordered-bullet-format style) number))
              (styled-content (parse-inline-styles content style)))
         (concatenate 'string
                     (make-string indent :initial-element #\Space)
                     bullet
                     styled-content)))

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

      ;; Regular paragraph text
      (t
       (parse-inline-styles trimmed style)))))

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
        (code-block-lines '()))

    (dolist (line lines)
      (cond
        ;; Check for code block markers
        ((and (>= (length (string-trim '(#\Space) line)) 3)
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

        ;; Regular line
        (t
         (let ((rendered (render-markdown-line line renderer)))
           (when rendered
             (push rendered result))))))

    (format nil "~{~A~^~%~}" (nreverse result))))

;;; Convenience function

(defun markdown (text &key (style :dark) (width 80))
  "Shorthand for render-markdown."
  (render-markdown text :style style :width width))
