;;; reflow.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Reflow utilities — wrapping, truncation, indentation

(in-package #:tuition)

;;; Internal tokenization that preserves ANSI escapes

(defstruct (%rtok (:constructor %mk-rtok))
  type   ; :ansi | :space | :word | :newline
  text)

(defun %ansi-tokenp (tok) (eql (%rtok-type tok) :ansi))
(defun %space-tokenp (tok) (eql (%rtok-type tok) :space))
(defun %word-tokenp (tok) (eql (%rtok-type tok) :word))
(defun %newline-tokenp (tok) (eql (%rtok-type tok) :newline))

(defun %whitespacep (c)
  (or (char= c #\Space)
      (char= c #\Tab)
      (char= c #\Page)
      (char= c #\Return)))

(defun %tokenize (str)
  "Tokenize STR into ANSI sequences, words, spaces, and newlines.
ANSI escape sequences are preserved but not counted for width."
  (let ((out '())
        (i 0)
        (n (length str)))
    (labels ((push-token (type start end)
               (when (< start end)
                 (push (%mk-rtok :type type :text (subseq str start end)) out))))
      (loop while (< i n) do
        (let ((ch (char str i)))
          (cond
            ;; newline
            ((char= ch #\Newline)
             (push (%mk-rtok :type :newline :text (string ch)) out)
             (incf i))
            ;; ANSI escape sequence: ESC ... m
            ((char= ch #\Escape)
             (let ((j (1+ i)))
               (loop while (and (< j n)
                                (not (char= (char str j) #\m)))
                     do (incf j))
               (when (< j n) (incf j))
               (push-token :ansi i j)
               (setf i j)))
            ;; whitespace run
            ((%whitespacep ch)
             (let ((j i))
               (loop while (and (< j n)
                                (%whitespacep (char str j)))
                     do (incf j))
               (push-token :space i j)
               (setf i j)))
            ;; word run
            (t
             (let ((j i))
               (loop while (and (< j n)
                                (let ((c (char str j)))
                                  (and (not (%whitespacep c))
                                       (not (char= c #\Newline))
                                       (not (char= c #\Escape)))))
                     do (incf j))
               (push-token :word i j)
               (setf i j))))))
    (nreverse out))))

;;; Public API

(defun indent-lines (text n)
  "Indent each line of TEXT by N spaces."
  (let* ((pad (make-string (max 0 n) :initial-element #\Space))
         (lines (split-string-by-newline text)))
    (format nil "~{~A~^~%~}"
            (mapcar (lambda (line) (concatenate 'string pad line)) lines))))

(defun truncate-text (text width &key (ellipsis "…"))
  "Truncate TEXT to WIDTH visible columns, preserving ANSI sequences.
If truncation occurs, append ELLIPSIS (default: …)."
  (let* ((tokens (%tokenize text))
         (maxw (max 0 width))
         (ellw (visible-length ellipsis))
         (budget (max 0 (- maxw ellw)))
         (out '())
         (w 0)
         (truncated nil))
    (dolist (tkn tokens)
      (cond
        ((%ansi-tokenp tkn) (push (%rtok-text tkn) out))
        ((%newline-tokenp tkn)
         (push (%rtok-text tkn) out))
        (t
         (let* ((txt (%rtok-text tkn))
                (tw (visible-length txt)))
           (cond
             ((<= (+ w tw) budget)
              (incf w tw)
              (push txt out))
             (t
              ;; need partial slice
              (let ((need (max 0 (- budget w))))
                (when (> need 0)
                  ;; copy characters up to NEED visible width
                  (let ((acc "") (seen 0))
                    (loop for ch across txt until (>= seen need) do
                      (incf seen)
                      (setf acc (concatenate 'string acc (string ch))))
                    (push acc out)))
                (setf truncated t)
                (return))))))))
    (let ((s (apply #'concatenate 'string (nreverse out))))
      (if truncated
          (concatenate 'string s ellipsis)
          s)))

(defun ellipsize (text width)
  "Convenience wrapper around TRUNCATE-TEXT with default ellipsis."
  (truncate-text text width :ellipsis "…"))

(defun wrap-text (text width &key (break-words nil) (normalize-spaces t) (indent 0) (continuation-indent 0))
  "Wrap TEXT to WIDTH columns, preserving ANSI sequences.
Options:
- BREAK-WORDS: if true, split words longer than WIDTH.
- NORMALIZE-SPACES: collapse runs of spaces into single spaces between words.
- INDENT: spaces at the start of the first line.
- CONTINUATION-INDENT: spaces at the start of wrapped lines."
  (let* ((tokens (%tokenize text))
         (maxw (max 1 width))
         (first-indent (max 0 indent))
         (cont-indent (max 0 continuation-indent))
         (line "")
         (out '())
         (w 0)
         (current-indent first-indent)
         (need-space nil))
    (labels ((emit-line ()
               (push line out)
               (setf line "" w 0 need-space nil current-indent cont-indent))
             (add-text (txt)
               (setf line (concatenate 'string line txt)))
             (ensure-indent ()
               (when (> current-indent 0)
                 (add-text (make-string current-indent :initial-element #\Space))
                 (incf w current-indent)
                 (setf current-indent 0))))
      (dolist (tkn tokens)
        (cond
          ((%ansi-tokenp tkn) (add-text (%rtok-text tkn)))
          ((%newline-tokenp tkn) (emit-line))
          ((%space-tokenp tkn)
           (when (and (not need-space) (not (string= line "")))
             (setf need-space t)))
          ((%word-tokenp tkn)
           (let* ((word (%rtok-text tkn))
                  (ww (visible-length word))
                  (sp (if (and need-space (not normalize-spaces)) 1 (if need-space 1 0)))
                  (room (- maxw w)))
             (ensure-indent)
             (cond
               ;; fits as is, include preceding space if any
               ((<= (+ sp ww) room)
                (when (and need-space (> sp 0)) (add-text " ") (incf w 1))
                (add-text word)
                (incf w ww)
                (setf need-space nil))
               ;; word longer than line width
               ((and break-words (> ww maxw))
                (let ((remaining word)
                      (first t))
                  (loop while (> (visible-length remaining) 0) do
                    (let* ((take (min (visible-length remaining) (- maxw (if first w 0))))
                           (acc "") (seen 0))
                      (when (and need-space (<= 1 (- maxw w)))
                        (add-text " ") (incf w 1) (setf need-space nil))
                      ;; slice TAKE chars
                      (loop for ch across remaining until (>= seen take) do
                        (incf seen) (setf acc (concatenate 'string acc (string ch))))
                      (add-text acc)
                      (setf remaining (subseq remaining seen))
                      (cond
                        ((= (visible-length remaining) 0)
                         (incf w seen))
                        (t
                         (emit-line)))))))
               ;; move to next line
               (t
                (emit-line)
                (ensure-indent)
                (add-text word)
                (incf w ww)
                (setf need-space nil)))))))
      ;; flush last line
      (when (or (not (string= line "")) (= (length out) 0))
        (push line out))
      (format nil "~{~A~^~%~}" (nreverse out))))))
