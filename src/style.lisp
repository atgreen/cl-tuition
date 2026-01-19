;;; style.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Style system - terminal styling and formatting inspired by Lipgloss

(in-package #:tuition)

;;; ANSI color codes
(defun ansi-color (code)
  "Generate ANSI escape code for a color."
  (format nil "~C[~Am" #\Escape code))

(defun ansi-reset ()
  "Reset all ANSI formatting."
  (format nil "~C[0m" #\Escape))

;;; Extended color support

(defun color-256 (n &key (foreground t))
  "Create a 256-color code (0-255).
   Set foreground to nil for background colors."
  (format nil "~A;5;~D" (if foreground "38" "48") n))

(defun color-rgb (r g b &key (foreground t))
  "Create an RGB true color code (0-255 for each component).
   Set foreground to nil for background colors."
  (format nil "~A;2;~D;~D;~D" (if foreground "38" "48") r g b))

(defun parse-hex-color (hex &key (foreground t))
  "Parse a hex color like #FF0000 or #F00 to RGB ANSI code."
  (let ((clean (string-trim '(#\#) hex)))
    (cond
      ;; 6-digit hex: #RRGGBB
      ((= (length clean) 6)
       (let ((r (parse-integer (subseq clean 0 2) :radix 16))
             (g (parse-integer (subseq clean 2 4) :radix 16))
             (b (parse-integer (subseq clean 4 6) :radix 16)))
         (color-rgb r g b :foreground foreground)))

      ;; 3-digit hex: #RGB -> #RRGGBB
      ((= (length clean) 3)
       (let ((r (parse-integer (string (char clean 0)) :radix 16))
             (g (parse-integer (string (char clean 1)) :radix 16))
             (b (parse-integer (string (char clean 2)) :radix 16)))
         (color-rgb (* r 17) (* g 17) (* b 17) :foreground foreground)))

      (t (error "Invalid hex color: ~A" hex)))))

;;; Adaptive colors (for light/dark backgrounds)

(defclass adaptive-color ()
  ((light :initarg :light :accessor adaptive-light
          :documentation "Color for light backgrounds")
   (dark :initarg :dark :accessor adaptive-dark
         :documentation "Color for dark backgrounds"))
  (:documentation "Color that adapts to terminal background."))

(defun make-adaptive-color (light dark)
  "Create an adaptive color that changes based on terminal background."
  (make-instance 'adaptive-color :light light :dark dark))

;;; Complete colors (specify exact values for different color profiles)

(defclass complete-color ()
  ((truecolor :initarg :truecolor :reader complete-truecolor
              :initform nil
              :documentation "24-bit RGB color (hex string like #FF0000)")
   (ansi256 :initarg :ansi256 :reader complete-ansi256
            :initform nil
            :documentation "256-color ANSI code (0-255)")
   (ansi :initarg :ansi :reader complete-ansi
         :initform nil
         :documentation "16-color ANSI code (0-15 or ANSI code string)"))
  (:documentation "Complete color specification with values for each color profile."))

(defun make-complete-color (&key truecolor ansi256 ansi)
  "Create a complete color with specific values for different terminal profiles."
  (make-instance 'complete-color
                 :truecolor truecolor
                 :ansi256 ansi256
                 :ansi ansi))

(defclass complete-adaptive-color ()
  ((light :initarg :light :reader complete-adaptive-light
          :documentation "Complete color for light backgrounds")
   (dark :initarg :dark :reader complete-adaptive-dark
         :documentation "Complete color for dark backgrounds"))
  (:documentation "Adaptive color using complete color specifications."))

(defun make-complete-adaptive-color (light dark)
  "Create an adaptive color with complete color specs for light and dark backgrounds."
  (make-instance 'complete-adaptive-color :light light :dark dark))

(defun detect-dark-background ()
  "Attempt to detect if terminal has dark background.
   Returns T for dark, NIL for light.

   Checks multiple environment variables and known terminals."
  ;; Check COLORFGBG environment variable (used by some terminals)
  ;; Format is usually "foreground;background" with color numbers
  (let ((colorfgbg (uiop:getenv "COLORFGBG")))
    (when colorfgbg
      (let ((parts (uiop:split-string colorfgbg :separator ";")))
        (when (>= (length parts) 2)
          ;; Last number is background color
          (let ((bg (parse-integer (car (last parts)) :junk-allowed t)))
            (when bg
              ;; 0-7 are dark colors, 8-15 are light colors in ANSI
              (return-from detect-dark-background (< bg 8))))))))

  ;; Check for GNOME Terminal (usually dark by default)
  (when (uiop:getenv "GNOME_TERMINAL_SCREEN")
    (return-from detect-dark-background t))

  ;; Check TERM_PROGRAM for known terminals with defaults
  (let ((term-program (uiop:getenv "TERM_PROGRAM")))
    (when term-program
      (cond
        ((search "iTerm" term-program) (return-from detect-dark-background t))
        ((search "Apple_Terminal" term-program) (return-from detect-dark-background nil))
        ((search "vscode" term-program) (return-from detect-dark-background t)))))

  ;; Default assumption: most modern terminals use dark backgrounds
  t)

(defun detect-color-support ()
  "Detect the color support level of the terminal.
   Returns :truecolor, :256color, :16color, or :monochrome."
  (cond
    ;; Check for true color support
    ((or (equal (uiop:getenv "COLORTERM") "truecolor")
         (equal (uiop:getenv "COLORTERM") "24bit"))
     :truecolor)

    ;; Check TERM variable for 256 color
    ((let ((term (uiop:getenv "TERM")))
       (when term
         (or (search "256color" term)
             (search "256colour" term))))
     :256color)

    ;; Check for basic ANSI color support
    ((let ((term (uiop:getenv "TERM")))
       (when term
         (or (search "color" term)
             (search "ansi" term)
             (search "xterm" term))))
     :16color)

    ;; Fallback to monochrome
    (t :monochrome)))

(defun resolve-color (color)
  "Resolve a color to an ANSI code string based on terminal capabilities.
   Handles complete-color objects by selecting the best available option."
  (cond
    ;; Complete color - select based on terminal capability
    ((typep color 'complete-color)
     (let ((profile (detect-color-support)))
       (case profile
         (:truecolor
          (or (when (complete-truecolor color)
                (parse-hex-color (complete-truecolor color)))
              (when (complete-ansi256 color)
                (color-256 (complete-ansi256 color)))
              (complete-ansi color)))
         (:256color
          (or (when (complete-ansi256 color)
                (color-256 (complete-ansi256 color)))
              (complete-ansi color)))
         (t (complete-ansi color)))))

    ;; Regular string color
    (t color)))

(defun resolve-adaptive-color (color)
  "Resolve an adaptive color to actual color based on terminal background."
  (cond
    ;; Complete adaptive color
    ((typep color 'complete-adaptive-color)
     (let ((selected (if (detect-dark-background)
                         (complete-adaptive-dark color)
                         (complete-adaptive-light color))))
       (resolve-color selected)))

    ;; Simple adaptive color
    ((typep color 'adaptive-color)
     (if (detect-dark-background)
         (adaptive-dark color)
         (adaptive-light color)))

    ;; Complete color or regular color
    (t (resolve-color color))))

;;; Ensure color codes are in the correct foreground/background form

(defun %maybe-parse-int (s)
  (ignore-errors (parse-integer s :junk-allowed t)))

(defun %replace-first (str from to)
  (let ((pos (search from str)))
    (if pos
        (concatenate 'string (subseq str 0 pos) to (subseq str (+ pos (length from))))
        str)))

(defun as-foreground-code (code)
  "Normalize CODE so it represents a foreground SGR parameter."
  (cond
    ((null code) nil)
    ((not (stringp code)) code)
    ;; truecolor 48;2 -> 38;2
    ((search "48;2;" code) (%replace-first code "48;2;" "38;2;"))
    ;; otherwise coerce numeric background to foreground
    (t
     (let ((n (%maybe-parse-int code)))
       (cond
         ((and n (<= 40 n) (<= n 47)) (format nil "~D" (- n 10)))
         ((and n (<= 100 n) (<= n 107)) (format nil "~D" (- n 10)))
         (t code))))))

(defun as-background-code (code)
  "Normalize CODE so it represents a background SGR parameter."
  (cond
    ((null code) nil)
    ((not (stringp code)) code)
    ;; truecolor 38;2 -> 48;2
    ((search "38;2;" code) (%replace-first code "38;2;" "48;2;"))
    ;; otherwise coerce numeric foreground to background
    (t
     (let ((n (%maybe-parse-int code)))
       (cond
         ((and n (<= 30 n) (<= n 37)) (format nil "~D" (+ n 10)))
         ((and n (<= 90 n) (<= n 97)) (format nil "~D" (+ n 10)))
         (t code))))))

;;; Foreground colors (30-37, 90-97)
(defparameter *fg-black* "30")
(defparameter *fg-red* "31")
(defparameter *fg-green* "32")
(defparameter *fg-yellow* "33")
(defparameter *fg-blue* "34")
(defparameter *fg-magenta* "35")
(defparameter *fg-cyan* "36")
(defparameter *fg-white* "37")

(defparameter *fg-bright-black* "90")
(defparameter *fg-bright-red* "91")
(defparameter *fg-bright-green* "92")
(defparameter *fg-bright-yellow* "93")
(defparameter *fg-bright-blue* "94")
(defparameter *fg-bright-magenta* "95")
(defparameter *fg-bright-cyan* "96")
(defparameter *fg-bright-white* "97")

;;; Background colors (40-47, 100-107)
(defparameter *bg-black* "40")
(defparameter *bg-red* "41")
(defparameter *bg-green* "42")
(defparameter *bg-yellow* "43")
(defparameter *bg-blue* "44")
(defparameter *bg-magenta* "45")
(defparameter *bg-cyan* "46")
(defparameter *bg-white* "47")

(defparameter *bg-bright-black* "100")
(defparameter *bg-bright-red* "101")
(defparameter *bg-bright-green* "102")
(defparameter *bg-bright-yellow* "103")
(defparameter *bg-bright-blue* "104")
(defparameter *bg-bright-magenta* "105")
(defparameter *bg-bright-cyan* "106")
(defparameter *bg-bright-white* "107")

;;; Text attributes
(defparameter *bold* "1")
(defparameter *dim* "2")
(defparameter *italic* "3")
(defparameter *underline* "4")
(defparameter *blink* "5")
(defparameter *reverse* "7")
(defparameter *hidden* "8")
(defparameter *strikethrough* "9")

;;; Style class
(defclass style ()
  ((foreground :initform nil :accessor style-foreground)
   (background :initform nil :accessor style-background)
   (bold :initform nil :accessor style-bold)
   (italic :initform nil :accessor style-italic)
   (underline :initform nil :accessor style-underline)
   (blink :initform nil :accessor style-blink)
   (reverse :initform nil :accessor style-reverse)
   (strikethrough :initform nil :accessor style-strikethrough)
   (faint :initform nil :accessor style-faint)
   (padding-left :initform 0 :accessor style-padding-left)
   (padding-right :initform 0 :accessor style-padding-right)
   (padding-top :initform 0 :accessor style-padding-top)
   (padding-bottom :initform 0 :accessor style-padding-bottom)
   (margin-left :initform 0 :accessor style-margin-left)
   (margin-right :initform 0 :accessor style-margin-right)
   (margin-top :initform 0 :accessor style-margin-top)
   (margin-bottom :initform 0 :accessor style-margin-bottom)
   (width :initform nil :accessor style-width)
   (height :initform nil :accessor style-height)
   (max-width :initform nil :accessor style-max-width)
   (max-height :initform nil :accessor style-max-height)
   (inline :initform nil :accessor style-inline)
   (align :initform :left :accessor style-align))
  (:documentation "A style definition for terminal output."))

(defun make-style (&key foreground background bold italic underline
                       blink reverse strikethrough faint
                       (padding 0) padding-left padding-right padding-top padding-bottom
                       (margin 0) margin-left margin-right margin-top margin-bottom
                       width height max-width max-height inline align)
  "Create a new style with the given attributes."
  (let ((s (make-instance 'style)))
    (when foreground (setf (style-foreground s) foreground))
    (when background (setf (style-background s) background))
    (when bold (setf (style-bold s) bold))
    (when italic (setf (style-italic s) italic))
    (when underline (setf (style-underline s) underline))
    (when blink (setf (style-blink s) blink))
    (when reverse (setf (style-reverse s) reverse))
    (when strikethrough (setf (style-strikethrough s) strikethrough))
    (when faint (setf (style-faint s) faint))

    ;; Handle padding shorthand
    (setf (style-padding-left s) (or padding-left padding))
    (setf (style-padding-right s) (or padding-right padding))
    (setf (style-padding-top s) (or padding-top padding))
    (setf (style-padding-bottom s) (or padding-bottom padding))

    ;; Handle margin shorthand
    (setf (style-margin-left s) (or margin-left margin))
    (setf (style-margin-right s) (or margin-right margin))
    (setf (style-margin-top s) (or margin-top margin))
    (setf (style-margin-bottom s) (or margin-bottom margin))

    (when width (setf (style-width s) width))
    (when height (setf (style-height s) height))
    (when max-width (setf (style-max-width s) max-width))
    (when max-height (setf (style-max-height s) max-height))
    (when inline (setf (style-inline s) inline))
    (when align (setf (style-align s) align))
    s))

(defun render-styled (style text)
  "Render TEXT with STYLE, ensuring ANSI colors cover the whole block.

The previous implementation applied ANSI escape codes before padding,
width/alignment and height adjustments. That meant only the glyphs in
TEXT were styled, while layout spaces added later (for padding or centering)
were left unstyled — so background colors didn’t fill boxes. We instead
compute the final block first, then wrap it with ANSI codes."
  (let ((result text))
    ;; Handle inline mode - force single line, ignore margins/padding/borders
    (when (style-inline style)
      (setf result (car (split-string-by-newline result))))

    ;; Pre-wrap: if a width is set, wrap text to (width - horizontal padding)
    ;; Note: normalize-spaces is nil to preserve intentional spacing in content
    (when (and (not (style-inline style)) (style-width style))
      (let ((wrap-at (- (style-width style)
                        (style-padding-left style)
                        (style-padding-right style))))
        (when (> wrap-at 0)
          (setf result (wrap-text result wrap-at :break-words nil :normalize-spaces nil)))))

    ;; Apply padding (unless inline)
    (when (and (not (style-inline style))
               (or (> (style-padding-left style) 0)
                   (> (style-padding-right style) 0)
                   (> (style-padding-top style) 0)
                   (> (style-padding-bottom style) 0)))
      (setf result (apply-padding result style)))

    ;; Apply width and alignment
    (when (style-width style)
      (setf result (apply-width-align result style)))

    ;; Apply minimum height
    (when (style-height style)
      (setf result (apply-min-height result (style-height style))))

    ;; Defensive clamp: ensure no line exceeds the requested width even if
    ;; upstream alignment/padding introduced extra spaces due to ANSI quirks.
    (when (style-width style)
      (setf result (truncate-to-width result (style-width style))))

    ;; If underline is requested, apply it only to non-space text, not padding.
    (when (style-underline style)
      (setf result (apply-underline-to-text-only result)))

    ;; Finally, collect and apply ANSI codes PER LINE so they wrap each line's
    ;; content (padding/width/height), but NOT the margins. This ensures that
    ;; when margins are applied later, they appear as unstyled spaces because
    ;; each line ends with a reset code before its margin.
    ;; Note: underline is handled above per-text; do not include it here.
    (let ((codes '()))
      (when (style-bold style) (push *bold* codes))
      (when (style-faint style) (push *dim* codes))
      (when (style-italic style) (push *italic* codes))
      (when (style-blink style) (push *blink* codes))
      (when (style-reverse style) (push *reverse* codes))
      (when (style-strikethrough style) (push *strikethrough* codes))
      ;; Resolve adaptive colors
      (when (style-foreground style)
        (push (as-foreground-code (resolve-adaptive-color (style-foreground style))) codes))
      (when (style-background style)
        (push (as-background-code (resolve-adaptive-color (style-background style))) codes))

      (setf result (if codes
                       (let* ((seq (format nil "~C[~{~A~^;~}m" #\Escape (nreverse codes)))
                              (rst (ansi-reset))
                              (rst+seq (concatenate 'string rst seq))
                              (lines (split-string-by-newline result)))
                         ;; Apply codes per-line: ESC[codes]<line>ESC[0m
                         ;; Re-apply the outer style after any inner reset so
                         ;; backgrounds continue across styled child content.
                         (format nil "~{~A~^~%~}"
                                 (mapcar (lambda (line)
                                           (let ((pos 0)
                                                 (len (length line))
                                                 (reapplied ""))
                                             (setf reapplied
                                                   (with-output-to-string (s)
                                                     (loop for idx = (search rst line :start2 pos)
                                                           while idx do
                                                             (write-string (subseq line pos idx) s)
                                                             (write-string rst+seq s)
                                                             (setf pos (+ idx (length rst))))
                                                     (when (< pos len)
                                                       (write-string (subseq line pos) s))))
                                             (format nil "~A~A~A" seq reapplied rst)))
                                         lines)))
                       result)))

    ;; Apply margins last (and unstyled) so they appear as plain spacing
    ;; around the styled content.
    (when (and (not (style-inline style))
               (or (> (style-margin-left style) 0)
                   (> (style-margin-right style) 0)
                   (> (style-margin-top style) 0)
                   (> (style-margin-bottom style) 0)))
      (setf result (apply-margin result style)))

    ;; Apply MaxWidth/MaxHeight at the very end (ANSI-aware)
    (when (style-max-width style)
      (setf result (truncate-to-width result (style-max-width style))))
    (when (style-max-height style)
      (setf result (truncate-to-height result (style-max-height style))))

    result))

;; Insert underline on/off only around non-space text on each line.
(defun apply-underline-to-text-only (text)
  (let ((on (ansi-color *underline*))
        (off (ansi-color "24")))
    (format nil "~{~A~^~%~}"
            (mapcar (lambda (line)
                      (let* ((len (length line))
                             (start (position-if (lambda (c) (not (char= c #\Space))) line))
                             ;; Find last non-space character index
                             (last-ns (when start (position-if (lambda (c) (not (char= c #\Space))) line :from-end t)))
                             (end (when last-ns (1+ last-ns))))
                        (if (and start end (> end start))
                            (concatenate 'string
                                         (subseq line 0 start)
                                         on
                                         (subseq line start end)
                                         off
                                         (subseq line end))
                            line)))
                    (split-string-by-newline text)))))

(defun apply-padding (text style)
  "Apply padding to TEXT. Adds left/right spaces and blank lines for top/bottom.

This version preserves each line's natural width (no global expansion),
which avoids runaway trailing spaces when composing large documents."
  (let* ((lines (split-string-by-newline text))
         (pad-left (make-string (style-padding-left style) :initial-element #\Space))
         (pad-right (make-string (style-padding-right style) :initial-element #\Space))
         (padded-lines (mapcar (lambda (line)
                                 (format nil "~A~A~A" pad-left line pad-right))
                              lines)))
    (with-output-to-string (s)
      ;; Top padding: blank lines only
      (dotimes (i (style-padding-top style)) (format s "~%"))
      ;; Content
      (format s "~{~A~^~%~}" padded-lines)
      ;; Bottom padding: blank lines only
      (dotimes (i (style-padding-bottom style)) (format s "~%")))))

(defun apply-width-align (text style)
  "Apply width and alignment to text. If content exceeds width, truncate."
  (let* ((lines (split-string-by-newline text))
         (width (style-width style))
         (align (style-align style)))
    (format nil "~{~A~^~%~}"
            (mapcar (lambda (line)
                      (let* ((visible-len (visible-length line)))
                        (cond
                          ((or (null width) (<= width 0)) "")
                          ((> visible-len width)
                           (truncate-text line width :ellipsis ""))
                          (t
                           (let* ((padding (max 0 (- width visible-len)))
                                  (aligned (case align
                                             (:left (format nil "~A~A" line (make-string padding :initial-element #\Space)))
                                             (:right (format nil "~A~A" (make-string padding :initial-element #\Space) line))
                                             (:center (let* ((left-pad (floor padding 2))
                                                             (right-pad (- padding left-pad)))
                                                        (format nil "~A~A~A"
                                                                (make-string left-pad :initial-element #\Space)
                                                                line
                                                                (make-string right-pad :initial-element #\Space))))
                                             (t line))))
                             ;; Final guard: never exceed width, even if alignment produced
                             ;; extra spaces due to ANSI/measurement quirks.
                             (if (> (visible-length aligned) width)
                                 (truncate-text aligned width :ellipsis "")
                                 aligned))))))
                    lines))))

(defun apply-margin (text style)
  "Apply margins to text."
  (let* ((lines (split-string-by-newline text))
         (margin-left (make-string (style-margin-left style) :initial-element #\Space))
         (margin-right (make-string (style-margin-right style) :initial-element #\Space))
         (margined-lines (mapcar (lambda (line)
                                   (format nil "~A~A~A" margin-left line margin-right))
                                lines)))
    (with-output-to-string (s)
      ;; Top margin
      (dotimes (i (style-margin-top style))
        (format s "~%"))
      ;; Content
      (format s "~{~A~^~%~}" margined-lines)
      ;; Bottom margin
      (dotimes (i (style-margin-bottom style))
        (format s "~%")))))

(defun truncate-to-width (text max-width)
  "Truncate each line of text to max-width columns."
  (let ((lines (split-string-by-newline text)))
    (format nil "~{~A~^~%~}"
            (mapcar (lambda (line)
                      (truncate-text line max-width :ellipsis ""))
                    lines))))

(defun truncate-to-height (text max-height)
  "Truncate text to max-height lines."
  (let ((lines (split-string-by-newline text)))
    (if (<= (length lines) max-height)
        text
        (format nil "~{~A~^~%~}" (subseq lines 0 max-height)))))

(defun apply-min-height (text min-height)
  "Ensure TEXT has at least MIN-HEIGHT lines by adding space-filled lines.

Lines added are padded to the current block width so backgrounds render."
  (let* ((lines (split-string-by-newline text))
         (current-height (length lines)))
    (if (>= current-height min-height)
        text
        (let* ((maxw (if lines (apply #'max (mapcar #'visible-length lines)) 0))
               (needed (- min-height current-height))
               (blank (make-string maxw :initial-element #\Space)))
          (with-output-to-string (s)
            (format s "~{~A~^~%~}" lines)
            (dotimes (i needed)
              (format s "~%~A" blank)))))))

;;; Utility functions

(defun split-string-by-newline (str)
  "Split a string by newlines."
  (let ((input (or str ""))
        (result '())
        (start 0))
    (flet ((strip-cr (line)
             (if (and (> (length line) 0)
                      (char= (char line (1- (length line))) #\Return))
                 (subseq line 0 (1- (length line)))
                 line)))
      (loop for pos = (position #\Newline input :start start)
          while pos
          do (push (strip-cr (subseq input start pos)) result)
             (setf start (1+ pos))
          finally (push (strip-cr (subseq input start)) result)))
    (nreverse result)))

(defun %code-in-range-p (code ranges)
  (loop for (a b) in ranges
        thereis (and (<= a code) (<= code b))))

(defun %combining-char-p (code)
  "Return T if CODE is a combining/zero-width mark.
This is a minimal set covering common Unicode combining ranges and Arabic vowel marks."
  (%code-in-range-p code
                    '((#x0300 #x036F)  ; Combining Diacritical Marks
                      (#x0483 #x0489)  ; Cyrillic Combining
                      (#x0591 #x05BD)  ; Hebrew
                      (#x05BF #x05BF)
                      (#x05C1 #x05C2)
                      (#x05C4 #x05C5)
                      (#x05C7 #x05C7)
                      (#x0610 #x061A)  ; Arabic
                      (#x064B #x065F)
                      (#x0670 #x0670)
                      (#x06D6 #x06DC)
                      (#x06DF #x06E4)
                      (#x06E7 #x06E8)
                      (#x06EA #x06ED)
                      (#x200B #x200F)  ; zero‑width / bidi marks
                      (#x202A #x202E)
                      (#x2060 #x2064)
                      (#x2066 #x2069)  ; LRI/RLI/FSI/PDI (bidi isolates)
                      (#x20D0 #x20FF)  ; Combining Diacritical Marks for Symbols
                      (#xFE20 #xFE2F)))) ; Combining Half Marks

(defun %east-asian-wide-p (code)
  "Return T if CODE should be treated as double-width in monospaced terminals.
This is a pragmatic subset covering CJK, Hangul, Hiragana/Katakana, and fullwidth forms."
  (%code-in-range-p code
                    '((#x1100 #x115F)  ; Hangul Jamo init. consonants
                      (#x231A #x231B)  ; ⌚⌛ and similar
                      (#x2329 #x232A)
                      (#x23E9 #x23EC)
                      (#x23F0 #x23F3)
                      (#x25FD #x25FE)
                      (#x2600 #x2605)
                      (#x2614 #x2615)
                      (#x2648 #x2653)
                      (#x2E80 #x2FFF)  ; CJK Radicals/Symbols
                      (#x3000 #x303F)  ; CJK punctuation + IDEOGRAPHIC SPACE
                      (#x3040 #x309F)  ; Hiragana
                      (#x30A0 #x30FF)  ; Katakana
                      (#x3100 #x312F)  ; Bopomofo
                      (#x3130 #x318F)  ; Hangul Compatibility Jamo
                      (#x31A0 #x31EF)  ; Bopomofo ext / Katakana phonetic ext
                      (#x3200 #x32FF)  ; Enclosed CJK
                      (#x3400 #x4DBF)  ; CJK Ext A
                      (#x4E00 #x9FFF)  ; CJK Unified Ideographs
                      (#xA960 #xA97F)  ; Hangul Jamo Extended-A
                      (#xAC00 #xD7A3)  ; Hangul Syllables
                      (#xF900 #xFAFF)  ; CJK Compatibility Ideographs
                      (#xFE10 #xFE19)
                      (#xFE30 #xFE6B)  ; CJK Compatibility Forms etc
                      (#xFF01 #xFF60)  ; Fullwidth ASCII variants
                      (#xFFE0 #xFFE6)  ; Fullwidth symbol variants
                      (#x1F300 #x1F64F) ; emoji (subset, treat as wide)
                      (#x1F900 #x1F9FF))))

(defun %char-display-width (ch)
  "Return display width of character CH in columns: 0, 1, or 2."
  (let ((code (char-code ch)))
    (cond
      ;; control characters (except newline handled elsewhere)
      ((< code 32) 0)
      ((= code #x7F) 0)
      ((%combining-char-p code) 0)
      ((%east-asian-wide-p code) 2)
      (t 1))))

(defun visible-length (str)
  "Calculate visible display width of STR, excluding ANSI escapes and accounting for wide/combining chars."
  (let ((result 0)
        (i 0)
        (len (length str)))
    (loop while (< i len) do
      (let ((char (char str i)))
        (cond
          ;; Start of ESC sequence
          ((char= char #\Escape)
           ;; Skip entire CSI sequence: ESC [ ... <final-byte>
           ;; Final byte is in range 0x40-0x7E (includes both 'm' and 'z')
           (incf i)  ; skip ESC
           (when (and (< i len) (char= (char str i) #\[))
             (incf i)  ; skip [
             ;; Skip parameter bytes and intermediate bytes until final byte
             (loop while (< i len) do
               (let ((code (char-code (char str i))))
                 (incf i)
                 ;; Final byte range: 0x40-0x7E (@A-Z[\]^_`a-z{|}~)
                 (when (and (>= code #x40) (<= code #x7E))
                   (return))))))
          ;; Regular character - count its display width
          (t
           (incf result (%char-display-width char))
           (incf i)))))
    result))

;;; Bidi helpers
(defun bidi-isolate (text)
  "Wrap TEXT in Unicode bidi isolate markers (FSI ... PDI) to prevent
mixing LTR/RTL from affecting surrounding separators/borders."
  (let ((fsi (string (code-char #x2068)))
        (pdi (string (code-char #x2069))))
    (format nil "~A~A~A" fsi text pdi)))

(defun bidi-isolate-ltr (text)
  "Wrap TEXT in LTR isolate (LRI ... PDI) and append LRM inside to keep
trailing padding/separators in LTR context."
  (let ((lri (string (code-char #x2066)))
        (lrm (string (code-char #x200E)))
        (pdi (string (code-char #x2069))))
    (format nil "~A~A~A~A" lri text lrm pdi)))

;;; Convenience functions

(defun bold (text)
  "Render text in bold."
  (render-styled (make-style :bold t) text))

(defun italic (text)
  "Render text in italic."
  (render-styled (make-style :italic t) text))

(defun underline (text)
  "Render text underlined."
  (render-styled (make-style :underline t) text))

(defun colored (text &key fg bg)
  "Render text with foreground and/or background color."
  (render-styled (make-style :foreground fg :background bg) text))

;;; Reflow utilities — wrapping, truncation, indentation
;; Implemented here to ensure availability with core styling utilities.

;; Internal tokenization that preserves ANSI escapes
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
               (setf i j)))))))
    (nreverse out)))

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
        ((%newline-tokenp tkn) (push (%rtok-text tkn) out))
        (t
         (let* ((txt (%rtok-text tkn))
                (tw (visible-length txt)))
           (cond
             ((<= (+ w tw) budget)
              (incf w tw)
              (push txt out))
             (t
              (let ((need (max 0 (- budget w))))
                (when (> need 0)
                  (let ((acc "") (seen 0))
                    (loop for ch across txt until (>= seen need) do
                      (incf seen)
                      (setf acc (concatenate 'string acc (string ch))))
                    (push acc out)))
                (setf truncated t)
                (return))))))))
    (let ((s (apply #'concatenate 'string (nreverse out))))
      (if truncated (concatenate 'string s ellipsis) s))))

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
               ((<= (+ sp ww) room)
                (when (and need-space (> sp 0)) (add-text " ") (incf w 1))
                (add-text word)
                (incf w ww)
                (setf need-space nil))
               ((and break-words (> ww maxw))
                (let ((remaining word)
                      (first t))
                  (loop while (> (visible-length remaining) 0) do
                    (let* ((take (min (visible-length remaining) (- maxw (if first w 0))))
                           (acc "") (seen 0))
                      (when (and need-space (<= 1 (- maxw w)))
                        (add-text " ") (incf w 1) (setf need-space nil))
                      (loop for ch across remaining until (>= seen take) do
                        (incf seen) (setf acc (concatenate 'string acc (string ch))))
                      (add-text acc)
                      (setf remaining (subseq remaining seen))
                      (cond
                        ((= (visible-length remaining) 0)
                         (incf w seen))
                        (t
                         (emit-line)))))))
               (t
                (emit-line)
                (ensure-indent)
                (add-text word)
                (incf w ww)
                (setf need-space nil)))))))
      (when (or (not (string= line "")) (= (length out) 0))
        (push line out))
      (format nil "~{~A~^~%~}" (nreverse out)))))

;;; Measurement utilities

(defun width (text)
  "Get the width (in columns) of rendered text.
   Accounts for ANSI escape sequences and returns the width of the widest line."
  (let ((lines (split-string-by-newline text)))
    (if lines
        (loop for line in lines
              maximize (visible-length line))
        0)))

(defun height (text)
  "Get the height (in lines) of rendered text."
  (let ((lines (split-string-by-newline text)))
    (length lines)))

(defun size (text)
  "Get both width and height of rendered text.
   Returns (values width height)."
  (values (width text) (height text)))
