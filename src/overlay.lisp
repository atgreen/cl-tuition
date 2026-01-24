;;; overlay.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Overlay compositing - layer foreground text blocks on top of backgrounds
;;;
;;; Inspired by github.com/rmhubbert/bubbletea-overlay

(in-package #:tuition)

;;; ANSI-aware string manipulation for compositing

(defun ansi-take-columns (str columns)
  "Take the first COLUMNS visible columns from STR, preserving ANSI codes.
Returns the substring with proper ANSI handling."
  (when (or (null str) (zerop (length str)))
    (return-from ansi-take-columns ""))
  (when (<= columns 0)
    (return-from ansi-take-columns ""))

  (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (i 0)
        (len (length str))
        (visible-cols 0))
    (loop while (and (< i len) (< visible-cols columns)) do
      (let ((char (char str i)))
        (cond
          ;; ANSI escape sequence - include in output but don't count width
          ((char= char #\Escape)
           (vector-push-extend char result)
           (incf i)
           ;; Copy until we hit the final byte (0x40-0x7E range)
           (when (and (< i len) (char= (char str i) #\[))
             (vector-push-extend (char str i) result)
             (incf i)
             (loop while (< i len) do
               (let* ((c (char str i))
                      (code (char-code c)))
                 (vector-push-extend c result)
                 (incf i)
                 ;; Final byte in range 0x40-0x7E ends the sequence
                 (when (and (>= code #x40) (<= code #x7E))
                   (return))))))
          ;; Regular character
          (t
           (let ((char-width (%char-display-width char)))
             ;; Check if adding this character would exceed our column limit
             (when (> (+ visible-cols char-width) columns)
               ;; For wide characters that would overflow, we stop here
               (return))
             (vector-push-extend char result)
             (incf visible-cols char-width)
             (incf i))))))
    (coerce result 'string)))

(defun %background-sgr-code-p (code-str)
  "Check if CODE-STR represents a background color SGR code.
Background codes are: 40-47, 100-107, 48;5;N, 48;2;R;G;B"
  (when (and code-str (> (length code-str) 0))
    (let ((n (ignore-errors (parse-integer code-str :junk-allowed t))))
      (or
       ;; Standard backgrounds 40-47
       (and n (>= n 40) (<= n 47))
       ;; Bright backgrounds 100-107
       (and n (>= n 100) (<= n 107))
       ;; 256-color or truecolor background (48;...)
       (and (>= (length code-str) 2)
            (char= (char code-str 0) #\4)
            (char= (char code-str 1) #\8))))))

(defun %extract-background-sgr (str)
  "Extract only the FIRST (outermost) background color SGR code from the initial SGR sequences in STR.
Returns an SGR sequence string for just the background color, or empty string if none.
This filters out foreground colors and text attributes, keeping only the first background found,
which represents the outer container's background (not inner content styling)."
  (when (or (null str) (zerop (length str)))
    (return-from %extract-background-sgr ""))

  (let ((i 0)
        (len (length str))
        (bg-code nil))
    ;; Scan all leading SGR sequences, but stop at first background found
    (loop while (and (< i len) (char= (char str i) #\Escape) (null bg-code)) do
      (incf i)
      (when (and (< i len) (char= (char str i) #\[))
        (incf i)
        (let ((params-start i))
          ;; Find the end of this SGR sequence
          (loop while (< i len) do
            (let ((code (char-code (char str i))))
              (incf i)
              (when (and (>= code #x40) (<= code #x7E))
                ;; Check if this is an SGR sequence (ends with 'm')
                (when (char= (char str (1- i)) #\m)
                  ;; Parse the parameters (semicolon-separated)
                  (let* ((params-str (subseq str params-start (1- i)))
                         (params (uiop:split-string params-str :separator ";")))
                    ;; Look for first background code in this sequence
                    (loop for j from 0 below (length params)
                          for param = (nth j params)
                          until bg-code  ; Stop once we find one
                          do (cond
                               ;; Extended color: 48;5;N or 48;2;R;G;B
                               ((and (string= param "48")
                                     (< (1+ j) (length params)))
                                (let ((type (nth (1+ j) params)))
                                  (cond
                                    ;; 256-color: 48;5;N
                                    ((and (string= type "5")
                                          (< (+ j 2) (length params)))
                                     (setf bg-code (format nil "48;5;~A" (nth (+ j 2) params))))
                                    ;; Truecolor: 48;2;R;G;B
                                    ((and (string= type "2")
                                          (< (+ j 4) (length params)))
                                     (setf bg-code (format nil "48;2;~A;~A;~A"
                                                           (nth (+ j 2) params)
                                                           (nth (+ j 3) params)
                                                           (nth (+ j 4) params)))))))
                               ;; Standard/bright background: 40-47, 100-107
                               ((%background-sgr-code-p param)
                                (setf bg-code param))))))
                (return)))))))
    ;; Return the background SGR sequence if found
    (if bg-code
        (format nil "~C[~Am" #\Escape bg-code)
        "")))

(defun %insert-before-trailing-reset (str padding)
  "Insert PADDING string before any trailing ESC[0m reset sequence in STR.
If STR doesn't end with a reset, just append PADDING."
  (let* ((len (length str))
         (reset-len 4))  ; ESC [ 0 m
    (if (and (>= len reset-len)
             (char= (char str (- len 4)) #\Escape)
             (char= (char str (- len 3)) #\[)
             (char= (char str (- len 2)) #\0)
             (char= (char str (- len 1)) #\m))
        ;; Insert before trailing reset
        (concatenate 'string
                     (subseq str 0 (- len reset-len))
                     padding
                     (subseq str (- len reset-len)))
        ;; No trailing reset, just append
        (concatenate 'string str padding))))

(defun ansi-drop-columns (str columns)
  "Drop the first COLUMNS visible columns from STR, returning the rest.
Preserves trailing ANSI codes and content after the dropped portion."
  (when (or (null str) (zerop (length str)))
    (return-from ansi-drop-columns ""))
  (when (<= columns 0)
    (return-from ansi-drop-columns str))

  (let ((i 0)
        (len (length str))
        (visible-cols 0))
    ;; Skip over the first COLUMNS visible columns
    (loop while (and (< i len) (< visible-cols columns)) do
      (let ((char (char str i)))
        (cond
          ;; ANSI escape sequence - skip but don't count
          ((char= char #\Escape)
           (incf i)
           (when (and (< i len) (char= (char str i) #\[))
             (incf i)
             (loop while (< i len) do
               (let ((code (char-code (char str i))))
                 (incf i)
                 (when (and (>= code #x40) (<= code #x7E))
                   (return))))))
          ;; Regular character
          (t
           (incf visible-cols (%char-display-width char))
           (incf i)))))
    ;; Return everything from position i onward
    (if (>= i len)
        ""
        (subseq str i))))

(defun %calculate-position (position bg-size fg-size)
  "Calculate the starting coordinate for positioning.
POSITION can be :top/:left (0), :middle/:center (centered), :bottom/:right (end),
or a number (0.0-1.0 as fraction, or integer as absolute position)."
  (let ((space (- bg-size fg-size)))
    (cond
      ((or (eq position :top) (eq position :left)) 0)
      ((or (eq position :middle) (eq position :center)) (max 0 (floor space 2)))
      ((or (eq position :bottom) (eq position :right)) (max 0 space))
      ((and (numberp position) (< position 1) (>= position 0))
       ;; Fraction between 0 and 1
       (max 0 (floor (* space position))))
      ((integerp position) position)
      (t 0))))

(defun %clamp (value min-val max-val)
  "Clamp VALUE between MIN-VAL and MAX-VAL."
  (max min-val (min max-val value)))

(defun composite (foreground background
                  &key (x-position +center+) (y-position +middle+)
                       (x-offset 0) (y-offset 0))
  "Composite FOREGROUND text block on top of BACKGROUND at the specified position.

X-POSITION and Y-POSITION specify where to place the foreground:
  - :left/:top - align to left/top edge
  - :center/:middle - center the foreground
  - :right/:bottom - align to right/bottom edge
  - A number 0.0-1.0 - fractional position
  - An integer - absolute position in columns/rows

X-OFFSET and Y-OFFSET provide fine-tuning adjustment (can be negative).

The foreground is placed on top of the background, replacing the background
content in that region. ANSI escape codes are handled correctly.

Example:
  (composite dialog-box main-ui :x-position +center+ :y-position +middle+)
  (composite tooltip content :x-position +left+ :y-position +top+ :x-offset 5 :y-offset 2)"

  ;; Handle empty cases
  (when (or (null background) (zerop (length background)))
    (return-from composite (or foreground "")))
  (when (or (null foreground) (zerop (length foreground)))
    (return-from composite background))

  ;; Split into lines
  (let* ((fg-lines (split-string-by-newline foreground))
         (bg-lines (split-string-by-newline background))
         (fg-height (length fg-lines))
         (bg-height (length bg-lines))
         (fg-width (if fg-lines (apply #'max (mapcar #'visible-length fg-lines)) 0))
         (bg-width (if bg-lines (apply #'max (mapcar #'visible-length bg-lines)) 0)))

    ;; Handle edge cases
    (when (or (zerop fg-height) (zerop fg-width))
      (return-from composite background))
    (when (or (zerop bg-height) (zerop bg-width))
      (return-from composite foreground))

    ;; Calculate position
    (let* ((base-x (%calculate-position x-position bg-width fg-width))
           (base-y (%calculate-position y-position bg-height fg-height))
           ;; Apply offsets
           (x (+ base-x x-offset))
           (y (+ base-y y-offset)))

      ;; Clamp position to keep foreground at least partially visible
      ;; Allow foreground to be partially outside bounds, but ensure some is visible
      (setf x (%clamp x (- fg-width) bg-width))
      (setf y (%clamp y (- fg-height) bg-height))

      ;; Build the result
      (let ((result '()))
        (dotimes (row bg-height)
          (let ((bg-line (nth row bg-lines)))
            (cond
              ;; Row is before or after the foreground region
              ((or (< row y) (>= row (+ y fg-height)))
               (push bg-line result))
              ;; Row is within the foreground region
              (t
               (let* ((fg-row (- row y))
                      (fg-line (nth fg-row fg-lines))
                      (fg-line-width (visible-length fg-line))
                      (bg-line-width (visible-length bg-line))
                      ;; Effective fg width for this line (use max width for consistency)
                      (effective-fg-width fg-width))
                 ;; Build the composited line
                 (let ((left-part "")
                       (right-part "")
                       (padded-fg-line fg-line))

                   ;; Pad foreground line to max foreground width for consistent edges
                   ;; Insert padding BEFORE trailing reset to keep spaces inside styled region
                   (when (< fg-line-width effective-fg-width)
                     (let ((padding (make-string (- effective-fg-width fg-line-width)
                                                 :initial-element #\Space)))
                       (setf padded-fg-line
                             (%insert-before-trailing-reset fg-line padding))))

                   ;; Get left part of background (before foreground)
                   (when (> x 0)
                     (setf left-part (ansi-take-columns bg-line x))
                     ;; Pad if background line is shorter than x
                     (let ((left-width (visible-length left-part)))
                       (when (< left-width x)
                         (setf left-part
                               (concatenate 'string left-part
                                           (make-string (- x left-width) :initial-element #\Space))))))

                   ;; Handle negative x (foreground starts before background)
                   (when (< x 0)
                     (setf padded-fg-line (ansi-drop-columns padded-fg-line (- x)))
                     (setf effective-fg-width (- effective-fg-width (- x))))

                   ;; Get right part of background (after foreground)
                   ;; Use effective-fg-width (max width) not individual line width
                   (let ((right-start (+ (max 0 x) effective-fg-width)))
                     (when (< right-start bg-line-width)
                       ;; Extract only the background color from the line's initial SGR codes
                       ;; This ensures we restore just the background, not foreground colors
                       ;; that might be specific to content (like selected item styling)
                       (let ((bg-color (%extract-background-sgr bg-line))
                             (dropped (ansi-drop-columns bg-line right-start)))
                         (setf right-part
                               (if (zerop (length bg-color))
                                   dropped
                                   ;; Reset to clear foreground state, then apply background color
                                   (concatenate 'string
                                                (format nil "~C[0m" #\Escape)
                                                bg-color
                                                dropped))))))

                   ;; Combine the parts
                   (push (concatenate 'string left-part padded-fg-line right-part) result)))))))

        ;; Join lines back together
        (format nil "~{~A~^~%~}" (nreverse result))))))

;;; Convenience function for common overlay patterns

(defun overlay-centered (foreground background)
  "Composite FOREGROUND centered on BACKGROUND.
Shorthand for (composite foreground background :x-position +center+ :y-position +middle+)."
  (composite foreground background :x-position +center+ :y-position +middle+))

(defun overlay-at (foreground background x y)
  "Composite FOREGROUND on BACKGROUND at absolute position (X, Y).
X is the column (0 = left edge), Y is the row (0 = top edge)."
  (composite foreground background :x-position x :y-position y))

;;; Transparent shadow compositing

(defun %strip-ansi (str)
  "Remove all ANSI escape sequences from STR, returning only visible characters."
  (when (or (null str) (zerop (length str)))
    (return-from %strip-ansi ""))
  (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (i 0)
        (len (length str)))
    (loop while (< i len) do
      (let ((char (char str i)))
        (cond
          ((char= char #\Escape)
           (incf i)
           (when (and (< i len) (char= (char str i) #\[))
             (incf i)
             (loop while (< i len) do
               (let ((code (char-code (char str i))))
                 (incf i)
                 (when (and (>= code #x40) (<= code #x7E))
                   (return))))))
          (t
           (vector-push-extend char result)
           (incf i)))))
    (coerce result 'string)))

(defun %darken-line-segment (line start-col end-col color bg-color)
  "Darken columns START-COL to END-COL in LINE by stripping styling and applying COLOR/BG-COLOR.
Returns the full line with the darkened segment spliced in.
Restores the line's original background color after the darkened segment."
  (let* ((line-width (visible-length line))
         (actual-start (max 0 (min start-col line-width)))
         (actual-end (max actual-start (min end-col line-width))))
    (when (= actual-start actual-end)
      (return-from %darken-line-segment line))
    (let* ((left (if (> actual-start 0)
                     (ansi-take-columns line actual-start)
                     ""))
           (mid-raw (%strip-ansi
                     (ansi-take-columns (ansi-drop-columns line actual-start)
                                        (- actual-end actual-start))))
           (mid-colored (colored mid-raw :fg color :bg bg-color))
           (right (if (< actual-end line-width)
                      (ansi-drop-columns line actual-end)
                      ""))
           ;; Restore the line's background color after the shadow reset
           (restore-bg (if (> (length right) 0)
                           (%extract-background-sgr line)
                           "")))
      (concatenate 'string left mid-colored restore-bg right))))

(defun composite-with-shadow (foreground background
                              &key (x-position +center+) (y-position +middle+)
                                   (x-offset 0) (y-offset 0)
                                   (shadow-width 2) (shadow-offset 1)
                                   (shadow-color *fg-bright-black*)
                                   (shadow-bg-color *bg-black*))
  "Composite FOREGROUND onto BACKGROUND with a transparent drop shadow.
The shadow darkens the background characters rather than covering them,
so the underlying content remains partially visible.

SHADOW-WIDTH is the shadow thickness in characters (default 2).
SHADOW-OFFSET is the row/col offset before the shadow starts (default 1).
SHADOW-COLOR is the foreground color applied to darkened characters (default bright-black).
SHADOW-BG-COLOR is the background color applied to the shadow region (default black)."
  (when (or (null background) (zerop (length background)))
    (return-from composite-with-shadow (or foreground "")))
  (when (or (null foreground) (zerop (length foreground)))
    (return-from composite-with-shadow background))

  (let* ((fg-lines (split-string-by-newline foreground))
         (bg-lines (split-string-by-newline background))
         (fg-height (length fg-lines))
         (bg-height (length bg-lines))
         (fg-width (if fg-lines (apply #'max (mapcar #'visible-length fg-lines)) 0))
         (bg-width (if bg-lines (apply #'max (mapcar #'visible-length bg-lines)) 0)))

    (when (or (zerop fg-height) (zerop fg-width))
      (return-from composite-with-shadow background))

    ;; Calculate foreground position (same as composite)
    (let* ((base-x (%calculate-position x-position bg-width fg-width))
           (base-y (%calculate-position y-position bg-height fg-height))
           (fx (+ base-x x-offset))
           (fy (+ base-y y-offset))
           ;; Shadow region: right edge and bottom edge offset from foreground
           (shadow-right-x (+ fx fg-width))
           (shadow-right-y-start (+ fy shadow-offset))
           (shadow-right-y-end (+ fy fg-height))
           (shadow-bottom-x-start (+ fx shadow-offset))
           (shadow-bottom-x-end (+ fx fg-width shadow-width))
           (shadow-bottom-y (+ fy fg-height)))

      ;; Darken the background in the shadow regions
      (let ((darkened-bg-lines (copy-list bg-lines)))
        ;; Right edge shadow
        (loop for row from shadow-right-y-start below (min shadow-right-y-end bg-height)
              when (and (>= row 0) (< row bg-height))
              do (setf (nth row darkened-bg-lines)
                       (%darken-line-segment (nth row darkened-bg-lines)
                                             shadow-right-x
                                             (+ shadow-right-x shadow-width)
                                             shadow-color shadow-bg-color)))
        ;; Bottom edge shadow
        (when (and (>= shadow-bottom-y 0) (< shadow-bottom-y bg-height))
          (setf (nth shadow-bottom-y darkened-bg-lines)
                (%darken-line-segment (nth shadow-bottom-y darkened-bg-lines)
                                      shadow-bottom-x-start
                                      shadow-bottom-x-end
                                      shadow-color shadow-bg-color)))

        ;; Now composite the foreground on top of the darkened background
        (composite foreground
                   (format nil "~{~A~^~%~}" darkened-bg-lines)
                   :x-position fx :y-position fy)))))
