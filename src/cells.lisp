;;; cells.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Cell-based terminal buffer for the "Cursed Renderer"
;;;;
;;;; Each terminal cell stores a character, its display width, style
;;;; attributes (fg, bg, text attrs), and an optional hyperlink URL.
;;;; A screen-buffer is a 2D row-major grid of cells with dirty-line
;;;; tracking.  The diff renderer compares two screen-buffers and emits
;;;; minimal ANSI escape sequences to update only changed cells.

(in-package #:tuition)

;;; ---------- Attribute bitmask constants ----------

(defconstant +attr-bold+          #b00000001)
(defconstant +attr-italic+        #b00000010)
(defconstant +attr-underline+     #b00000100)
(defconstant +attr-blink+         #b00001000)
(defconstant +attr-reverse+       #b00010000)
(defconstant +attr-strikethrough+ #b00100000)
(defconstant +attr-faint+         #b01000000)

;;; ---------- Cell structure ----------

(defstruct (cell (:constructor %make-cell))
  "A single terminal cell."
  (char #\Space :type character)
  (width 1 :type (integer 0 2))          ; display width (0 for combining, 2 for wide)
  (fg nil)                                ; foreground color string or nil
  (bg nil)                                ; background color string or nil
  (attrs 0 :type fixnum)                  ; bitmask of +attr-*+ constants
  (link nil :type (or null string)))      ; hyperlink URL

(defun make-cell (&key (char #\Space) (width 1) fg bg (attrs 0) link)
  "Create a cell with the given attributes."
  (%make-cell :char char :width width :fg fg :bg bg :attrs attrs :link link))

(defun blank-cell ()
  "Return a fresh blank cell (space, default colors, no attrs)."
  (%make-cell))

(defun cell-equal (a b)
  "Return T if cells A and B are visually identical."
  (and (char= (cell-char a) (cell-char b))
       (= (cell-width a) (cell-width b))
       (equal (cell-fg a) (cell-fg b))
       (equal (cell-bg a) (cell-bg b))
       (= (cell-attrs a) (cell-attrs b))
       (equal (cell-link a) (cell-link b))))

;;; ---------- Screen buffer ----------

(defstruct (screen-buffer (:constructor %make-screen-buffer))
  "A 2D grid of cells, stored row-major."
  (width 80 :type fixnum)
  (height 24 :type fixnum)
  (cells #() :type simple-vector)          ; row-major vector of cell structs
  (touched nil :type (or null simple-bit-vector))) ; dirty line tracking

(defun make-screen-buffer (width height)
  "Allocate a screen-buffer of WIDTH x HEIGHT blank cells."
  (let* ((n (* width height))
         (cells (make-array n :initial-element nil)))
    (dotimes (i n)
      (setf (aref cells i) (blank-cell)))
    (%make-screen-buffer :width width
                         :height height
                         :cells cells
                         :touched (make-array height :element-type 'bit
                                             :initial-element 1))))

(defun screen-buffer-ref (buf x y)
  "Return the cell at column X, row Y."
  (aref (screen-buffer-cells buf) (+ (* y (screen-buffer-width buf)) x)))

(defun (setf screen-buffer-ref) (cell buf x y)
  "Set the cell at column X, row Y."
  (setf (aref (screen-buffer-cells buf) (+ (* y (screen-buffer-width buf)) x)) cell))

(defun screen-buffer-clear (buf)
  "Reset all cells to blank and mark all lines touched."
  (let ((cells (screen-buffer-cells buf))
        (n (* (screen-buffer-width buf) (screen-buffer-height buf))))
    (dotimes (i n)
      (let ((c (aref cells i)))
        (setf (cell-char c) #\Space
              (cell-width c) 1
              (cell-fg c) nil
              (cell-bg c) nil
              (cell-attrs c) 0
              (cell-link c) nil))))
  (fill (screen-buffer-touched buf) 1))

(defun screen-buffer-touch-line (buf y)
  "Mark line Y as dirty."
  (when (and (>= y 0) (< y (screen-buffer-height buf)))
    (setf (bit (screen-buffer-touched buf) y) 1)))

(defun screen-buffer-touch-all (buf)
  "Mark all lines as dirty."
  (fill (screen-buffer-touched buf) 1))

(defun screen-buffer-resize (buf new-width new-height)
  "Return a new screen-buffer of NEW-WIDTH x NEW-HEIGHT, copying overlapping cells."
  (let ((new-buf (make-screen-buffer new-width new-height))
        (copy-w (min (screen-buffer-width buf) new-width))
        (copy-h (min (screen-buffer-height buf) new-height)))
    (dotimes (y copy-h)
      (dotimes (x copy-w)
        (let ((src (screen-buffer-ref buf x y))
              (dst (screen-buffer-ref new-buf x y)))
          (setf (cell-char dst) (cell-char src)
                (cell-width dst) (cell-width src)
                (cell-fg dst) (cell-fg src)
                (cell-bg dst) (cell-bg src)
                (cell-attrs dst) (cell-attrs src)
                (cell-link dst) (cell-link src)))))
    new-buf))

;;; ---------- ANSI string parser → cell grid ----------

(defun parse-styled-string (str width height)
  "Parse an ANSI-styled string into a screen-buffer of WIDTH x HEIGHT.
Walk the string character by character, tracking style state as ANSI
escapes are encountered, placing characters into cells."
  (let ((buf (make-screen-buffer width height))
        (x 0) (y 0)
        (cur-fg nil) (cur-bg nil) (cur-attrs 0) (cur-link nil)
        (i 0) (len (length str)))
    (flet ((put-cell (ch cw)
             "Place character CH of display-width CW at (x,y)."
             (when (and (>= x 0) (< x width) (>= y 0) (< y height))
               (let ((c (screen-buffer-ref buf x y)))
                 (setf (cell-char c) ch
                       (cell-width c) cw
                       (cell-fg c) cur-fg
                       (cell-bg c) cur-bg
                       (cell-attrs c) cur-attrs
                       (cell-link c) cur-link))
               (screen-buffer-touch-line buf y)
               ;; For wide chars, blank the trailing cell
               (when (and (= cw 2) (< (1+ x) width))
                 (let ((c2 (screen-buffer-ref buf (1+ x) y)))
                   (setf (cell-char c2) #\Space
                         (cell-width c2) 0
                         (cell-fg c2) cur-fg
                         (cell-bg c2) cur-bg
                         (cell-attrs c2) cur-attrs
                         (cell-link c2) cur-link))))
             (incf x cw)))
      (loop while (< i len) do
        (let ((ch (char str i)))
          (cond
            ;; ESC sequence
            ((char= ch #\Escape)
             (incf i)
             (when (< i len)
               (let ((next (char str i)))
                 (cond
                   ;; CSI sequence: ESC [
                   ((char= next #\[)
                    (incf i)
                    (let ((params (make-array 8 :fill-pointer 0 :initial-element 0))
                          (current 0)
                          (have-digit nil))
                      (vector-push-extend 0 params)
                      (loop while (< i len) do
                        (let ((c (char str i)))
                          (cond
                            ((digit-char-p c)
                             (setf current (+ (* current 10) (digit-char-p c)))
                             (setf have-digit t)
                             (incf i))
                            ((or (char= c #\;) (char= c #\:))
                             (setf (aref params (1- (fill-pointer params))) current)
                             (setf current 0 have-digit nil)
                             (vector-push-extend 0 params)
                             (incf i))
                            ;; Final byte 0x40-0x7E
                            ((and (>= (char-code c) #x40) (<= (char-code c) #x7E))
                             (when have-digit
                               (setf (aref params (1- (fill-pointer params))) current))
                             ;; Process based on final byte
                             (case c
                               (#\m ; SGR
                                (if (and (= (fill-pointer params) 1)
                                         (= (aref params 0) 0)
                                         (not have-digit))
                                    ;; ESC[m with no params = reset
                                    (setf cur-fg nil cur-bg nil cur-attrs 0)
                                    ;; Process SGR params
                                    (let ((pidx 0))
                                      (loop while (< pidx (fill-pointer params)) do
                                        (let ((p (aref params pidx)))
                                          (cond
                                            ((= p 0) (setf cur-fg nil cur-bg nil cur-attrs 0))
                                            ((= p 1) (setf cur-attrs (logior cur-attrs +attr-bold+)))
                                            ((= p 2) (setf cur-attrs (logior cur-attrs +attr-faint+)))
                                            ((= p 3) (setf cur-attrs (logior cur-attrs +attr-italic+)))
                                            ((= p 4) (setf cur-attrs (logior cur-attrs +attr-underline+)))
                                            ((= p 5) (setf cur-attrs (logior cur-attrs +attr-blink+)))
                                            ((= p 7) (setf cur-attrs (logior cur-attrs +attr-reverse+)))
                                            ((= p 9) (setf cur-attrs (logior cur-attrs +attr-strikethrough+)))
                                            ((= p 21) (setf cur-attrs (logior cur-attrs +attr-underline+))) ; double underline
                                            ((= p 22) (setf cur-attrs (logand cur-attrs (lognot (logior +attr-bold+ +attr-faint+)))))
                                            ((= p 23) (setf cur-attrs (logand cur-attrs (lognot +attr-italic+))))
                                            ((= p 24) (setf cur-attrs (logand cur-attrs (lognot +attr-underline+))))
                                            ((= p 25) (setf cur-attrs (logand cur-attrs (lognot +attr-blink+))))
                                            ((= p 27) (setf cur-attrs (logand cur-attrs (lognot +attr-reverse+))))
                                            ((= p 29) (setf cur-attrs (logand cur-attrs (lognot +attr-strikethrough+))))
                                            ;; Standard fg colors 30-37
                                            ((<= 30 p 37) (setf cur-fg (format nil "~D" p)))
                                            ;; Default fg
                                            ((= p 39) (setf cur-fg nil))
                                            ;; Standard bg colors 40-47
                                            ((<= 40 p 47) (setf cur-bg (format nil "~D" p)))
                                            ;; Default bg
                                            ((= p 49) (setf cur-bg nil))
                                            ;; Bright fg 90-97
                                            ((<= 90 p 97) (setf cur-fg (format nil "~D" p)))
                                            ;; Bright bg 100-107
                                            ((<= 100 p 107) (setf cur-bg (format nil "~D" p)))
                                            ;; Extended colors: 38;5;N or 38;2;R;G;B
                                            ((= p 38)
                                             (when (< (1+ pidx) (fill-pointer params))
                                               (let ((mode (aref params (1+ pidx))))
                                                 (cond
                                                   ((= mode 5)
                                                    (when (< (+ pidx 2) (fill-pointer params))
                                                      (setf cur-fg (format nil "38;5;~D" (aref params (+ pidx 2))))
                                                      (incf pidx 2)))
                                                   ((= mode 2)
                                                    (when (< (+ pidx 4) (fill-pointer params))
                                                      (setf cur-fg (format nil "38;2;~D;~D;~D"
                                                                           (aref params (+ pidx 2))
                                                                           (aref params (+ pidx 3))
                                                                           (aref params (+ pidx 4))))
                                                      (incf pidx 4)))))))
                                            ;; Extended bg: 48;5;N or 48;2;R;G;B
                                            ((= p 48)
                                             (when (< (1+ pidx) (fill-pointer params))
                                               (let ((mode (aref params (1+ pidx))))
                                                 (cond
                                                   ((= mode 5)
                                                    (when (< (+ pidx 2) (fill-pointer params))
                                                      (setf cur-bg (format nil "48;5;~D" (aref params (+ pidx 2))))
                                                      (incf pidx 2)))
                                                   ((= mode 2)
                                                    (when (< (+ pidx 4) (fill-pointer params))
                                                      (setf cur-bg (format nil "48;2;~D;~D;~D"
                                                                           (aref params (+ pidx 2))
                                                                           (aref params (+ pidx 3))
                                                                           (aref params (+ pidx 4))))
                                                      (incf pidx 4))))))))
                                          (incf pidx)))))))
                             (incf i)
                             (return))
                            ;; Skip unknown intermediate bytes
                            (t (incf i)))))))
                   ;; OSC sequence: ESC ]
                   ((char= next #\])
                    (incf i)
                    ;; Read until ST (ESC \) or BEL (^G)
                    (let ((osc-start i))
                      (declare (ignore osc-start))
                      (loop while (< i len) do
                        (let ((c (char str i)))
                          (cond
                            ((char= c #\Bel) (incf i) (return))
                            ((and (char= c #\Escape)
                                  (< (1+ i) len)
                                  (char= (char str (1+ i)) #\\))
                             (incf i 2) (return))
                            (t (incf i)))))))
                   ;; Other escape - skip
                   (t (incf i))))))

            ;; Newline
            ((char= ch #\Newline)
             (incf y)
             (setf x 0)
             (incf i))

            ;; Tab
            ((char= ch #\Tab)
             (let ((stop (* (1+ (floor x 8)) 8)))
               (loop while (< x stop) do (put-cell #\Space 1)))
             (incf i))

            ;; Regular character
            (t
             (let ((cw (%char-display-width ch)))
               (cond
                 ((= cw 0)
                  ;; Combining character - attach to previous cell if possible
                  (incf i))
                 (t
                  ;; Check if it fits on current line
                  (when (> (+ x cw) width)
                    ;; Wrap to next line
                    (incf y)
                    (setf x 0))
                  (when (< y height)
                    (put-cell ch cw))
                  (incf i))))))))
    buf)))

;;; ---------- Cell diff renderer ----------

(defun %sgr-from-cell (cell)
  "Build the SGR parameter string for CELL's style (without ESC[ prefix)."
  (let ((parts nil))
    (when (logtest (cell-attrs cell) +attr-bold+) (push "1" parts))
    (when (logtest (cell-attrs cell) +attr-faint+) (push "2" parts))
    (when (logtest (cell-attrs cell) +attr-italic+) (push "3" parts))
    (when (logtest (cell-attrs cell) +attr-underline+) (push "4" parts))
    (when (logtest (cell-attrs cell) +attr-blink+) (push "5" parts))
    (when (logtest (cell-attrs cell) +attr-reverse+) (push "7" parts))
    (when (logtest (cell-attrs cell) +attr-strikethrough+) (push "9" parts))
    (when (cell-fg cell)
      (push (cell-fg cell) parts))
    (when (cell-bg cell)
      (push (cell-bg cell) parts))
    (nreverse parts)))

(defun %style-differs (a b)
  "Return T if cells A and B have different visual style."
  (or (not (equal (cell-fg a) (cell-fg b)))
      (not (equal (cell-bg a) (cell-bg b)))
      (/= (cell-attrs a) (cell-attrs b))
      (not (equal (cell-link a) (cell-link b)))))

(defun render-diff (old-buf new-buf stream)
  "Compare OLD-BUF and NEW-BUF cell by cell, emitting minimal ANSI
escape sequences to STREAM to update the terminal.  Skips unchanged
cells and tracks cursor position to emit minimal movement sequences."
  (let* ((w (screen-buffer-width new-buf))
         (h (screen-buffer-height new-buf))
         (cur-x 0) (cur-y 0)      ; assumed cursor position
         (wrote-anything nil)
         ;; Track current terminal style state so we only emit SGR when it changes
         (term-fg nil) (term-bg nil) (term-attrs 0) (term-link nil))
    (dotimes (y h)
      ;; Skip untouched lines
      (unless (and (screen-buffer-touched new-buf)
                   (= (bit (screen-buffer-touched new-buf) y) 0))
        (let ((x 0))
          (loop while (< x w) do
            (let ((new-cell (screen-buffer-ref new-buf x y))
                  (old-cell (when (and old-buf
                                       (< y (screen-buffer-height old-buf))
                                       (< x (screen-buffer-width old-buf)))
                              (screen-buffer-ref old-buf x y))))
              ;; Skip if cell is identical to old buffer
              (if (and old-cell (cell-equal old-cell new-cell))
                  ;; Skip this cell
                  (incf x (max 1 (cell-width new-cell)))
                  ;; Cell changed - emit update
                  (progn
                    ;; Move cursor if needed
                    (unless (and (= cur-x x) (= cur-y y))
                      (cond
                        ;; Same row, move column
                        ((= cur-y y)
                         (cond
                           ((= x 0)
                            (write-char #\Return stream)
                            (setf cur-x 0))
                           ((> x cur-x)
                            (format stream "~C[~DC" #\Escape (- x cur-x))
                            (setf cur-x x))
                           ((< x cur-x)
                            (format stream "~C[~DD" #\Escape (- cur-x x))
                            (setf cur-x x))))
                        ;; Different row
                        (t
                         ;; Use absolute cursor positioning
                         (format stream "~C[~D;~DH" #\Escape (1+ y) (1+ x))
                         (setf cur-x x cur-y y))))

                    ;; Emit SGR if style changed
                    (let ((need-sgr (or (not (equal (cell-fg new-cell) term-fg))
                                        (not (equal (cell-bg new-cell) term-bg))
                                        (/= (cell-attrs new-cell) term-attrs))))
                      (when need-sgr
                        (let ((parts (%sgr-from-cell new-cell)))
                          (if parts
                              (format stream "~C[0;~{~A~^;~}m" #\Escape parts)
                              (format stream "~C[0m" #\Escape)))
                        (setf term-fg (cell-fg new-cell)
                              term-bg (cell-bg new-cell)
                              term-attrs (cell-attrs new-cell))))

                    ;; Handle hyperlinks
                    (when (not (equal (cell-link new-cell) term-link))
                      (if (cell-link new-cell)
                          (format stream "~C]8;;~A~C\\" #\Escape (cell-link new-cell) #\Escape)
                          (format stream "~C]8;;~C\\" #\Escape #\Escape))
                      (setf term-link (cell-link new-cell)))

                    ;; Emit the character
                    (let ((ch (cell-char new-cell)))
                      (unless (= (cell-width new-cell) 0)
                        (write-char ch stream)))

                    ;; Update cursor position
                    (incf cur-x (max 1 (cell-width new-cell)))
                    (setf wrote-anything t)

                    ;; Advance past the cell
                    (incf x (max 1 (cell-width new-cell))))))))))

    ;; Reset SGR at end if we wrote anything
    (when (and wrote-anything
               (or term-fg term-bg (/= term-attrs 0)))
      (format stream "~C[0m" #\Escape))

    ;; Close any open hyperlink
    (when (and wrote-anything term-link)
      (format stream "~C]8;;~C\\" #\Escape #\Escape))))

;;; ---------- Synchronized output ----------

(defun write-syncd-begin (stream)
  "Begin synchronized update (DEC private mode 2026)."
  (format stream "~C[?2026h" #\Escape))

(defun write-syncd-end (stream)
  "End synchronized update (DEC private mode 2026)."
  (format stream "~C[?2026l" #\Escape))
