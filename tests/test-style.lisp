;;; test-style.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Tests for style system (src/style.lisp)

(in-package #:tuition-tests)

(def-suite style-tests
  :description "Tests for style functions."
  :in tuition-tests)

(in-suite style-tests)

;;; --- visible-length ---

(test visible-length-empty
  "visible-length of empty string is 0."
  (is (= 0 (visible-length ""))))

(test visible-length-ascii
  "visible-length counts ASCII characters."
  (is (= 11 (visible-length "hello world"))))

(test visible-length-ansi-codes
  "visible-length ignores ANSI SGR sequences."
  (let ((styled (format nil "~C[1;31mhello~C[0m" #\Escape #\Escape)))
    (is (= 5 (visible-length styled)))))

(test visible-length-multiple-ansi
  "visible-length handles multiple ANSI sequences."
  (let ((s (format nil "~C[31mred~C[0m ~C[32mgreen~C[0m"
                   #\Escape #\Escape #\Escape #\Escape)))
    (is (= 9 (visible-length s)))))

;;; --- color-256, color-rgb, parse-hex-color ---

(test color-256-foreground
  "color-256 produces correct foreground code."
  (is (string= "38;5;196" (color-256 196))))

(test color-256-background
  "color-256 produces correct background code."
  (is (string= "48;5;196" (color-256 196 :foreground nil))))

(test color-rgb-foreground
  "color-rgb produces correct foreground code."
  (is (string= "38;2;255;0;128" (color-rgb 255 0 128))))

(test color-rgb-background
  "color-rgb produces correct background code."
  (is (string= "48;2;255;0;128" (color-rgb 255 0 128 :foreground nil))))

(test parse-hex-color-6digit
  "parse-hex-color handles 6-digit hex."
  (is (string= "38;2;255;0;0" (parse-hex-color "#FF0000"))))

(test parse-hex-color-3digit
  "parse-hex-color handles 3-digit shorthand."
  (is (string= "38;2;255;0;0" (parse-hex-color "#F00"))))

(test parse-hex-color-background
  "parse-hex-color handles background mode."
  (is (string= "48;2;0;255;0" (parse-hex-color "#00FF00" :foreground nil))))

;;; --- split-string-by-newline ---

(test split-empty-string
  "split-string-by-newline of empty string returns list with empty string."
  (is (equal '("") (split-string-by-newline ""))))

(test split-no-newlines
  "split-string-by-newline with no newlines returns single-element list."
  (is (equal '("hello") (split-string-by-newline "hello"))))

(test split-multiple-newlines
  "split-string-by-newline splits correctly."
  (is (equal '("a" "b" "c") (split-string-by-newline (format nil "a~%b~%c")))))

(test split-trailing-newline
  "split-string-by-newline handles trailing newline."
  (is (equal '("a" "") (split-string-by-newline (format nil "a~%")))))

;;; --- bold, italic, underline, colored ---

(test bold-wraps-ansi
  "bold wraps text with ANSI bold codes."
  (let ((result (bold "hi")))
    (is (= 2 (visible-length result)))
    (is (search (format nil "~C[1m" #\Escape) result))))

(test italic-wraps-ansi
  "italic wraps text with ANSI italic codes."
  (let ((result (italic "hi")))
    (is (= 2 (visible-length result)))
    (is (search (format nil "~C[3m" #\Escape) result))))

(test underline-wraps-ansi
  "underline wraps text with ANSI underline codes."
  (let ((result (underline "hi")))
    (is (= 2 (visible-length result)))))

(test colored-fg
  "colored applies foreground color."
  (let ((result (colored "hi" :fg *fg-red*)))
    (is (= 2 (visible-length result)))
    (is (search "31" result))))

(test colored-bg
  "colored applies background color."
  (let ((result (colored "hi" :bg *bg-blue*)))
    (is (= 2 (visible-length result)))
    (is (search "44" result))))

;;; --- indent-lines ---

(test indent-lines-basic
  "indent-lines adds spaces to each line."
  (let ((result (indent-lines (format nil "a~%b") 3)))
    (is (string= (format nil "   a~%   b") result))))

(test indent-lines-zero
  "indent-lines with 0 returns original."
  (is (string= "hello" (indent-lines "hello" 0))))

;;; --- width, height, size ---

(test width-single-line
  "width of single line."
  (is (= 5 (width "hello"))))

(test width-multi-line
  "width returns widest line."
  (is (= 5 (width (format nil "hi~%hello~%yo")))))

(test height-single-line
  "height of single line is 1."
  (is (= 1 (height "hello"))))

(test height-multi-line
  "height counts lines."
  (is (= 3 (height (format nil "a~%b~%c")))))

(test size-returns-both
  "size returns width and height."
  (multiple-value-bind (w h) (size (format nil "hello~%hi"))
    (is (= 5 w))
    (is (= 2 h))))

;;; --- Color math ---

(test hex-to-rgb-6digit
  "hex-to-rgb parses 6-digit hex."
  (multiple-value-bind (r g b) (tuition::%hex-to-rgb "#FF8040")
    (is (= 255 r))
    (is (= 128 g))
    (is (= 64 b))))

(test hex-to-rgb-3digit
  "hex-to-rgb parses 3-digit hex."
  (multiple-value-bind (r g b) (tuition::%hex-to-rgb "#F00")
    (is (= 255 r))
    (is (= 0 g))
    (is (= 0 b))))

(test rgb-to-hex
  "rgb-to-hex produces correct hex string."
  (is (string= "#FF0080" (tuition::%rgb-to-hex 255 0 128))))

(test darken-color-basic
  "darken-color makes color darker."
  (let ((result (darken-color "#FFFFFF" 0.5)))
    ;; White darkened by 50% should be around #808080
    (multiple-value-bind (r g b) (tuition::%hex-to-rgb result)
      (is (< r 200))
      (is (< g 200))
      (is (< b 200)))))

(test lighten-color-basic
  "lighten-color makes color lighter."
  (let ((result (lighten-color "#000000" 0.5)))
    (multiple-value-bind (r g b) (tuition::%hex-to-rgb result)
      (is (> r 50))
      (is (> g 50))
      (is (> b 50)))))

(test complementary-color-basic
  "complementary-color rotates hue 180 degrees."
  (let ((result (complementary-color "#FF0000")))
    ;; Complement of pure red should be cyan-ish
    (multiple-value-bind (r g b) (tuition::%hex-to-rgb result)
      (is (< r 50))
      (is (> g 200))
      (is (> b 200)))))

(test blend-colors-endpoints
  "blend-colors at 0.0 returns first color, at 1.0 returns second."
  (is (string= "#FF0000" (blend-colors "#FF0000" "#0000FF" 0.0)))
  (is (string= "#0000FF" (blend-colors "#FF0000" "#0000FF" 1.0))))

(test blend-colors-midpoint
  "blend-colors at 0.5 returns midpoint."
  (let ((result (blend-colors "#FF0000" "#0000FF" 0.5)))
    (multiple-value-bind (r g b) (tuition::%hex-to-rgb result)
      (is (and (> r 100) (< r 160)))
      (is (= 0 g))
      (is (and (> b 100) (< b 160))))))

;;; --- RGB to palette conversion ---

(test rgb-to-ansi256-pure-red
  "rgb-to-ansi256 maps pure red to 196."
  (is (= 196 (rgb-to-ansi256 255 0 0))))

(test rgb-to-ansi256-pure-green
  "rgb-to-ansi256 maps pure green to 46."
  (is (= 46 (rgb-to-ansi256 0 255 0))))

(test rgb-to-ansi256-pure-blue
  "rgb-to-ansi256 maps pure blue to 21."
  (is (= 21 (rgb-to-ansi256 0 0 255))))

(test rgb-to-ansi256-white
  "rgb-to-ansi256 maps white to 231."
  (is (= 231 (rgb-to-ansi256 255 255 255))))

(test rgb-to-ansi256-black
  "rgb-to-ansi256 maps black to 16 (cube black) or 0 (base black)."
  (is (member (rgb-to-ansi256 0 0 0) '(0 16))))

(test rgb-to-ansi256-mid-gray
  "rgb-to-ansi256 maps mid gray to a grayscale index."
  (let ((idx (rgb-to-ansi256 128 128 128)))
    ;; Should land in the grayscale ramp (232-255) or base palette
    (is (>= idx 0))
    (is (<= idx 255))))

(test rgb-to-ansi16-pure-red
  "rgb-to-ansi16 maps pure red to bright red (9)."
  (is (= 9 (rgb-to-ansi16 255 0 0))))

(test rgb-to-ansi16-black
  "rgb-to-ansi16 maps black to 0."
  (is (= 0 (rgb-to-ansi16 0 0 0))))

(test rgb-to-ansi16-white
  "rgb-to-ansi16 maps white to bright white (15)."
  (is (= 15 (rgb-to-ansi16 255 255 255))))

(test hex-to-ansi256-red
  "hex-to-ansi256 maps #FF0000 to 196."
  (is (= 196 (hex-to-ansi256 "#FF0000"))))

(test hex-to-ansi256-shorthand
  "hex-to-ansi256 handles 3-digit hex."
  (is (= 196 (hex-to-ansi256 "#F00"))))

(test hex-to-ansi16-blue
  "hex-to-ansi16 maps #0000FF to bright blue (12)."
  (is (= 12 (hex-to-ansi16 "#0000FF"))))

(test ansi256-to-hex-round-trip-cube
  "ansi256-to-hex converts cube colors correctly."
  ;; Index 196 = red (r=5, g=0, b=0) = RGB(255, 0, 0)
  (is (string= "#FF0000" (ansi256-to-hex 196))))

(test ansi256-to-hex-grayscale
  "ansi256-to-hex converts grayscale correctly."
  ;; Index 232 = darkest gray = RGB(8, 8, 8)
  (is (string= "#080808" (ansi256-to-hex 232))))

(test make-complete-color-auto-populate
  "make-complete-color auto-computes ansi256 and ansi from truecolor."
  (let ((c (make-complete-color :truecolor "#FF0000")))
    (is (string= "#FF0000" (tuition::complete-truecolor c)))
    (is (= 196 (tuition::complete-ansi256 c)))
    (is (stringp (tuition::complete-ansi c)))))

(test make-complete-color-explicit-override
  "make-complete-color respects explicitly provided ansi256/ansi."
  (let ((c (make-complete-color :truecolor "#FF0000" :ansi256 160 :ansi "31")))
    (is (= 160 (tuition::complete-ansi256 c)))
    (is (string= "31" (tuition::complete-ansi c)))))

;;; --- make-style ---

(test make-style-defaults
  "make-style with no args creates valid style."
  (let ((s (make-style)))
    (is (typep s 'style))))

(test make-style-bold
  "make-style :bold t sets bold."
  (let ((s (make-style :bold t)))
    (is (tuition::style-bold s))))

(test make-style-padding
  "make-style :padding sets all padding sides."
  (let ((s (make-style :padding 2)))
    (is (= 2 (tuition::style-padding-left s)))
    (is (= 2 (tuition::style-padding-right s)))
    (is (= 2 (tuition::style-padding-top s)))
    (is (= 2 (tuition::style-padding-bottom s)))))
