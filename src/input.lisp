;;; input.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Input handling and key event processing

(in-package #:tuition)

(defparameter *input-log-enabled* nil
  "When true, key parsing writes debug logs to *input-log-file*.")

(defparameter *input-log-file* "/tmp/tuition-input-debug.log"
  "Path to write input debugging logs when enabled.")

(defun enable-input-logging (&optional (path *input-log-file*))
  "Enable verbose input parsing logs to PATH."
  (setf *input-log-enabled* t
        *input-log-file* path)
  (with-open-file (s *input-log-file*
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format s "=== Input logging enabled ===~%"))
  t)

(defun disable-input-logging ()
  "Disable input parsing logs."
  (setf *input-log-enabled* nil)
  t)

(defun %ilog (fmt &rest args)
  "Internal: write a log line if *input-log-enabled* is true."
  (when *input-log-enabled*
    (with-open-file (s *input-log-file*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
      (apply #'format s (concatenate 'string fmt "~%") args))))

(defvar *input-stream* nil
  "The stream to read input from. Set by the program.")

(defun read-key ()
  "Read a single key from stdin and return a key-press-msg.
   Returns nil if no input is available."
  (let ((stream (or *input-stream* *standard-input*)))
    (let ((char (read-char-no-hang stream nil nil)))
      (when char
        (%ilog "read-key: char='~A' code=~D" char (char-code char))
        (cond
          ;; Escape sequences - read the rest of the sequence
          ((char= char #\Escape)
           (%ilog "read-key: ESC detected -> parse-escape-start")
           (parse-escape-start))

          ;; ASCII DEL (127) is commonly sent for backspace
          ((= (char-code char) 127)
           (%ilog "read-key: DEL(127) -> :backspace")
           (make-key-press-msg :code :backspace))

          ;; Control characters (including ASCII BS = 8)
          ((< (char-code char) 32)
           (let ((k (ctrl-char-to-key char)))
             (%ilog "read-key: control char ~D -> ~A" (char-code char) k)
             (make-key-press-msg :code k :mod +mod-ctrl+)))

          ;; Regular characters
          (t
           (%ilog "read-key: graphic '~A'" char)
           (make-key-press-msg :code char :text (string char))))))))

(defun ctrl-char-to-key (char)
  "Convert a control character to its key representation."
  (let ((code (char-code char)))
    (case code
      (8 :backspace)
      (9 :tab)
      (10 :enter)
      (13 :enter)
      ;; For other control characters, return the corresponding letter
      (otherwise (code-char (+ code 96))))))

(defun parse-escape-start ()
  "Parse an escape sequence after reading ESC."
  (let ((stream (or *input-stream* *standard-input*)))
    ;; Check if there's a following character with a very short timeout
    (let ((tries 0))
      ;; Briefly wait for the next byte(s) of an escape sequence
      (loop while (and (not (listen stream)) (< tries 8))
            do (sleep 0.005) (incf tries))
      (%ilog "parse-escape-start: waited ~D ticks, listen=~A" tries (listen stream))
      (if (listen stream)
          (let ((next (read-char stream nil nil)))
            (%ilog "parse-escape-start: next='~A' code=~D" next (and next (char-code next)))
            (when next
              (let ((ret (parse-escape-sequence next)))
                (%ilog "parse-escape-start: ret=~S" ret)
                ret)))
          ;; No following char - just escape key
          (progn
            (%ilog "parse-escape-start: lone ESC -> :escape")
            (make-key-press-msg :code :escape))))))

(defun parse-escape-sequence (char)
  "Parse an escape sequence starting after ESC."
  (cond
    ;; CSI sequences (ESC [)
    ((char= char #\[)
     (%ilog "parse-escape-sequence: CSI '['")
     (parse-csi-sequence))

    ;; SS3 sequences (ESC O) used by some terminals for arrows and Home/End
    ((char= char #\O)
     (%ilog "parse-escape-sequence: SS3 'O'")
     (parse-ss3-sequence))

    ;; OSC sequences (ESC ])
    ((char= char #\])
     (%ilog "parse-escape-sequence: OSC ']'")
     (parse-osc-sequence))

    ;; DCS sequences (ESC P) - used for XTVERSION responses
    ((char= char #\P)
     (%ilog "parse-escape-sequence: DCS 'P'")
     (parse-dcs-sequence))

    ;; Alt+key
    ((graphic-char-p char)
     (make-key-press-msg :code char :mod +mod-alt+ :text (string char)))
    ;; ESC + DEL (common M-Backspace in some terminals)
    ((and (characterp char) (= (char-code char) 127))
     (make-key-press-msg :code :backspace :mod +mod-alt+))

    ;; Unknown escape sequence
    (t
     (%ilog "parse-escape-sequence: unknown ESC seq start '~A'" char)
     (make-key-press-msg :code :escape))))

(defun %xterm-modifier-to-mod (modifier)
  "Convert xterm-style modifier number to mod bitmask.
Modifier is 1-based: 2=Shift, 3=Alt, 4=Shift+Alt, 5=Ctrl, etc."
  (let ((m (1- modifier)))
    (make-mod :shift (logbitp 0 m)
              :alt (logbitp 1 m)
              :ctrl (logbitp 2 m)
              :meta (logbitp 3 m))))

(defun parse-csi-sequence ()
  "Parse a CSI (Control Sequence Introducer) sequence (ESC [)."
  (let ((stream (or *input-stream* *standard-input*)))
    (let ((ch (read-char stream nil nil)))
      (%ilog "parse-csi-sequence: ch='~A' (code ~A)" ch (and ch (char-code ch)))
      (cond
        ;; Mouse tracking: ESC [ < ...
        ((and ch (char= ch #\<))
         (parse-mouse-sequence stream))

        ;; Focus events: ESC [ I (focus in), ESC [ O (focus out)
        ((and ch (member ch '(#\I #\O)))
         (case ch
           (#\I (progn (%ilog "parse-csi-sequence: -> focus-msg") (make-focus-msg)))
           (#\O (progn (%ilog "parse-csi-sequence: -> blur-msg") (make-blur-msg)))))

        ;; Navigation keys: arrows, Home, End, Backtab
        ((and ch (member ch '(#\A #\B #\C #\D #\H #\F #\Z)))
         (case ch
           (#\A (make-key-press-msg :code :up))
           (#\B (make-key-press-msg :code :down))
           (#\C (make-key-press-msg :code :right))
           (#\D (make-key-press-msg :code :left))
           (#\H (make-key-press-msg :code :home))
           (#\F (make-key-press-msg :code :end))
           (#\Z (make-key-press-msg :code :tab :mod +mod-shift+))))

        ;; Kitty keyboard response: ESC [ ? flags u
        ((and ch (char= ch #\?))
         (parse-kitty-query-response stream))

        ;; Digits: e.g., 3~ for Delete, or 1;2A for Shift+Up, or Kitty u sequences
        ((and ch (digit-char-p ch))
         (let ((params nil)
               (current-num (digit-char-p ch))
               (sub-params nil)
               (term nil))
           ;; Read digits, semicolons, colons until we hit a terminator
           (loop for c = (read-char stream nil nil)
                 while c
                 do (cond
                      ((digit-char-p c)
                       (setf current-num (+ (* current-num 10) (digit-char-p c))))
                      ((char= c #\;)
                       (push current-num params)
                       (setf current-num 0))
                      ((char= c #\:)
                       ;; Sub-parameter separator (used in Kitty protocol)
                       (push current-num sub-params)
                       (setf current-num 0))
                      (t
                       (push current-num params)
                       (setf term c)
                       (return))))
           (setf params (nreverse params))
           (setf sub-params (nreverse sub-params))
           (%ilog "parse-csi-sequence: params=~A sub-params=~A term='~A'" params sub-params term)
           (cond
             ;; Kitty keyboard protocol: ESC [ codepoint ; modifiers [: event-type] u
             ((and term (char= term #\u))
              (parse-kitty-key-sequence params sub-params))

             ;; Modified arrow keys: ESC[1;NA where N is modifier
             ((and term (member term '(#\A #\B #\C #\D #\H #\F))
                   (= (length params) 2)
                   (= (first params) 1))
              (let* ((modifier (second params))
                     (mod (%xterm-modifier-to-mod modifier))
                     (base-key (case term
                                 (#\A :up)
                                 (#\B :down)
                                 (#\C :right)
                                 (#\D :left)
                                 (#\H :home)
                                 (#\F :end))))
                (%ilog "parse-csi-sequence: modified key ~A mod=~D" base-key mod)
                (make-key-press-msg :code base-key :mod mod)))

             ;; Special keys with ~ terminator
             ((and term (char= term #\~))
              (let ((num (first params))
                    (mod (if (= (length params) 2)
                             (%xterm-modifier-to-mod (second params))
                             0)))
                (case num
                  (2 (make-key-press-msg :code :insert :mod mod))
                  (3 (make-key-press-msg :code :delete :mod mod))
                  (5 (make-key-press-msg :code :page-up :mod mod))
                  (6 (make-key-press-msg :code :page-down :mod mod))
                  (200 (progn (%ilog "parse-csi-sequence: bracketed paste start")
                              (parse-bracketed-paste stream)))
                  (otherwise (make-key-press-msg :code :unknown)))))
             (t (make-key-press-msg :code :unknown)))))

        (t
         ;; Consume any remaining chars in the sequence then return unknown
         (loop while (listen stream) do (read-char-no-hang stream nil nil))
         (make-key-press-msg :code :unknown))))))

;;; ---------- Kitty keyboard protocol ----------

(defun parse-kitty-key-sequence (params sub-params)
  "Parse a Kitty keyboard sequence from CSI params.
Format: CSI codepoint ; modifiers [: event-type] u"
  (let* ((codepoint (first params))
         (mod-and-event (if (>= (length params) 2)
                            (second params)
                            1))
         ;; If sub-params has values, they came from the modifier param
         ;; Format is modifiers:event-type
         (event-type (cond
                       ;; Sub-params from : separator within the modifier param
                       (sub-params (car (last sub-params)))
                       (t 1))) ; default to press
         (modifier (if sub-params
                       (first sub-params)
                       mod-and-event))
         ;; Convert Kitty modifier (1-based) to our bitmask
         (mod (%kitty-modifier-to-mod modifier))
         ;; Convert codepoint to key code
         (code (%kitty-codepoint-to-key codepoint))
         (text (if (and (> codepoint 31) (< codepoint 127))
                   (string (code-char codepoint))
                   "")))
    (%ilog "parse-kitty-key: cp=~D mod=~D event=~D -> code=~A" codepoint modifier event-type code)
    (case event-type
      (1 (make-key-press-msg :code code :mod mod :text text))            ; press
      (2 (make-key-press-msg :code code :mod mod :text text :repeat-p t)) ; repeat
      (3 (make-key-release-msg :code code :mod mod :text text))           ; release
      (otherwise (make-key-press-msg :code code :mod mod :text text)))))

(defun %kitty-modifier-to-mod (modifier)
  "Convert Kitty modifier value (1-based bitmask) to our mod bitmask."
  (let ((m (1- modifier)))
    (make-mod :shift (logbitp 0 m)
              :alt (logbitp 1 m)
              :ctrl (logbitp 2 m)
              :super (logbitp 3 m)
              :hyper (logbitp 4 m)
              :meta (logbitp 5 m)
              :caps-lock (logbitp 6 m)
              :num-lock (logbitp 7 m))))

(defun %kitty-codepoint-to-key (codepoint)
  "Convert a Unicode codepoint from Kitty protocol to a key code."
  (case codepoint
    (9 :tab)
    (13 :enter)
    (27 :escape)
    (127 :backspace)
    ;; Function keys (Kitty uses specific codepoints)
    (57344 :escape)
    (57345 :enter)
    (57346 :tab)
    (57347 :backspace)
    (57348 :insert)
    (57349 :delete)
    (57350 :left)
    (57351 :right)
    (57352 :up)
    (57353 :down)
    (57354 :page-up)
    (57355 :page-down)
    (57356 :home)
    (57357 :end)
    (57358 :caps-lock)
    (57359 :scroll-lock)
    (57360 :num-lock)
    (57361 :print-screen)
    (57362 :pause)
    (57363 :menu)
    ;; F1-F35
    (57364 :f1) (57365 :f2) (57366 :f3) (57367 :f4)
    (57368 :f5) (57369 :f6) (57370 :f7) (57371 :f8)
    (57372 :f9) (57373 :f10) (57374 :f11) (57375 :f12)
    (57376 :f13) (57377 :f14) (57378 :f15) (57379 :f16)
    (57380 :f17) (57381 :f18) (57382 :f19) (57383 :f20)
    ;; Keypad
    (57399 :kp-0) (57400 :kp-1) (57401 :kp-2) (57402 :kp-3)
    (57403 :kp-4) (57404 :kp-5) (57405 :kp-6) (57406 :kp-7)
    (57407 :kp-8) (57408 :kp-9)
    (57409 :kp-decimal) (57410 :kp-divide) (57411 :kp-multiply)
    (57412 :kp-subtract) (57413 :kp-add) (57414 :kp-enter)
    (57415 :kp-equal)
    ;; Modifier keys themselves
    (57441 :left-shift) (57442 :left-ctrl) (57443 :left-alt)
    (57444 :left-super) (57445 :left-hyper) (57446 :left-meta)
    (57447 :right-shift) (57448 :right-ctrl) (57449 :right-alt)
    (57450 :right-super) (57451 :right-hyper) (57452 :right-meta)
    ;; Regular printable characters
    (otherwise
     (if (and (>= codepoint 32) (<= codepoint #x10FFFF))
         (code-char codepoint)
         :unknown))))

(defun parse-kitty-query-response (stream)
  "Parse a CSI ? sequence: either Kitty query (CSI ? flags u)
or DECRPM response (CSI ? mode ; setting $ y)."
  (let ((params nil)
        (current-num 0)
        (term nil)
        (prev-char nil))
    (loop for c = (read-char stream nil nil)
          while c
          do (cond
               ((digit-char-p c)
                (setf current-num (+ (* current-num 10) (digit-char-p c))))
               ((char= c #\;)
                (push current-num params)
                (setf current-num 0))
               ((char= c #\$)
                ;; DECRPM: next char should be 'y'
                (push current-num params)
                (setf prev-char c))
               (t
                (push current-num params)
                (setf term c)
                (return))))
    (setf params (nreverse params))
    (cond
      ;; Kitty keyboard query response: CSI ? flags u
      ((and term (char= term #\u))
       (%ilog "parse-kitty-query-response: flags=~D" (first params))
       (make-instance 'keyboard-enhancements-msg :flags (or (first params) 0)))
      ;; DECRPM response: CSI ? mode ; setting $ y
      ((and term (char= term #\y) prev-char)
       (let ((mode (first params))
             (setting (second params)))
         (%ilog "parse-decrpm: mode=~D setting=~D" mode setting)
         (make-mode-report-msg :mode mode :setting setting)))
      (t (make-key-press-msg :code :unknown)))))

;;; ---------- Mouse sequence parsing ----------

(defun parse-mouse-sequence (stream)
  "Parse SGR mouse tracking sequence: ESC [ < Cb ; Cx ; Cy (M or m)
   Returns v2 mouse event types with modifier bitmask."
  (let ((params (make-array 3 :initial-element 0))
        (param-idx 0)
        (current-num 0))
    ;; Read parameters separated by semicolons
    (loop for char = (read-char stream nil nil)
          while char
          do (cond
               ((digit-char-p char)
                (setf current-num (+ (* current-num 10) (digit-char-p char))))
               ((char= char #\;)
                (setf (aref params param-idx) current-num)
                (incf param-idx)
                (setf current-num 0))
               ;; M = press, m = release
               ((or (char= char #\M) (char= char #\m))
                (setf (aref params param-idx) current-num)
                (let* ((cb (aref params 0))
                       (x (1+ (aref params 1)))  ; convert to 1-based
                       (y (1+ (aref params 2)))  ; convert to 1-based
                       (is-press (char= char #\M))
                       (base-button (logand cb 3))
                       (is-motion (logbitp 5 cb))  ; Motion bit (32)
                       (button (case base-button
                                 (0 :left)
                                 (1 :middle)
                                 (2 :right)
                                 (t nil)))
                       (mod (make-mod :shift (logbitp 2 cb)
                                      :alt (logbitp 3 cb)
                                      :ctrl (logbitp 4 cb))))
                  (return-from parse-mouse-sequence
                    (cond
                      ;; Scroll events
                      ((= cb 64) (make-mouse-wheel-msg :x x :y y :direction :up :mod mod))
                      ((= cb 65) (make-mouse-wheel-msg :x x :y y :direction :down :mod mod))
                      ;; Motion events (with or without button)
                      (is-motion
                       (make-mouse-motion-msg :x x :y y :button button :mod mod))
                      ;; Click/release
                      (is-press
                       (make-mouse-click-msg :x x :y y :button button :mod mod))
                      (t
                       (make-mouse-release-msg :x x :y y :button button :mod mod))))))
               (t (return nil))))
    ;; If we didn't parse properly, return unknown key
    (make-key-press-msg :code :unknown)))

;;; ---------- Utility functions ----------

(defun key-string (key-event)
  "Convert a key event to a string representation."
  (let ((code (key-event-code key-event)))
    (cond
      ((characterp code) (string code))
      ((keywordp code) (string-downcase (symbol-name code)))
      (t (format nil "~A" code)))))

(defun parse-bracketed-paste (stream)
  "Read everything until ESC [ 201 ~ and return a paste message."
  (let ((out (make-string-output-stream)))
    (labels ((readc () (read-char stream nil nil)))
      (loop for ch = (readc) while ch do
            (if (char= ch #\Escape)
                (let ((c1 (readc)))
                  (if (and c1 (char= c1 #\[))
                      (let* ((digits '()) (d nil) (term nil))
                        (loop for d = (readc) while d do
                              (cond
                                ((digit-char-p d) (push d digits))
                                (t (setf term d) (return))))
                        (when (and term (char= term #\~))
                          (let* ((num-str (coerce (nreverse digits) 'string))
                                 (num (ignore-errors (parse-integer num-str))))
                            (when (= num 201)
                              (return)))))
                      (progn
                        (write-char ch out)
                        (when c1 (write-char c1 out)))))
                (write-char ch out))))
    (let ((text (get-output-stream-string out)))
      (%ilog "parse-bracketed-paste: collected ~D chars" (length text))
      (make-paste-msg :text text))))

(defun parse-ss3-sequence ()
  "Parse an SS3 sequence (ESC O <char>) used for arrows in application mode."
  (let ((stream (or *input-stream* *standard-input*)))
    (let ((ch (read-char stream nil nil)))
      (%ilog "parse-ss3-sequence: ch='~A' code=~A" ch (and ch (char-code ch)))
      (case ch
        (#\A (make-key-press-msg :code :up))
        (#\B (make-key-press-msg :code :down))
        (#\C (make-key-press-msg :code :right))
        (#\D (make-key-press-msg :code :left))
        (#\F (make-key-press-msg :code :end))
        (#\H (make-key-press-msg :code :home))
        (otherwise (make-key-press-msg :code :unknown))))))

;;; ---------- OSC sequence parsing ----------

(defun %read-until-st (stream)
  "Read until String Terminator (ST = ESC \\ or BEL).
Returns the accumulated string content."
  (let ((out (make-string-output-stream)))
    (loop for ch = (read-char stream nil nil)
          while ch
          do (cond
               ;; ESC \\ is ST
               ((char= ch #\Escape)
                (let ((next (read-char stream nil nil)))
                  (when (and next (char= next #\\))
                    (return)))
                ;; If not \\, put ESC content in output
                (write-char ch out))
               ;; BEL is also ST
               ((= (char-code ch) 7) (return))
               (t (write-char ch out))))
    (get-output-stream-string out)))

(defun parse-osc-sequence ()
  "Parse an OSC (Operating System Command) sequence: ESC ] Ps ; Pt ST.
Handles color responses (OSC 10/11/12) and clipboard (OSC 52)."
  (let ((stream (or *input-stream* *standard-input*)))
    ;; Read the parameter number
    (let ((num 0))
      (loop for c = (read-char stream nil nil)
            while c
            do (cond
                 ((digit-char-p c)
                  (setf num (+ (* num 10) (digit-char-p c))))
                 ((char= c #\;)
                  ;; Now read the payload until ST
                  (let ((payload (%read-until-st stream)))
                    (%ilog "parse-osc: num=~D payload='~A'" num payload)
                    (return-from parse-osc-sequence
                      (case num
                        ;; OSC 10: foreground color response
                        (10 (make-instance 'foreground-color-msg :color payload))
                        ;; OSC 11: background color response
                        (11 (make-instance 'background-color-msg :color payload))
                        ;; OSC 12: cursor color response
                        (12 (make-instance 'cursor-color-msg :color payload))
                        ;; OSC 52: clipboard response
                        (52 (parse-osc52-payload payload))
                        (otherwise
                         (%ilog "parse-osc: unhandled OSC ~D" num)
                         nil)))))
                 (t
                  ;; Unexpected char, consume until ST
                  (%read-until-st stream)
                  (return-from parse-osc-sequence nil))))
      nil)))

(defun parse-osc52-payload (payload)
  "Parse OSC 52 clipboard payload: selection;base64-content.
Returns a clipboard-msg with decoded content."
  (let ((semi-pos (position #\; payload)))
    (if semi-pos
        (let* ((b64 (subseq payload (1+ semi-pos)))
               (content (handler-case
                            (cl-base64:base64-string-to-string b64)
                          (error () b64))))
          (%ilog "parse-osc52: decoded clipboard content (~D chars)" (length content))
          (make-clipboard-msg :content content))
        ;; No semicolon — malformed, return raw
        (make-clipboard-msg :content payload))))

;;; ---------- DCS sequence parsing ----------

(defun parse-dcs-sequence ()
  "Parse a DCS (Device Control String) sequence: ESC P ... ST.
Handles XTVERSION response: DCS >| version ST."
  (let ((stream (or *input-stream* *standard-input*)))
    (let ((ch (read-char stream nil nil)))
      (cond
        ;; XTVERSION response: ESC P >| version-string ST
        ((and ch (char= ch #\>))
         (let ((next (read-char stream nil nil)))
           (if (and next (char= next #\|))
               (let ((version (%read-until-st stream)))
                 (%ilog "parse-dcs: XTVERSION='~A'" version)
                 (make-terminal-version-msg :version version))
               ;; Not XTVERSION, consume until ST
               (progn
                 (%read-until-st stream)
                 nil))))
        (t
         ;; Unknown DCS, consume until ST
         (%read-until-st stream)
         nil)))))
