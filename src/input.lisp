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
  "Read a single key from stdin and return a key-msg.
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
           (make-key-msg :key :backspace))

          ;; Control characters (including ASCII BS = 8)
          ((< (char-code char) 32)
           (let ((k (ctrl-char-to-key char)))
             (%ilog "read-key: control char ~D -> ~A" (char-code char) k)
             (make-key-msg :key k :ctrl t)))

          ;; Regular characters
          (t
           (%ilog "read-key: graphic '~A'" char)
           (make-key-msg :key char)))))))

(defun ctrl-char-to-key (char)
  "Convert a control character to its key representation.
   For most control chars, returns the corresponding lowercase letter.
   For special cases like tab, enter, backspace, returns a keyword."
  (let ((code (char-code char)))
    (case code
      (8 :backspace)
      (9 :tab)
      (10 :enter)
      (13 :enter)
      ;; For other control characters, return the corresponding letter
      ;; Ctrl+A is code 1, Ctrl+Z is code 26, etc.
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
            (make-key-msg :key :escape))))))

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

    ;; Alt+key
    ((graphic-char-p char)
     (make-key-msg :key char :alt t))
    ;; ESC + DEL (common M-Backspace in some terminals)
    ((and (characterp char) (= (char-code char) 127))
     (make-key-msg :key :backspace :alt t))

    ;; Unknown escape sequence
    (t
     (%ilog "parse-escape-sequence: unknown ESC seq start '~A'" char)
     (make-key-msg :key :escape))))

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
           (#\I (progn (%ilog "parse-csi-sequence: -> focus-in") (make-focus-in-msg)))
           (#\O (progn (%ilog "parse-csi-sequence: -> focus-out") (make-focus-out-msg)))))

        ;; Navigation keys: arrows, Home, End, Backtab
        ((and ch (member ch '(#\A #\B #\C #\D #\H #\F #\Z)))
         (case ch
           (#\A (progn (%ilog "parse-csi-sequence: -> :up") (make-key-msg :key :up)))
           (#\B (progn (%ilog "parse-csi-sequence: -> :down") (make-key-msg :key :down)))
           (#\C (progn (%ilog "parse-csi-sequence: -> :right") (make-key-msg :key :right)))
           (#\D (progn (%ilog "parse-csi-sequence: -> :left") (make-key-msg :key :left)))
           (#\H (progn (%ilog "parse-csi-sequence: -> :home") (make-key-msg :key :home)))
           (#\F (progn (%ilog "parse-csi-sequence: -> :end") (make-key-msg :key :end)))
           (#\Z (progn (%ilog "parse-csi-sequence: -> :backtab") (make-key-msg :key :backtab)))))

        ;; Digits: e.g., 3~ for Delete
        ((and ch (digit-char-p ch))
         (let ((digits (list ch))
               (term nil)
               (c nil))
           (loop for c = (read-char stream nil nil)
                 while c
                 do (if (digit-char-p c)
                        (push c digits)
                        (progn
                          (setf term c)
                          (return))))
           (%ilog "parse-csi-sequence: digits='~A' term='~A'" (coerce (nreverse digits) 'string) term)
           (if (and term (char= term #\~))
               (let* ((num-str (coerce (nreverse digits) 'string))
                      (num (ignore-errors (parse-integer num-str))))
                 (case num
                   (3 (progn (%ilog "parse-csi-sequence: -> :delete") (make-key-msg :key :delete)))
                   (200 (progn (%ilog "parse-csi-sequence: bracketed paste start")
                               (parse-bracketed-paste stream)))
                   (otherwise (make-key-msg :key :unknown))))
               (make-key-msg :key :unknown))))

        (t
         ;; Consume any remaining chars in the sequence then return unknown
         (loop while (listen stream) do (read-char-no-hang stream nil nil))
         (make-key-msg :key :unknown))))))

(defun parse-mouse-sequence (stream)
  "Parse SGR mouse tracking sequence: ESC [ < Cb ; Cx ; Cy (M or m)
   Returns new hierarchical mouse event types."
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
                       (is-scroll (or (= cb 64) (= cb 65)))
                       (button (case base-button
                                 (0 :left)
                                 (1 :middle)
                                 (2 :right)
                                 (t nil)))
                       (shift (logbitp 2 cb))
                       (alt (logbitp 3 cb))
                       (ctrl (logbitp 4 cb)))
                  (return-from parse-mouse-sequence
                    (cond
                      ;; Scroll events
                      ((= cb 64) (make-mouse-scroll-event :x x :y y :direction :up
                                                         :shift shift :alt alt :ctrl ctrl))
                      ((= cb 65) (make-mouse-scroll-event :x x :y y :direction :down
                                                         :shift shift :alt alt :ctrl ctrl))
                      ;; Drag events (motion with button held)
                      ((and is-motion button)
                       (make-mouse-drag-event :x x :y y :button button
                                             :shift shift :alt alt :ctrl ctrl))
                      ;; Move events (motion without button)
                      (is-motion
                       (make-mouse-move-event :x x :y y
                                             :shift shift :alt alt :ctrl ctrl))
                      ;; Press/release events
                      (is-press
                       (make-mouse-press-event :x x :y y :button button
                                              :shift shift :alt alt :ctrl ctrl))
                      (t
                       (make-mouse-release-event :x x :y y :button button
                                                :shift shift :alt alt :ctrl ctrl))))))
               (t (return nil))))
    ;; If we didn't parse properly, return unknown key
    (make-key-msg :key :unknown)))

(defun key-string (key-msg)
  "Convert a key-msg to a string representation."
  (let ((key (key-msg-key key-msg)))
    (cond
      ((characterp key) (string key))
      ((keywordp key) (string-downcase (symbol-name key)))
      (t (format nil "~A" key)))))

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
        (#\A (make-key-msg :key :up))
        (#\B (make-key-msg :key :down))
        (#\C (make-key-msg :key :right))
        (#\D (make-key-msg :key :left))
        (#\F (make-key-msg :key :end))
        (#\H (make-key-msg :key :home))
        (otherwise (make-key-msg :key :unknown))))))
