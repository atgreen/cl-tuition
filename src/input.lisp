;;;; SPDX-License-Identifier: MIT
;;;; Input handling and key event processing

(in-package #:tuition)

(defvar *input-stream* nil
  "The stream to read input from. Set by the program.")

(defun read-key ()
  "Read a single key from stdin and return a key-msg.
   Returns nil if no input is available."
  (let ((stream (or *input-stream* *standard-input*)))
    (let ((char (read-char-no-hang stream nil nil)))
      (when char
        (cond
          ;; Escape sequences - read the rest of the sequence
          ((char= char #\Escape)
           (parse-escape-start))

          ;; Control characters
          ((< (char-code char) 32)
           (make-key-msg :key (ctrl-char-to-key char) :ctrl t))

          ;; Regular characters
          (t (make-key-msg :key char)))))))

(defun ctrl-char-to-key (char)
  "Convert a control character to its key representation."
  (let ((code (char-code char)))
    (case code
      (3 :ctrl-c)
      (4 :ctrl-d)
      (9 :tab)
      (10 :enter)
      (13 :enter)
      (26 :ctrl-z)
      (otherwise (intern (format nil "CTRL-~A" (code-char (+ code 96))) :keyword)))))

(defun parse-escape-start ()
  "Parse an escape sequence after reading ESC."
  (let ((stream (or *input-stream* *standard-input*)))
    ;; Check if there's a following character with a very short timeout
    (let ((next-available (listen stream)))
      (if next-available
          (let ((next (read-char stream nil nil)))
            (when next
              (parse-escape-sequence next)))
          ;; No following char - just escape key
          (make-key-msg :key :escape)))))

(defun parse-escape-sequence (char)
  "Parse an escape sequence starting after ESC."
  (cond
    ;; CSI sequences (ESC [)
    ((char= char #\[)
     (parse-csi-sequence))

    ;; Alt+key
    ((graphic-char-p char)
     (make-key-msg :key char :alt t))

    ;; Unknown escape sequence
    (t (make-key-msg :key :escape))))

(defun parse-csi-sequence ()
  "Parse a CSI (Control Sequence Introducer) sequence (ESC [)."
  (let ((stream (or *input-stream* *standard-input*)))
    ;; Read first character - could be < for mouse, or a digit, or a letter
    (let ((first-char (read-char stream nil nil)))
      (cond
        ;; Mouse tracking: ESC [ < ...
        ((and first-char (char= first-char #\<))
         (parse-mouse-sequence stream))

        ;; Arrow keys and other simple sequences
        (first-char
         (case first-char
           (#\A (make-key-msg :key :up))
           (#\B (make-key-msg :key :down))
           (#\C (make-key-msg :key :right))
           (#\D (make-key-msg :key :left))
           (otherwise
            ;; Consume any remaining chars in the sequence
            (loop while (listen stream)
                  do (read-char-no-hang stream nil nil))
            (make-key-msg :key :unknown))))

        ;; No character read
        (t (make-key-msg :key :unknown))))))

(defun parse-mouse-sequence (stream)
  "Parse SGR mouse tracking sequence: ESC [ < Cb ; Cx ; Cy (M or m)"
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
                       (action (if (char= char #\M) :press :release))
                       (button (case (logand cb 3)
                                 (0 :left)
                                 (1 :middle)
                                 (2 :right)
                                 (t nil)))
                       (shift (logbitp 2 cb))
                       (alt (logbitp 3 cb))
                       (ctrl (logbitp 4 cb)))
                  (return-from parse-mouse-sequence
                    (make-mouse-msg :x x :y y :button button :action action
                                   :shift shift :alt alt :ctrl ctrl))))
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
