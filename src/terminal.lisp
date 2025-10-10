;;;; SPDX-License-Identifier: MIT
;;;; Terminal control and raw mode handling

(in-package #:tuition)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defvar *original-termios* nil
  "Stores the terminal state before entering raw mode.")

(defun stream-fd (stream)
  "Get the file descriptor for a stream."
  (let ((stream (typecase stream
                  (synonym-stream (symbol-value (synonym-stream-symbol stream)))
                  (two-way-stream (two-way-stream-input-stream stream))
                  (sb-sys:fd-stream stream))))
    (sb-posix:file-descriptor stream)))

(defun enter-raw-mode ()
  "Put the terminal in raw mode for TUI applications using POSIX termios."
  #+sbcl
  (handler-case
      (let* ((fd (stream-fd *terminal-io*))
             (termios (sb-posix:tcgetattr fd)))
        ;; Save original state
        (unless *original-termios*
          (setf *original-termios* termios))

        ;; Get a fresh termios to modify
        (let ((new-termios (sb-posix:tcgetattr fd)))
          ;; Disable canonical mode (ICANON) and echo (ECHO)
          (setf (sb-posix:termios-lflag new-termios)
                (logand (sb-posix:termios-lflag new-termios)
                        (lognot (logior sb-posix:icanon
                                        sb-posix:echo
                                        sb-posix:echoe
                                        sb-posix:echok
                                        sb-posix:echonl
                                        sb-posix:isig))))

          ;; Disable input processing
          (setf (sb-posix:termios-iflag new-termios)
                (logand (sb-posix:termios-iflag new-termios)
                        (lognot (logior sb-posix:icrnl
                                        sb-posix:ixon))))

          ;; Keep OPOST enabled so newlines are properly converted to CR+LF
          ;; (raw output mode disabled to allow proper line handling)

          ;; Set minimum chars and timeout for non-blocking read
          (let ((cc (sb-posix:termios-cc new-termios)))
            (setf (aref cc sb-posix:vmin) 0)   ; Return immediately
            (setf (aref cc sb-posix:vtime) 0))  ; No timeout

          ;; Apply the new settings immediately
          (sb-posix:tcsetattr fd sb-posix:tcsanow new-termios)))
    (error (c)
      (error (make-condition 'terminal-operation-error :operation :enter-raw-mode :reason c))))
  #-sbcl
  (warn "Raw mode not yet implemented for this Lisp"))

(defun exit-raw-mode ()
  "Restore the terminal to its original state."
  #+sbcl
  (handler-case
      (when *original-termios*
        (let ((fd (stream-fd *terminal-io*)))
          (sb-posix:tcsetattr fd sb-posix:tcsanow *original-termios*)
          (setf *original-termios* nil)))
    (error (c)
      (error (make-condition 'terminal-operation-error :operation :exit-raw-mode :reason c))))
  #-sbcl
  (warn "Raw mode not yet implemented for this Lisp"))

(defun get-terminal-size ()
  "Get the current terminal size as (width . height)."
  #+unix
  (handler-case
      (let ((output (uiop:run-program '("stty" "size") :output :string :error-output nil)))
        (let ((parts (uiop:split-string output)))
          (when (= 2 (length parts))
            (cons (parse-integer (second parts))
                  (parse-integer (first parts))))))
    (error ()
      (cons 80 24))) ; default fallback
  #+windows
  (cons 80 24)) ; default fallback

(defun clear-screen ()
  "Clear the terminal screen."
  (format t "~C[2J~C[H" #\Escape #\Escape)
  (force-output))

(defun hide-cursor ()
  "Hide the terminal cursor."
  (format t "~C[?25l" #\Escape)
  (force-output))

(defun show-cursor ()
  "Show the terminal cursor."
  (format t "~C[?25h" #\Escape)
  (force-output))

(defun enter-alt-screen ()
  "Enter alternate screen buffer."
  (format t "~C[?1049h" #\Escape)
  (force-output))

(defun exit-alt-screen ()
  "Exit alternate screen buffer."
  (format t "~C[?1049l" #\Escape)
  (force-output))

(defun enable-mouse-cell-motion ()
  "Enable mouse cell motion tracking."
  (format t "~C[?1002h~C[?1006h" #\Escape #\Escape)
  (force-output))

(defun enable-mouse-all-motion ()
  "Enable mouse all motion tracking."
  (format t "~C[?1003h~C[?1006h" #\Escape #\Escape)
  (force-output))

(defun disable-mouse ()
  "Disable mouse tracking."
  (format t "~C[?1002l~C[?1003l~C[?1006l" #\Escape #\Escape #\Escape)
  (force-output))

;;; High-level terminal lifecycle
(defmacro with-raw-terminal ((&key alt-screen mouse) &body body)
  "Execute BODY with the terminal in raw TUI mode.

Args:
- ALT-SCREEN: enter/exit alternate screen when true.
- MOUSE: one of :cell-motion, :all-motion, or NIL.

Always restores the terminal, even if BODY errors."
  (let ((alt (gensym "ALT"))
        (m (gensym "MOUSE"))
        (raw-ok (gensym "RAW-OK")))
    `(block with-raw-terminal
       (let ((,alt ,alt-screen)
             (,m ,mouse)
             (,raw-ok nil))
         (unwind-protect
              (progn
                (restart-case
                    (progn
                      (enter-raw-mode)
                      (setf ,raw-ok t))
                  (use-no-raw ()
                    :report "Continue without entering raw mode."
                    (setf ,raw-ok nil))
                  (retry ()
                    :report "Retry entering raw mode."
                    (enter-raw-mode)
                    (setf ,raw-ok t))
                  (abort ()
                    :report "Abort terminal setup."
                    (return-from with-raw-terminal nil)))

                (when ,raw-ok
                  (hide-cursor)
                  (clear-screen))
                (when ,alt (enter-alt-screen))
                (ecase ,m
                  ((nil) nil)
                  (:cell-motion (enable-mouse-cell-motion))
                  (:all-motion (enable-mouse-all-motion)))
                ,@body)
           (ignore-errors
             (when ,alt (exit-alt-screen))
             (when ,m (disable-mouse))
              (when ,raw-ok (show-cursor))
              (when ,raw-ok (exit-raw-mode))))))))
