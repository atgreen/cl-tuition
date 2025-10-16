;;; terminal.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
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
                  (sb-sys:fd-stream stream)
                  (otherwise stream))))
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
      ;; Use /dev/tty to query the actual terminal, not stdin
      (let ((output (uiop:run-program '("sh" "-c" "stty size < /dev/tty")
                                     :output :string
                                     :error-output nil)))
        (let ((parts (uiop:split-string output)))
          (when (= 2 (length parts))
            (cons (parse-integer (second parts))
                  (parse-integer (first parts))))))
    (error ()
      (cons 80 24))) ; default fallback
  #+windows
  (cons 80 24)) ; default fallback

(defun clear-screen (&optional (stream *standard-output*))
  "Clear the terminal screen."
  (format stream "~C[2J~C[H" #\Escape #\Escape)
  (force-output stream))

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

(defun enable-bracketed-paste ()
  "Enable bracketed paste mode."
  (format t "~C[?2004h" #\Escape)
  (force-output))

(defun disable-bracketed-paste ()
  "Disable bracketed paste mode."
  (format t "~C[?2004l" #\Escape)
  (force-output))

(defun enable-focus-events ()
  "Enable focus in/out event reporting."
  (format t "~C[?1004h" #\Escape)
  (force-output))

(defun disable-focus-events ()
  "Disable focus in/out event reporting."
  (format t "~C[?1004l" #\Escape)
  (force-output))

;;; Suspend/resume support

(defun suspend-terminal (&key alt-screen mouse focus-events)
  "Suspend the terminal - restore original state for backgrounding.
Returns a function to restore the TUI state."
  ;; Clean up TUI state
  (when mouse (disable-mouse))
  (disable-bracketed-paste)
  (when focus-events (disable-focus-events))
  (when alt-screen (exit-alt-screen))
  (show-cursor)
  (exit-raw-mode)
  (force-output)

  ;; Return a function to restore TUI state
  (lambda ()
    (enter-raw-mode)
    (hide-cursor)
    (clear-screen)
    (when alt-screen (enter-alt-screen))
    (enable-bracketed-paste)
    (when focus-events (enable-focus-events))
    (ecase mouse
      ((nil) nil)
      (:cell-motion (enable-mouse-cell-motion))
      (:all-motion (enable-mouse-all-motion)))
    (force-output)))

(defun resume-terminal (restore-fn)
  "Resume the terminal using the restore function."
  (when restore-fn
    (funcall restore-fn)))

;;; High-level terminal lifecycle
(defmacro with-raw-terminal ((&key alt-screen mouse (focus-events t)) &body body)
  "Execute BODY with the terminal in raw TUI mode.

Args:
- ALT-SCREEN: enter/exit alternate screen when true.
- MOUSE: one of :cell-motion, :all-motion, or NIL.
- FOCUS-EVENTS: enable focus in/out events (default: T).

Always restores the terminal, even if BODY errors."
  (let ((alt (gensym "ALT"))
        (m (gensym "MOUSE"))
        (foc (gensym "FOCUS"))
        (raw-ok (gensym "RAW-OK")))
    `(block with-raw-terminal
       (let ((,alt ,alt-screen)
             (,m ,mouse)
             (,foc ,focus-events)
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
                (when ,raw-ok (enable-bracketed-paste))
                (when ,foc (enable-focus-events))
                (ecase ,m
                  ((nil) nil)
                  (:cell-motion (enable-mouse-cell-motion))
                  (:all-motion (enable-mouse-all-motion)))
                ,@body)
           (ignore-errors
             (when ,alt (exit-alt-screen))
             (when ,m (disable-mouse))
             (when ,foc (disable-focus-events))
             (when ,raw-ok (disable-bracketed-paste))
              (when ,raw-ok (show-cursor))
              (when ,raw-ok (exit-raw-mode))))))))
