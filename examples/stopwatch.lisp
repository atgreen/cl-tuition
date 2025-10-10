;;;; SPDX-License-Identifier: MIT
;;;; Stopwatch example - demonstrates start/stop/reset timing

(defpackage #:tuition-example-stopwatch
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-stopwatch)

;;; Tick message for updating elapsed time
(tui:defmessage tick-msg ())

;;; Model
(defclass stopwatch-model ()
  ((running :initform nil :accessor stopwatch-running)
   (start-time :initform nil :accessor stopwatch-start-time)
   (elapsed :initform 0 :accessor stopwatch-elapsed)
   (quitting :initform nil :accessor stopwatch-quitting)))

;;; Init
(defmethod tui:init ((model stopwatch-model))
  nil)

;;; Helper to format elapsed time
(defun format-elapsed (seconds)
  "Format elapsed time as MM:SS.MS"
  (let* ((mins (floor seconds 60))
         (secs (floor (mod seconds 60)))
         (ms (floor (* (mod seconds 1) 100))))
    (format nil "~2,'0D:~2,'0D.~2,'0D" mins secs ms)))

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model stopwatch-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Quit on q or ctrl+c
      ((and (characterp key) (char= key #\q))
       (setf (stopwatch-quitting model) t)
       (values model (tui:quit-cmd)))
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c))
       (setf (stopwatch-quitting model) t)
       (values model (tui:quit-cmd)))
      ;; Start/stop on s
      ((and (characterp key) (char= key #\s))
       (if (stopwatch-running model)
           ;; Stop
           (progn
             (setf (stopwatch-elapsed model)
                   (+ (stopwatch-elapsed model)
                      (- (get-internal-real-time) (stopwatch-start-time model))))
             (setf (stopwatch-running model) nil)
             (values model nil))
           ;; Start
           (progn
             (setf (stopwatch-start-time model) (get-internal-real-time))
             (setf (stopwatch-running model) t)
             (values model
                     (lambda ()
                       (sleep 0.01)
                       (make-instance 'tick-msg))))))
      ;; Reset on r
      ((and (characterp key) (char= key #\r))
       (setf (stopwatch-elapsed model) 0)
       (setf (stopwatch-start-time model) nil)
       (setf (stopwatch-running model) nil)
       (values model nil))
      (t (values model nil)))))

(defmethod tui:update-message ((model stopwatch-model) (msg tick-msg))
  (if (stopwatch-running model)
      (values model
              (lambda ()
                (sleep 0.01)
                (make-instance 'tick-msg)))
      (values model nil)))

;;; View
(defmethod tui:view ((model stopwatch-model))
  (let* ((current-elapsed
          (if (stopwatch-running model)
              (+ (stopwatch-elapsed model)
                 (- (get-internal-real-time) (stopwatch-start-time model)))
              (stopwatch-elapsed model)))
         (seconds (/ current-elapsed internal-time-units-per-second)))
    (format nil "~%Elapsed: ~A~%~%~
                 Controls:~%~
                 s    ~A~%~
                 r    Reset~%~
                 q    Quit~%~%"
            (format-elapsed seconds)
            (if (stopwatch-running model) "Stop" "Start"))))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'stopwatch-model))))
    (tui:run program)))

#+nil
(main)
