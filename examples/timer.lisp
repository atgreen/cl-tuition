;;; timer.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Timer example - demonstrates countdown timer

(asdf:load-system :tuition)

(defpackage #:tuition-example-timer
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-timer)

;;; Messages
(tui:defmessage tick-msg ())
(tui:defmessage timeout-msg ())

;;; Model
(defclass timer-model ()
  ((running :initform t :accessor timer-running)
   (timeout :initform 5 :accessor timer-timeout)
   (remaining :initform 5 :accessor timer-remaining)
   (start-time :initform nil :accessor timer-start-time)
   (paused-at :initform nil :accessor timer-paused-at)
   (quitting :initform nil :accessor timer-quitting)))

;;; Init - start the timer
(defmethod tui:init ((model timer-model))
  (setf (timer-start-time model) (get-internal-real-time))
  (lambda ()
    (sleep 0.01)
    (make-instance 'tick-msg)))

;;; Helper to get remaining time
(defun get-remaining-time (model)
  "Calculate remaining time in seconds"
  (if (timer-running model)
      (max 0 (- (timer-timeout model)
                (/ (- (get-internal-real-time) (timer-start-time model))
                   internal-time-units-per-second)))
      (if (timer-paused-at model)
          (timer-paused-at model)
          (timer-timeout model))))

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model timer-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Quit on q or ctrl+c
      ((and (characterp key) (char= key #\q))
       (setf (timer-quitting model) t)
       (values model (tui:quit-cmd)))
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c))
       (setf (timer-quitting model) t)
       (values model (tui:quit-cmd)))
      ;; Start/stop on s
      ((and (characterp key) (char= key #\s))
       (if (timer-running model)
           ;; Stop/pause
           (progn
             (setf (timer-paused-at model) (get-remaining-time model))
             (setf (timer-running model) nil)
             (values model nil))
           ;; Start/resume
           (progn
             (setf (timer-start-time model)
                   (- (get-internal-real-time)
                      (* (- (timer-timeout model) (timer-paused-at model))
                         internal-time-units-per-second)))
             (setf (timer-running model) t)
             (values model
                     (lambda ()
                       (sleep 0.01)
                       (make-instance 'tick-msg))))))
      ;; Reset on r
      ((and (characterp key) (char= key #\r))
       (setf (timer-remaining model) (timer-timeout model))
       (setf (timer-paused-at model) (timer-timeout model))
       (when (timer-running model)
         (setf (timer-start-time model) (get-internal-real-time)))
       (values model
               (if (timer-running model)
                   (lambda ()
                     (sleep 0.01)
                     (make-instance 'tick-msg))
                   nil)))
      (t (values model nil)))))

(defmethod tui:update-message ((model timer-model) (msg tick-msg))
  (let ((remaining (get-remaining-time model)))
    (setf (timer-remaining model) remaining)
    (if (<= remaining 0)
        ;; Timer finished
        (progn
          (setf (timer-running model) nil)
          (values model (lambda () (make-instance 'timeout-msg))))
        ;; Keep ticking
        (if (timer-running model)
            (values model
                    (lambda ()
                      (sleep 0.01)
                      (make-instance 'tick-msg)))
            (values model nil)))))

(defmethod tui:update-message ((model timer-model) (msg timeout-msg))
  (setf (timer-quitting model) t)
  (values model (tui:quit-cmd)))

;;; View
(defmethod tui:view ((model timer-model))
  (let ((remaining (get-remaining-time model)))
    (if (<= remaining 0)
        (format nil "~%All done!~%")
        (format nil "~%Exiting in ~,2F seconds~%~%~
                     Controls:~%~
                     s    ~A~%~
                     r    Reset~%~
                     q    Quit~%~%"
                remaining
                (if (timer-running model) "Stop" "Start")))))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'timer-model))))
    (tui:run program)))

(eval-when (:load-toplevel :execute)
  (main))
