;;; components/timer.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Timer component - countdown timer

(defpackage #:tuition.components.timer
  (:use #:cl)
  (:nicknames #:tui.timer)
  (:documentation "Timer component: countdown with tick-based updates.")
  (:export
   ;; Model
   #:timer
   #:make-timer

   ;; Messages
   #:timer-timeout-msg
   #:timer-timeout-msg-p
   #:make-timer-timeout-msg

   ;; Accessors
   #:timer-running
   #:timer-remaining
   #:timer-duration
   #:timer-interval

   ;; Operations
   #:timer-init
   #:timer-update
   #:timer-view
   #:timer-start
   #:timer-stop
   #:timer-reset
   #:timer-toggle
   #:timer-set-duration))

(in-package #:tuition.components.timer)

;;; Timer timeout message
(tuition:defmessage timer-timeout-msg
  ((id :initarg :id :reader timer-timeout-msg-id))
  :print-name timer-timeout)

(defun timer-timeout-msg-p (obj)
  "Return true if OBJ is a timer-timeout-msg."
  (typep obj 'timer-timeout-msg))

(defun make-timer-timeout-msg (&key id)
  "Construct a timer-timeout-msg with identifier ID."
  (make-instance 'timer-timeout-msg :id id))

;;; Timer model
(defclass timer ()
  ((running :initform nil
            :accessor timer-running
            :documentation "Whether the timer is running")
   (duration :initarg :duration
             :initform 60.0
             :accessor timer-duration
             :documentation "Total timer duration in seconds")
   (remaining :initform 60.0
              :accessor timer-remaining
              :documentation "Remaining time in seconds")
   (start-time :initform nil
               :accessor timer-start-time
               :documentation "Internal real time when started")
   (interval :initarg :interval
             :initform 0.1
             :accessor timer-interval
             :documentation "Update interval in seconds (default 0.1 = 10 FPS)")
   (id :initform (get-universal-time)
       :accessor timer-id
       :documentation "Unique ID for messages"))
  (:documentation "A countdown timer component."))

(defun make-timer (&key (duration 60.0) (interval 0.1))
  "Create a new countdown timer.

Parameters:
- DURATION: Timer duration in seconds (default 60)
- INTERVAL: Update frequency in seconds (default 0.1 for 10 FPS)"
  (make-instance 'timer :duration duration :interval interval))

;;; Component operations

(defun timer-init (timer)
  "Initialize the timer. Returns nil (no command unless auto-start)."
  ;; Set remaining to duration on init
  (setf (timer-remaining timer) (timer-duration timer))
  nil)

(defun timer-update (timer msg)
  "Update the timer with a tick message. Returns (values new-timer cmd)."
  (cond
    ;; Handle tick messages when running
    ((and (typep msg 'tuition:tick-msg) (timer-running timer))
     ;; Calculate remaining time
     (let* ((now (get-internal-real-time))
            (start (timer-start-time timer))
            (elapsed (/ (- now start) internal-time-units-per-second))
            (remaining (max 0 (- (timer-duration timer) elapsed))))
       (setf (timer-remaining timer) remaining)
       (if (<= remaining 0)
           ;; Timer expired
           (progn
             (setf (timer-running timer) nil)
             (values timer (lambda () (make-timer-timeout-msg :id (timer-id timer)))))
           ;; Still running
           (values timer (timer-tick-cmd timer)))))

    (t (values timer nil))))

(defun timer-view (timer)
  "Render the timer countdown."
  (tuition.components.stopwatch:format-duration (timer-remaining timer) :precision 1))

(defun timer-start (timer)
  "Start the timer. Returns a tick command."
  (unless (timer-running timer)
    (setf (timer-running timer) t)
    (if (timer-start-time timer)
        ;; Resume: adjust start time to account for elapsed time
        (let ((elapsed (- (timer-duration timer) (timer-remaining timer))))
          (setf (timer-start-time timer)
                (- (get-internal-real-time)
                   (* elapsed internal-time-units-per-second))))
        ;; First start
        (setf (timer-start-time timer) (get-internal-real-time)
              (timer-remaining timer) (timer-duration timer))))
  (timer-tick-cmd timer))

(defun timer-stop (timer)
  "Stop the timer."
  (when (timer-running timer)
    (setf (timer-running timer) nil)
    ;; Update remaining time one last time
    (let* ((now (get-internal-real-time))
           (start (timer-start-time timer))
           (elapsed (/ (- now start) internal-time-units-per-second))
           (remaining (max 0 (- (timer-duration timer) elapsed))))
      (setf (timer-remaining timer) remaining))))

(defun timer-reset (timer)
  "Reset the timer to its full duration."
  (setf (timer-remaining timer) (timer-duration timer)
        (timer-start-time timer) nil
        (timer-running timer) nil))

(defun timer-toggle (timer)
  "Toggle the timer between running and stopped.
Returns a tick command if starting, nil if stopping."
  (if (timer-running timer)
      (progn (timer-stop timer) nil)
      (timer-start timer)))

(defun timer-set-duration (timer duration)
  "Set the timer duration in seconds."
  (setf (timer-duration timer) duration
        (timer-remaining timer) duration
        (timer-start-time timer) nil
        (timer-running timer) nil))

(defun timer-tick-cmd (timer)
  "Create a tick command for the timer."
  (tuition:tick (timer-interval timer)))
