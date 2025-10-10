;;; components/stopwatch.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Stopwatch component - count-up timer

(defpackage #:tuition.components.stopwatch
  (:use #:cl)
  (:nicknames #:tui.stopwatch)
  (:documentation "Stopwatch component: count-up timer with tick updates.")
  (:export
   ;; Model
   #:stopwatch
   #:make-stopwatch

   ;; Accessors
   #:stopwatch-running
   #:stopwatch-elapsed
   #:stopwatch-interval

   ;; Operations
   #:stopwatch-init
   #:stopwatch-update
   #:stopwatch-view
   #:stopwatch-start
   #:stopwatch-stop
   #:stopwatch-reset
   #:stopwatch-toggle

   ;; Formatting
   #:format-duration))

(in-package #:tuition.components.stopwatch)

;;; Stopwatch model
(defclass stopwatch ()
  ((running :initform nil
            :accessor stopwatch-running
            :documentation "Whether the stopwatch is running")
   (elapsed :initform 0.0
            :accessor stopwatch-elapsed
            :documentation "Elapsed time in seconds")
   (start-time :initform nil
               :accessor stopwatch-start-time
               :documentation "Internal real time when started")
   (interval :initarg :interval
             :initform 0.1
             :accessor stopwatch-interval
             :documentation "Update interval in seconds (default 0.1 = 10 FPS)")
   (id :initform (get-universal-time)
       :accessor stopwatch-id
       :documentation "Unique ID for tick messages"))
  (:documentation "A stopwatch component that counts up."))

(defun make-stopwatch (&key (interval 0.1))
  "Create a new stopwatch.

Parameters:
- INTERVAL: Update frequency in seconds (default 0.1 for 10 FPS)"
  (make-instance 'stopwatch :interval interval))

;;; Component operations

(defun stopwatch-init (stopwatch)
  "Initialize the stopwatch. Returns nil (no command unless auto-start)."
  (declare (ignore stopwatch))
  nil)

(defun stopwatch-update (stopwatch msg)
  "Update the stopwatch with a tick message. Returns (values new-stopwatch cmd)."
  (cond
    ;; Handle tick messages when running
    ((and (typep msg 'tuition:tick-msg) (stopwatch-running stopwatch))
     ;; Update elapsed time based on real time
     (let* ((now (get-internal-real-time))
            (start (stopwatch-start-time stopwatch))
            (elapsed (/ (- now start) internal-time-units-per-second)))
       (setf (stopwatch-elapsed stopwatch) elapsed)
       ;; Return another tick command to keep updating
       (values stopwatch (stopwatch-tick-cmd stopwatch))))

    (t (values stopwatch nil))))

(defun stopwatch-view (stopwatch)
  "Render the stopwatch time."
  (format-duration (stopwatch-elapsed stopwatch)))

(defun stopwatch-start (stopwatch)
  "Start the stopwatch. Returns a tick command."
  (unless (stopwatch-running stopwatch)
    (setf (stopwatch-running stopwatch) t)
    (if (stopwatch-start-time stopwatch)
        ;; Resume: adjust start time to account for elapsed time
        (setf (stopwatch-start-time stopwatch)
              (- (get-internal-real-time)
                 (* (stopwatch-elapsed stopwatch) internal-time-units-per-second)))
        ;; First start
        (setf (stopwatch-start-time stopwatch) (get-internal-real-time))))
  (stopwatch-tick-cmd stopwatch))

(defun stopwatch-stop (stopwatch)
  "Stop the stopwatch."
  (when (stopwatch-running stopwatch)
    (setf (stopwatch-running stopwatch) nil)
    ;; Update elapsed time one last time
    (let* ((now (get-internal-real-time))
           (start (stopwatch-start-time stopwatch))
           (elapsed (/ (- now start) internal-time-units-per-second)))
      (setf (stopwatch-elapsed stopwatch) elapsed))))

(defun stopwatch-reset (stopwatch)
  "Reset the stopwatch to zero."
  (setf (stopwatch-elapsed stopwatch) 0.0
        (stopwatch-start-time stopwatch) nil
        (stopwatch-running stopwatch) nil))

(defun stopwatch-toggle (stopwatch)
  "Toggle the stopwatch between running and stopped.
Returns a tick command if starting, nil if stopping."
  (if (stopwatch-running stopwatch)
      (progn (stopwatch-stop stopwatch) nil)
      (stopwatch-start stopwatch)))

(defun stopwatch-tick-cmd (stopwatch)
  "Create a tick command for the stopwatch."
  (tuition:tick (stopwatch-interval stopwatch)))

;;; Formatting utilities

(defun format-duration (seconds &key (precision 2))
  "Format a duration in seconds as HH:MM:SS.SS

Parameters:
- SECONDS: Duration in seconds
- PRECISION: Number of decimal places for subseconds (default 2)"
  (let* ((total-secs (floor seconds))
         (subsecs (- seconds total-secs))
         (hours (floor total-secs 3600))
         (mins (floor (mod total-secs 3600) 60))
         (secs (mod total-secs 60)))
    (if (zerop precision)
        (format nil "~2,'0D:~2,'0D:~2,'0D" hours mins secs)
        (let ((format-str (format nil "~~2,'0D:~~2,'0D:~~2,'0D.~~V,'0D")))
          (format nil format-str hours mins secs precision
                  (floor (* subsecs (expt 10 precision))))))))
