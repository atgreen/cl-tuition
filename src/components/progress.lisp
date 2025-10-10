;;; components/progress.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Progress bar component - reusable progress indicator

(defpackage #:tuition.components.progress
  (:use #:cl)
  (:nicknames #:tui.progress)
  (:documentation "Progress bar component: textual progress visualization.")
  (:export
   ;; Model
   #:progress
   #:make-progress

   ;; Accessors
   #:progress-percent
   #:progress-width
   #:progress-show-percentage
   #:progress-full-char
   #:progress-empty-char

   ;; Operations
   #:progress-init
   #:progress-update
   #:progress-view
   #:progress-set-percent
   #:progress-increment))

(in-package #:tuition.components.progress)

;;; Progress bar model
(defclass progress ()
  ((percent :initarg :percent
            :initform 0.0
            :accessor progress-percent
            :documentation "Current progress (0.0 to 1.0)")
   (width :initarg :width
          :initform 40
          :accessor progress-width
          :documentation "Width of the progress bar in characters")
   (show-percentage :initarg :show-percentage
                    :initform t
                    :accessor progress-show-percentage
                    :documentation "Whether to show percentage")
   (full-char :initarg :full-char
              :initform #\█
              :accessor progress-full-char
              :documentation "Character for filled portion")
   (empty-char :initarg :empty-char
               :initform #\░
               :accessor progress-empty-char
               :documentation "Character for empty portion"))
  (:documentation "A progress bar component."))

(defun make-progress (&key (percent 0.0) (width 40) (show-percentage t)
                          (full-char #\█) (empty-char #\░))
  "Create a new progress bar."
  (make-instance 'progress
                 :percent (max 0.0 (min 1.0 percent))
                 :width width
                 :show-percentage show-percentage
                 :full-char full-char
                 :empty-char empty-char))

;;; Component operations

(defun progress-init (progress-bar)
  "Initialize the progress bar. Returns nil (no command needed)."
  (declare (ignore progress-bar))
  nil)

(defun progress-update (progress-bar msg)
  "Update the progress bar. Currently no messages to handle.
   Returns (values new-progress cmd)."
  (declare (ignore msg))
  (values progress-bar nil))

(defun progress-view (progress-bar)
  "Render the progress bar."
  (let* ((percent (max 0.0 (min 1.0 (progress-percent progress-bar))))
         (width (progress-width progress-bar))
         (filled (floor (* percent width)))
         (empty (- width filled))
         (full-char (progress-full-char progress-bar))
         (empty-char (progress-empty-char progress-bar))
         (bar (format nil "[~A~A]"
                     (make-string filled :initial-element full-char)
                     (make-string empty :initial-element empty-char))))
    (if (progress-show-percentage progress-bar)
        (format nil "~A ~3D%" bar (floor (* percent 100)))
        bar)))

;;; Helper functions

(defun progress-set-percent (progress-bar percent)
  "Set the progress percentage (0.0 to 1.0)."
  (setf (progress-percent progress-bar)
        (max 0.0 (min 1.0 percent))))

(defun progress-increment (progress-bar amount)
  "Increment the progress by amount."
  (progress-set-percent progress-bar
                       (+ (progress-percent progress-bar) amount)))
