;;; spring-animation.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Spring animation example - demonstrates smooth physics-based motion

(defpackage #:tuition-example-spring-animation
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-spring-animation)

;;; Tick message
(tui:defmessage tick-msg ())

;;; Model
(defclass spring-model ()
  ((;; Three balls with different spring types
    ball1-x :initform 0.0d0 :accessor ball1-x)
    (ball1-vel :initform 0.0d0 :accessor ball1-vel)
    (ball1-target :initform 50.0d0 :accessor ball1-target)

    (ball2-x :initform 0.0d0 :accessor ball2-x)
    (ball2-vel :initform 0.0d0 :accessor ball2-vel)
    (ball2-target :initform 50.0d0 :accessor ball2-target)

    (ball3-x :initform 0.0d0 :accessor ball3-x)
    (ball3-vel :initform 0.0d0 :accessor ball3-vel)
    (ball3-target :initform 50.0d0 :accessor ball3-target)

    ;; Springs with different characteristics
    (spring-smooth :initform (tui:make-spring-smooth 60) :reader spring-smooth)
    (spring-bouncy :initform (tui:make-spring-bouncy 60) :reader spring-bouncy)
    (spring-gentle :initform (tui:make-spring-gentle 60) :reader spring-gentle)

    ;; Counter to change target every 3 seconds
    (tick-count :initform 0 :accessor tick-count)

    (quitting :initform nil :accessor model-quitting)))

;;; Init
(defmethod tui:init ((model spring-model))
  ;; Start the animation loop
  (lambda ()
    (sleep 0.016) ; ~60 FPS
    (make-instance 'tick-msg)))

;;; Update
(defmethod tui:update ((model spring-model) (msg tick-msg))
  ;; Update all three balls with their respective springs
  (multiple-value-bind (new-x new-vel)
      (tui:spring-update (spring-smooth model)
                         (ball1-x model)
                         (ball1-vel model)
                         (ball1-target model))
    (setf (ball1-x model) new-x
          (ball1-vel model) new-vel))

  (multiple-value-bind (new-x new-vel)
      (tui:spring-update (spring-bouncy model)
                         (ball2-x model)
                         (ball2-vel model)
                         (ball2-target model))
    (setf (ball2-x model) new-x
          (ball2-vel model) new-vel))

  (multiple-value-bind (new-x new-vel)
      (tui:spring-update (spring-gentle model)
                         (ball3-x model)
                         (ball3-vel model)
                         (ball3-target model))
    (setf (ball3-x model) new-x
          (ball3-vel model) new-vel))

  ;; Change target every 180 ticks (~3 seconds at 60 FPS)
  (incf (tick-count model))
  (when (zerop (mod (tick-count model) 180))
    (let ((new-target (if (> (ball1-target model) 25.0d0) 10.0d0 50.0d0)))
      (setf (ball1-target model) new-target
            (ball2-target model) new-target
            (ball3-target model) new-target)))

  ;; Continue animation
  (values model
          (lambda ()
            (sleep 0.016)
            (make-instance 'tick-msg))))

(defmethod tui:update ((model spring-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Quit on 'q' or Ctrl+C
      ((or (and (characterp key) (char= key #\q))
           (and (characterp key) (char= key #\c) (tui:key-msg-ctrl msg)))
       (setf (model-quitting model) t)
       (values model (tui:quit-cmd)))

      ;; Change target on space
      ((and (characterp key) (char= key #\Space))
       (let ((new-target (if (> (ball1-target model) 25.0d0) 10.0d0 50.0d0)))
         (setf (ball1-target model) new-target
               (ball2-target model) new-target
               (ball3-target model) new-target))
       (values model nil))

      (t (values model nil)))))

(defmethod tui:update ((model spring-model) msg)
  (declare (ignore msg))
  (values model nil))

;;; View
(defun render-track (position label max-width)
  "Render a horizontal track with a ball at the given position."
  (let* ((pos (round position))
         (clamped-pos (max 0 (min pos max-width)))
         (before (make-string clamped-pos :initial-element #\-))
         (after (make-string (- max-width clamped-pos) :initial-element #\-)))
    (format nil "~A: ~A●~A" label before after)))

(defmethod tui:view ((model spring-model))
  (let* ((width 60)
         (smooth-track (render-track (ball1-x model) "Smooth (critically-damped)" width))
         (bouncy-track (render-track (ball2-x model) "Bouncy (under-damped)    " width))
         (gentle-track (render-track (ball3-x model) "Gentle (over-damped)     " width)))

    (format nil "~A~%~%~
                 ~A~%~%~
                 ~A~%~%~
                 Target: ~A~%~%~
                 Press SPACE to change target, 'q' to quit"
            smooth-track
            bouncy-track
            gentle-track
            (round (ball1-target model)))))

;;; Main
(defun main ()
  "Run the spring animation example."
  (let ((program (tui:make-program (make-instance 'spring-model))))
    (tui:run program)))
