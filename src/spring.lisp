;;; spring.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Spring physics for smooth, natural animations
;;;;
;;;; This is a port of Ryan Juckett's damped harmonic oscillator algorithm,
;;;; originally written in C++ in 2008 and ported to Go by Charmbracelet.
;;;; For background see: https://www.ryanjuckett.com/damped-springs/

(in-package #:tuition)

;;; Spring physics implementation

(defstruct spring
  "A cached set of motion parameters for efficient spring simulation.
Use MAKE-SPRING-ANIMATION to create, then SPRING-UPDATE to animate."
  (pos-pos-coef 0.0d0 :type double-float)
  (pos-vel-coef 0.0d0 :type double-float)
  (vel-pos-coef 0.0d0 :type double-float)
  (vel-vel-coef 0.0d0 :type double-float))

(defconstant +epsilon+ 1.0d-8
  "Small epsilon value for floating point comparisons.
Based on the harmonica Go implementation which uses a similar approach.")

(defun fps (n)
  "Convert frames per second to time delta (seconds per frame).
Use this as the delta-time parameter when creating a spring.

Example: (make-spring-animation (fps 60) 6.0d0 0.5d0)"
  (/ 1.0d0 (float n 1.0d0)))

(defun make-spring-animation (delta-time angular-frequency damping-ratio)
  "Create a new spring with precomputed coefficients.

DELTA-TIME: Time step (use FPS helper or provide seconds per frame)
ANGULAR-FREQUENCY: Speed of animation (higher = faster), typically 1.0-10.0
DAMPING-RATIO: Oscillation behavior:
  < 1.0: Under-damped (bouncy, overshoots)
  = 1.0: Critically-damped (smooth, no overshoot)
  > 1.0: Over-damped (slow, no overshoot)

Returns a SPRING structure for use with SPRING-UPDATE.

Example:
  (defvar *my-spring* (make-spring-animation (fps 60) 6.0d0 0.5d0))

Then in your update loop:
  (multiple-value-setq (x x-velocity)
    (spring-update *my-spring* x x-velocity target-x))"

  (let ((dt (float delta-time 1.0d0))
        (omega (max 0.0d0 (float angular-frequency 1.0d0)))
        (zeta (max 0.0d0 (float damping-ratio 1.0d0))))

    ;; If angular frequency is too small, return identity (no movement)
    (when (< omega +epsilon+)
      (return-from make-spring-animation
        (make-spring :pos-pos-coef 1.0d0
                     :pos-vel-coef 0.0d0
                     :vel-pos-coef 0.0d0
                     :vel-vel-coef 1.0d0)))

    (cond
      ;; Over-damped (damping ratio > 1)
      ((> zeta (+ 1.0d0 +epsilon+))
       (let* ((za (* (- omega) zeta))
              (zb (* omega (sqrt (- (* zeta zeta) 1.0d0))))
              (z1 (- za zb))
              (z2 (+ za zb))
              (e1 (exp (* z1 dt)))
              (e2 (exp (* z2 dt)))
              (inv-two-zb (/ 1.0d0 (* 2.0d0 zb)))
              (e1-over-two-zb (* e1 inv-two-zb))
              (e2-over-two-zb (* e2 inv-two-zb))
              (z1e1-over-two-zb (* z1 e1-over-two-zb))
              (z2e2-over-two-zb (* z2 e2-over-two-zb)))

         (make-spring
          :pos-pos-coef (+ (- (* e1-over-two-zb z2) z2e2-over-two-zb) e2)
          :pos-vel-coef (- e2-over-two-zb e1-over-two-zb)
          :vel-pos-coef (* (+ (- z1e1-over-two-zb z2e2-over-two-zb) e2) z2)
          :vel-vel-coef (- z2e2-over-two-zb z1e1-over-two-zb))))

      ;; Under-damped (damping ratio < 1)
      ((< zeta (- 1.0d0 +epsilon+))
       (let* ((omega-zeta (* omega zeta))
              (alpha (* omega (sqrt (- 1.0d0 (* zeta zeta)))))
              (exp-term (exp (* (- omega-zeta) dt)))
              (cos-term (cos (* alpha dt)))
              (sin-term (sin (* alpha dt)))
              (inv-alpha (/ 1.0d0 alpha))
              (exp-sin (* exp-term sin-term))
              (exp-cos (* exp-term cos-term))
              (exp-omega-zeta-sin-over-alpha (* exp-term omega-zeta sin-term inv-alpha)))

         (make-spring
          :pos-pos-coef (+ exp-cos exp-omega-zeta-sin-over-alpha)
          :pos-vel-coef (* exp-sin inv-alpha)
          :vel-pos-coef (- (* (- exp-sin) alpha) (* omega-zeta exp-omega-zeta-sin-over-alpha))
          :vel-vel-coef (- exp-cos exp-omega-zeta-sin-over-alpha))))

      ;; Critically-damped (damping ratio = 1)
      (t
       (let* ((exp-term (exp (* (- omega) dt)))
              (time-exp (* dt exp-term))
              (time-exp-freq (* time-exp omega)))

         (make-spring
          :pos-pos-coef (+ time-exp-freq exp-term)
          :pos-vel-coef time-exp
          :vel-pos-coef (* (- omega) time-exp-freq)
          :vel-vel-coef (+ (- time-exp-freq) exp-term)))))))

(defun spring-update (spring position velocity target-position)
  "Update position and velocity values toward a target using spring physics.

SPRING: Spring created with MAKE-SPRING-ANIMATION
POSITION: Current position
VELOCITY: Current velocity
TARGET-POSITION: Equilibrium position (where spring wants to be)

Returns (values new-position new-velocity)

Example:
  (let ((x 0.0d0)
        (x-vel 0.0d0)
        (target 100.0d0)
        (spring (make-spring-animation (fps 60) 6.0d0 0.5d0)))

    ;; In update loop:
    (multiple-value-setq (x x-vel)
      (spring-update spring x x-vel target)))"

  (let* ((pos (float position 1.0d0))
         (vel (float velocity 1.0d0))
         (target (float target-position 1.0d0))
         ;; Work in equilibrium-relative space
         (old-pos (- pos target))
         (old-vel vel)
         ;; Apply spring coefficients
         (new-pos (+ (* old-pos (spring-pos-pos-coef spring))
                     (* old-vel (spring-pos-vel-coef spring))
                     target))
         (new-vel (+ (* old-pos (spring-vel-pos-coef spring))
                     (* old-vel (spring-vel-vel-coef spring)))))

    (values new-pos new-vel)))

;;; Convenience function for creating common spring types

(defun make-spring-smooth (&optional (fps 60))
  "Create a smooth, critically-damped spring (no overshoot).
Good for: smooth scrolling, menu animations, general UI movement."
  (make-spring-animation (fps fps) 6.0d0 1.0d0))

(defun make-spring-bouncy (&optional (fps 60))
  "Create a bouncy, under-damped spring (overshoots and oscillates).
Good for: playful animations, attention-grabbing effects."
  (make-spring-animation (fps fps) 6.0d0 0.5d0))

(defun make-spring-gentle (&optional (fps 60))
  "Create a gentle, over-damped spring (slow, no overshoot).
Good for: subtle animations, fade-ins, gentle transitions."
  (make-spring-animation (fps fps) 3.0d0 1.5d0))

(defun make-spring-snappy (&optional (fps 60))
  "Create a snappy, responsive spring.
Good for: button presses, quick responses, interactive elements."
  (make-spring-animation (fps fps) 10.0d0 0.8d0))
