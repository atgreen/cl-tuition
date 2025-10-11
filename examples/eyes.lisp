;;; eyes.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Animated blinking eyes demo. Port of the BubbleTea eyes example.
;;; Roughly converted from https://github.com/dmtrKovalenko/esp32-smooth-eye-blinking

(asdf:load-system :tuition)

(defpackage #:eyes-demo
  (:use #:cl))

(in-package #:eyes-demo)

;;; Constants
(defconstant +eye-width+ 15 "Width of each eye")
(defconstant +eye-height+ 12 "Height of each eye")
(defconstant +eye-spacing+ 40 "Spacing between eyes")
(defconstant +blink-frames+ 20 "Frames in blink animation")
(defconstant +open-time-min+ 1000 "Minimum time eyes stay open (ms)")
(defconstant +open-time-max+ 4000 "Maximum time eyes stay open (ms)")

(defconstant +eye-char+ "●" "Character for drawing eyes")
(defconstant +bg-char+ " " "Background character")

;;; Model definition
(defclass eyes-model ()
  ((width :initform 80 :accessor width)
   (height :initform 24 :accessor height)
   (eye-positions :initform (vector 0 0) :accessor eye-positions)
   (eye-y :initform 0 :accessor eye-y)
   (is-blinking :initform nil :accessor is-blinking)
   (blink-state :initform 0 :accessor blink-state)
   (last-blink :initform (get-internal-real-time) :accessor last-blink)
   (open-time :initform 0 :accessor open-time)))

;;; Helper functions
(defun random-open-time ()
  "Generate random time for eyes to stay open (in internal time units)."
  (let ((ms (+ +open-time-min+ (random (- +open-time-max+ +open-time-min+)))))
    ;; Convert milliseconds to internal time units
    (floor (* ms internal-time-units-per-second) 1000)))

(defun update-eye-positions (model)
  "Update eye positions based on screen size."
  (let ((start-x (floor (- (width model) +eye-spacing+) 2)))
    (setf (eye-y model) (floor (height model) 2))
    (setf (aref (eye-positions model) 0) start-x)
    (setf (aref (eye-positions model) 1) (+ start-x +eye-spacing+))))

(defun time-since (start-time)
  "Get time elapsed since start-time (in internal time units)."
  (- (get-internal-real-time) start-time))

;;; Initialize the model
(defmethod tui:init ((model eyes-model))
  "Initialize the eyes model."
  (setf (open-time model) (random-open-time))
  (update-eye-positions model)
  ;; Return tick command for animation (50ms = 0.05s)
  (tui:tick 0.05))

;;; Update function
(defmethod tui:update ((model eyes-model) msg)
  "Handle messages and update the model."
  (cond
    ;; Window resize
    ((tui:window-size-msg-p msg)
     (setf (width model) (tui:window-size-msg-width msg))
     (setf (height model) (tui:window-size-msg-height msg))
     (update-eye-positions model)
     (values model (tui:tick 0.05)))

    ;; Key press - quit on Ctrl+C or Esc
    ((tui:key-msg-p msg)
     (let ((key (tui:key-msg-key msg))
           (ctrl (tui:key-msg-ctrl msg)))
       (if (or (eq key :escape)
               (and ctrl (characterp key) (char= key #\c)))
           (values model (tui:quit-cmd))
           (values model (tui:tick 0.05)))))

    ;; Tick - animate blinking
    ((tui:tick-msg-p msg)
     (let ((current-time (get-internal-real-time)))

       ;; Check if it's time to start blinking
       (when (and (not (is-blinking model))
                  (>= (time-since (last-blink model)) (open-time model)))
         (setf (is-blinking model) t)
         (setf (blink-state model) 0))

       ;; Update blink animation
       (when (is-blinking model)
         (incf (blink-state model))

         ;; Check if blink is complete
         (when (>= (blink-state model) +blink-frames+)
           (setf (is-blinking model) nil)
           (setf (last-blink model) current-time)
           (setf (open-time model) (random-open-time))

           ;; 10% chance of double blink
           (when (zerop (random 10))
             (setf (open-time model)
                   (floor (* 300 internal-time-units-per-second) 1000))))))

     (values model (tui:tick 0.05)))

    ;; Default
    (t (values model (tui:tick 0.05)))))

;;; Drawing functions
(defun draw-ellipse (canvas x0 y0 rx ry)
  "Draw an ellipse on the canvas."
  (loop for y from (- ry) to ry do
    (let* ((y-ratio (/ (float y) (float ry)))
           (width (floor (* rx (sqrt (- 1.0 (* y-ratio y-ratio)))))))
      (loop for x from (- width) to width do
        (let ((canvas-x (+ x0 x))
              (canvas-y (+ y0 y)))
          ;; Check bounds
          (when (and (>= canvas-x 0)
                     (< canvas-x (array-dimension canvas 1))
                     (>= canvas-y 0)
                     (< canvas-y (array-dimension canvas 0)))
            (setf (aref canvas canvas-y canvas-x) +eye-char+)))))))

(defun calculate-current-height (model)
  "Calculate current eye height based on blink state."
  (if (not (is-blinking model))
      +eye-height+
      (let ((blink-progress 0.0))
        (if (< (blink-state model) (/ +blink-frames+ 2))
            ;; Closing eyes (with easing)
            (let ((progress (/ (float (blink-state model))
                              (float (/ +blink-frames+ 2)))))
              (setf blink-progress (- 1.0 (* progress progress))))
            ;; Opening eyes (with easing)
            (let ((progress (/ (float (- (blink-state model) (/ +blink-frames+ 2)))
                              (float (/ +blink-frames+ 2)))))
              (setf blink-progress (* progress (- 2.0 progress)))))

        (max 1 (floor (* +eye-height+ blink-progress))))))

;;; View function
(defmethod tui:view ((model eyes-model))
  "Render the blinking eyes."
  ;; Create canvas
  (let ((canvas (make-array (list (height model) (width model))
                           :initial-element +bg-char+)))

    ;; Calculate current eye height
    (let ((current-height (calculate-current-height model)))

      ;; Draw both eyes
      (loop for i from 0 to 1 do
        (draw-ellipse canvas
                     (aref (eye-positions model) i)
                     (eye-y model)
                     +eye-width+
                     current-height)))

    ;; Convert canvas to string
    (with-output-to-string (s)
      (loop for y from 0 below (height model) do
        (when (> y 0) (format s "~%"))
        (loop for x from 0 below (width model) do
          (format s "~A" (aref canvas y x)))))

    ;; Apply styling
    (tui:colored (with-output-to-string (s)
                   (loop for y from 0 below (height model) do
                     (when (> y 0) (format s "~%"))
                     (loop for x from 0 below (width model) do
                       (format s "~A" (aref canvas y x)))))
                 :fg tui:*fg-white*)))

;;; Main entry point
(defun main ()
  "Run the eyes demo."
  (let ((model (make-instance 'eyes-model)))
    (tui:run (tui:make-program model :alt-screen t :mouse nil))))

;;; Run automatically when loaded
(main)
