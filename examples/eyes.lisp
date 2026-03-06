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
(defconstant +eye-width+ 15 "Width (x-radius) of each eye")
(defconstant +eye-height+ 12 "Height (y-radius) of each eye")
(defconstant +eye-spacing+ 40 "Spacing between eye centers")
(defconstant +blink-frames+ 20 "Frames in blink animation")
(defconstant +open-time-min+ 1000 "Minimum time eyes stay open (ms)")
(defconstant +open-time-max+ 4000 "Maximum time eyes stay open (ms)")

;;; Model definition
(defclass eyes-model ()
  ((width :initform 80 :accessor model-width)
   (height :initform 24 :accessor model-height)
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
    (floor (* ms internal-time-units-per-second) 1000)))

(defun update-eye-positions (model)
  "Update eye positions based on screen size."
  (let ((start-x (floor (- (model-width model) +eye-spacing+) 2)))
    (setf (eye-y model) (floor (model-height model) 2))
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
  (tui:tick 0.05))

;;; Update function
(defmethod tui:update-message ((model eyes-model) (msg tui:window-size-msg))
  (setf (model-width model) (tui:window-size-msg-width msg))
  (setf (model-height model) (tui:window-size-msg-height msg))
  (update-eye-positions model)
  (values model (tui:tick 0.05)))

(defmethod tui:update-message ((model eyes-model) (msg tui:key-press-msg))
  (let ((key (tui:key-event-code msg))
        (ctrl (tui:mod-contains (tui:key-event-mod msg) tui:+mod-ctrl+)))
    (if (or (eq key :escape)
            (and ctrl (characterp key) (char= key #\c)))
        (values model (tui:quit-cmd))
        (values model nil))))

(defmethod tui:update-message ((model eyes-model) (msg tui:tick-msg))
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

;;; Drawing functions
(defun draw-ellipse (canvas x0 y0 rx ry)
  "Draw a filled ellipse on the canvas (T = eye pixel, NIL = background)."
  (loop for y from (- ry) to ry do
    (let* ((y-ratio (/ (float y) (float ry)))
           (w (floor (* rx (sqrt (max 0.0 (- 1.0 (* y-ratio y-ratio))))))))
      (when (> w 0)  ; skip single-pixel poles
        (loop for x from (- w) to w do
          (let ((cx (+ x0 x))
                (cy (+ y0 y)))
            (when (and (>= cx 0)
                       (< cx (array-dimension canvas 1))
                       (>= cy 0)
                       (< cy (array-dimension canvas 0)))
              (setf (aref canvas cy cx) t))))))))

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

;;; View function — uses background color for solid fill (avoids ambiguous-width chars)
(defmethod tui:view ((model eyes-model))
  "Render the blinking eyes."
  (let ((canvas (make-array (list (model-height model) (model-width model))
                            :initial-element nil))
        (current-height (calculate-current-height model)))
    ;; Draw both eyes
    (loop for i from 0 to 1 do
      (draw-ellipse canvas
                    (aref (eye-positions model) i)
                    (eye-y model)
                    +eye-width+
                    current-height))
    ;; Render to string using ANSI background color for eye pixels.
    ;; Space characters are always 1 cell wide, avoiding ambiguous-width issues.
    (let ((eye-on (format nil "~C[48;2;240;240;240m" #\Escape))  ; bg #F0F0F0
          (eye-off (format nil "~C[0m" #\Escape)))
      (tui:make-view
       (with-output-to-string (s)
         (dotimes (y (model-height model))
           (when (> y 0) (format s "~%"))
           (let ((in-eye nil))
             (dotimes (x (model-width model))
               (let ((is-eye (aref canvas y x)))
                 (cond
                   ((and is-eye (not in-eye))
                    (write-string eye-on s)
                    (setf in-eye t))
                   ((and (not is-eye) in-eye)
                    (write-string eye-off s)
                    (setf in-eye nil))))
               (write-char #\Space s))
             (when in-eye
               (write-string eye-off s)))))
       :alt-screen t))))

;;; Main entry point
(defun main ()
  "Run the eyes demo."
  (let ((model (make-instance 'eyes-model)))
    (tui:run (tui:make-program model))))

;;; Run automatically when loaded
(main)
