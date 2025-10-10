;;;; SPDX-License-Identifier: MIT
;;;; Spinner component - reusable animated spinner

(defpackage #:tuition.components.spinner
  (:use #:cl)
  (:nicknames #:tui.spinner)
  (:export
   ;; Model
   #:spinner
   #:make-spinner

   ;; Predefined spinners
   #:*spinner-line*
   #:*spinner-dot*
   #:*spinner-minidot*
   #:*spinner-jump*
   #:*spinner-pulse*
   #:*spinner-points*
   #:*spinner-globe*
   #:*spinner-moon*
   #:*spinner-monkey*
   #:*spinner-meter*
   #:*spinner-hamburger*
   #:*spinner-ellipsis*

   ;; Operations
   #:spinner-init
   #:spinner-update
   #:spinner-view
   #:spinner-tick-msg-p))

(in-package #:tuition.components.spinner)

;;; Spinner definitions
(defparameter *spinner-line*
  '("|" "/" "-" "\\")
  "Simple line spinner.")

(defparameter *spinner-dot*
  '("⣾ " "⣽ " "⣻ " "⢿ " "⡿ " "⣟ " "⣯ " "⣷ ")
  "Dot spinner.")

(defparameter *spinner-minidot*
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Mini dot spinner.")

(defparameter *spinner-jump*
  '("⢄" "⢂" "⢁" "⡁" "⡈" "⡐" "⡠")
  "Jumping spinner.")

(defparameter *spinner-pulse*
  '("█" "▓" "▒" "░")
  "Pulsing spinner.")

(defparameter *spinner-points*
  '("∙∙∙" "●∙∙" "∙●∙" "∙∙●")
  "Points spinner.")

(defparameter *spinner-globe*
  '("🌍" "🌎" "🌏")
  "Globe spinner.")

(defparameter *spinner-moon*
  '("🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘")
  "Moon phases spinner.")

(defparameter *spinner-monkey*
  '("🙈" "🙉" "🙊")
  "Monkey spinner.")

(defparameter *spinner-meter*
  '("▱▱▱" "▰▱▱" "▰▰▱" "▰▰▰" "▰▰▱" "▰▱▱" "▱▱▱")
  "Meter spinner.")

(defparameter *spinner-hamburger*
  '("☱" "☲" "☴" "☲")
  "Hamburger spinner.")

(defparameter *spinner-ellipsis*
  '("" "." ".." "...")
  "Ellipsis spinner.")

;;; Tick message
;; Use a CLOS message for ticks to enable generic dispatch if desired
(tuition:defmessage spinner-tick-msg
  ((id :initarg :id :reader spinner-tick-msg-id))
  :print-name spinner-tick)

(defun spinner-tick-msg-p (obj)
  (typep obj 'spinner-tick-msg))

;;; Spinner model
(defclass spinner ()
  ((frames :initarg :frames
           :initform *spinner-line*
           :accessor spinner-frames
           :documentation "List of animation frames")
   (fps :initarg :fps
        :initform 0.1
        :accessor spinner-fps
        :documentation "Seconds between frames")
   (frame-index :initform 0
                :accessor spinner-frame-index
                :documentation "Current frame index")
   (id :initform (get-universal-time)
       :accessor spinner-id
       :documentation "Unique ID for this spinner instance"))
  (:documentation "A spinner component for indicating activity."))

(defun make-spinner (&key (frames *spinner-line*) (fps 0.1))
  "Create a new spinner with the given frames and FPS."
  (make-instance 'spinner :frames frames :fps fps))

;;; Component operations

(defun spinner-init (spinner)
  "Initialize the spinner and return a tick command."
  (lambda ()
    (sleep (spinner-fps spinner))
    (make-spinner-tick-msg :id (spinner-id spinner))))

(defun spinner-update (spinner msg)
  "Update the spinner with a message. Returns (values new-spinner cmd)."
  (cond
    ((spinner-tick-msg-p msg)
     ;; Only respond to our own tick messages
     (when (= (spinner-tick-msg-id msg) (spinner-id spinner))
       (setf (spinner-frame-index spinner)
             (mod (1+ (spinner-frame-index spinner))
                  (length (spinner-frames spinner))))
       (values spinner
               (lambda ()
                 (sleep (spinner-fps spinner))
                 (make-spinner-tick-msg :id (spinner-id spinner))))))
    (t
     (values spinner nil))))

(defun spinner-view (spinner)
  "Render the current spinner frame."
  (let* ((frames (spinner-frames spinner))
         (index (spinner-frame-index spinner)))
    (if (< index (length frames))
        (nth index frames)
        "")))
