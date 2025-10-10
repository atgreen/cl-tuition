;;;; SPDX-License-Identifier: MIT
;;;; Text input component - reusable single-line text input

(defpackage #:tuition.components.textinput
  (:use #:cl)
  (:nicknames #:tui.textinput)
  (:export
   ;; Model
   #:textinput
   #:make-textinput

   ;; Accessors
   #:textinput-value
   #:textinput-placeholder
   #:textinput-prompt
   #:textinput-width
   #:textinput-char-limit
   #:textinput-focused

   ;; Operations
   #:textinput-init
   #:textinput-update
   #:textinput-view
   #:textinput-focus
   #:textinput-blur
   #:textinput-set-value
   #:textinput-reset))

(in-package #:tuition.components.textinput)

;;; Text input model
(defclass textinput ()
  ((value :initarg :value
          :initform ""
          :accessor textinput-value
          :documentation "Current input value")
   (cursor-pos :initform 0
               :accessor textinput-cursor-pos
               :documentation "Cursor position")
   (placeholder :initarg :placeholder
                :initform ""
                :accessor textinput-placeholder
                :documentation "Placeholder text when empty")
   (prompt :initarg :prompt
           :initform "> "
           :accessor textinput-prompt
           :documentation "Prompt to display before input")
   (width :initarg :width
          :initform 20
          :accessor textinput-width
          :documentation "Display width")
   (char-limit :initarg :char-limit
               :initform 0
               :accessor textinput-char-limit
               :documentation "Maximum character limit (0 = unlimited)")
   (focused :initform t
            :accessor textinput-focused
            :documentation "Whether input is focused"))
  (:documentation "A single-line text input component."))

(defun make-textinput (&key (value "") (placeholder "") (prompt "> ")
                            (width 20) (char-limit 0))
  "Create a new text input."
  (make-instance 'textinput
                 :value value
                 :placeholder placeholder
                 :prompt prompt
                 :width width
                 :char-limit char-limit))

;;; Component operations

(defun textinput-init (input)
  "Initialize the text input. Returns nil (no command needed)."
  (declare (ignore input))
  nil)

(defun textinput-update (input msg)
  "Update the text input with a key message. Returns (values new-input cmd)."
  (if (and (typep msg 'tuition:key-msg) (textinput-focused input))
      (let ((key (tuition:key-msg-key msg))
            (value (textinput-value input))
            (pos (textinput-cursor-pos input)))
        (cond
          ;; Backspace
          ((eq key :backspace)
           (when (> pos 0)
             (setf (textinput-value input)
                   (concatenate 'string
                               (subseq value 0 (1- pos))
                               (subseq value pos)))
             (decf (textinput-cursor-pos input)))
           (values input nil))

          ;; Delete
          ((eq key :delete)
           (when (< pos (length value))
             (setf (textinput-value input)
                   (concatenate 'string
                               (subseq value 0 pos)
                               (subseq value (1+ pos)))))
           (values input nil))

          ;; Left arrow
          ((eq key :left)
           (when (> pos 0)
             (decf (textinput-cursor-pos input)))
           (values input nil))

          ;; Right arrow
          ((eq key :right)
           (when (< pos (length value))
             (incf (textinput-cursor-pos input)))
           (values input nil))

          ;; Home
          ((eq key :home)
           (setf (textinput-cursor-pos input) 0)
           (values input nil))

          ;; End
          ((eq key :end)
           (setf (textinput-cursor-pos input) (length value))
           (values input nil))

          ;; Regular character input
          ((and (characterp key) (graphic-char-p key))
           (let ((char-limit (textinput-char-limit input)))
             (when (or (zerop char-limit) (< (length value) char-limit))
               (setf (textinput-value input)
                     (concatenate 'string
                                 (subseq value 0 pos)
                                 (string key)
                                 (subseq value pos)))
               (incf (textinput-cursor-pos input))))
           (values input nil))

          (t (values input nil))))
      (values input nil)))

(defun textinput-view (input)
  "Render the text input."
  (let* ((value (textinput-value input))
         (cursor-pos (textinput-cursor-pos input))
         (placeholder (textinput-placeholder input))
         (prompt (textinput-prompt input))
         (focused (textinput-focused input))
         (display-value (if (and (zerop (length value))
                                (not focused))
                           placeholder
                           value))
         (display (if (and focused (>= cursor-pos 0) (<= cursor-pos (length display-value)))
                     (concatenate 'string
                                 (subseq display-value 0 cursor-pos)
                                 "█"  ; cursor
                                 (if (< cursor-pos (length display-value))
                                     (subseq display-value cursor-pos)
                                     ""))
                     display-value)))
    (format nil "~A~A" prompt display)))

;;; Helper functions

(defun textinput-focus (input)
  "Focus the text input."
  (setf (textinput-focused input) t))

(defun textinput-blur (input)
  "Blur (unfocus) the text input."
  (setf (textinput-focused input) nil))

(defun textinput-set-value (input value)
  "Set the input value and move cursor to end."
  (setf (textinput-value input) value)
  (setf (textinput-cursor-pos input) (length value)))

(defun textinput-reset (input)
  "Reset the input to empty."
  (setf (textinput-value input) "")
  (setf (textinput-cursor-pos input) 0))
