;;; components/textinput.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
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
   (offset :initform 0
           :accessor textinput-offset
           :documentation "Horizontal scroll offset for keeping cursor visible")
   (char-limit :initarg :char-limit
               :initform 0
               :accessor textinput-char-limit
               :documentation "Maximum character limit (0 = unlimited)")
   (focused :initform t
            :accessor textinput-focused
            :documentation "Whether input is focused")
   (echo-mode :initarg :echo-mode
              :initform :normal
              :accessor textinput-echo-mode
              :documentation "Echo mode: :normal or :password")
   (echo-char :initarg :echo-char
              :initform #\*
              :accessor textinput-echo-char
              :documentation "Mask character when echo-mode is :password")
   (validator :initarg :validator
              :initform nil
              :accessor textinput-validator
              :documentation "Optional function (string->bool) to validate new values")
   (transform :initarg :transform
              :initform nil
              :accessor textinput-transform
              :documentation "Optional function (string->string) to transform edits before apply")
   (on-change :initarg :on-change
              :initform nil
              :accessor textinput-on-change
              :documentation "Optional function called with (input new-value) after change")
   (keymap :initarg :keymap
           :initform nil
           :accessor textinput-keymap
           :documentation "Optional custom key handler: (lambda (input msg) -> handledp)")
   (kill-ring :initform '()
              :accessor textinput-kill-ring)
   (undo-stack :initform '()
               :accessor textinput-undo-stack)
   (redo-stack :initform '()
               :accessor textinput-redo-stack))
  (:documentation "A single-line text input component."))

(defun make-textinput (&key (value "") (placeholder "") (prompt "> ")
                            (width 20) (char-limit 0)
                            (echo-mode :normal) (echo-char #\*)
                            validator transform on-change keymap)
  "Create a new text input."
  (make-instance 'textinput
                 :value value
                 :placeholder placeholder
                 :prompt prompt
                 :width width
                 :char-limit char-limit
                 :echo-mode echo-mode
                 :echo-char echo-char
                 :validator validator
                 :transform transform
                 :on-change on-change
                 :keymap keymap))

;;; Component operations

(defun textinput-init (input)
  "Initialize the text input. Returns nil (no command needed)."
  (declare (ignore input))
  nil)

(defun %ti-push-undo (input)
  (push (cons (textinput-value input) (textinput-cursor-pos input))
        (textinput-undo-stack input))
  (setf (textinput-redo-stack input) '()))

(defun %ti-apply-change (input new-value &optional (new-cursor nil))
  (let* ((transform (textinput-transform input))
         (validator (textinput-validator input))
         (val* (if transform (funcall transform new-value) new-value)))
    (when (or (null validator) (funcall validator val*))
      (%ti-push-undo input)
      (setf (textinput-value input) val*)
      (when new-cursor (setf (textinput-cursor-pos input) new-cursor))
      (let ((cb (textinput-on-change input)))
        (when cb (funcall cb input val*)))))
  input)

(defun %ti-prev-word-boundary (s pos)
  (let ((i (max 0 (min pos (length s)))))
    (when (> i 0)
      (decf i)
      (loop while (and (> i 0) (alphanumericp (char s i))) do (decf i))
      (when (and (> i 0) (not (alphanumericp (char s i)))) (incf i))
      i)))

(defun %ti-next-word-boundary (s pos)
  (let ((i (max 0 (min pos (length s)))) (n (length s)))
    (when (< i n)
      (loop while (and (< i n) (alphanumericp (char s i))) do (incf i))
      i)))

(defun %ti-adjust-offset (input)
  (let* ((width (max 1 (textinput-width input)))
         (pos (textinput-cursor-pos input))
         (off (textinput-offset input)))
    (cond
      ((< pos off) (setf (textinput-offset input) pos))
      ((>= pos (+ off width)) (setf (textinput-offset input) (1+ (- pos width))))))
  input)

(defun textinput-update (input msg)
  "Update the text input with a key message. Returns (values new-input cmd)."
  (cond
    ;; Custom handler first
    ((and (textinput-keymap input) (funcall (textinput-keymap input) input msg))
     (values input nil))

    ;; Paste message (bracketed paste)
    ((typep msg 'tuition:paste-msg)
     (let* ((text (tuition:paste-msg-text msg))
            (value (textinput-value input))
            (pos (textinput-cursor-pos input))
            (char-limit (textinput-char-limit input))
            (new (concatenate 'string (subseq value 0 pos) text (subseq value pos)))
            (trimmed (if (and (plusp char-limit) (> (length new) char-limit))
                         (subseq new 0 char-limit)
                         new))
            (new-pos (min (length trimmed) (+ pos (length text)))))
       (%ti-apply-change input trimmed new-pos)
       (%ti-adjust-offset input)
       (values input nil)))

    ;; Key messages
    ((and (typep msg 'tuition:key-msg) (textinput-focused input))
     (let ((key (tuition:key-msg-key msg))
           (alt (tuition:key-msg-alt msg))
           (ctrl (tuition:key-msg-ctrl msg))
           (value (textinput-value input))
           (pos (textinput-cursor-pos input)))
        (cond
          ;; Backspace
          ((and (eq key :backspace) (not alt))
           (when (> pos 0)
             (%ti-apply-change input
                               (concatenate 'string (subseq value 0 (1- pos)) (subseq value pos))
                               (1- pos)))
           (%ti-adjust-offset input)
           (values input nil))

          ;; Alt+Backspace (delete word backward)
          ((and (eq key :backspace) alt)
           (let ((b (%ti-prev-word-boundary value pos)))
             (when b
               (%ti-apply-change input
                                 (concatenate 'string (subseq value 0 b) (subseq value pos))
                                 b)
               (%ti-adjust-offset input)))
           (values input nil))

          ;; Delete
          ((and (eq key :delete) (not alt))
           (when (< pos (length value))
             (%ti-apply-change input (concatenate 'string (subseq value 0 pos)
                                                 (subseq value (1+ pos))) pos))
           (%ti-adjust-offset input)
           (values input nil))

          ;; Alt+d (delete word forward) or M-d as alt + #\d
          ((and alt (characterp key) (char= key #\d))
           (let ((f (%ti-next-word-boundary value pos)))
             (when f
               (%ti-apply-change input
                                 (concatenate 'string (subseq value 0 pos) (subseq value f))
                                 pos)))
           (values input nil))

          ;; Left arrow
          ((or (eq key :left) (and ctrl (characterp key) (char= key #\b)))
           (when (> pos 0)
             (decf (textinput-cursor-pos input))
             (%ti-adjust-offset input))
           (values input nil))

          ;; Right arrow
          ((or (eq key :right) (and ctrl (characterp key) (char= key #\f)))
           (when (< pos (length value))
             (incf (textinput-cursor-pos input))
             (%ti-adjust-offset input))
           (values input nil))

          ;; Home
          ((or (eq key :home) (and ctrl (characterp key) (char= key #\a)))
           (setf (textinput-cursor-pos input) 0)
           (%ti-adjust-offset input)
           (values input nil))

          ;; End
          ((or (eq key :end) (and ctrl (characterp key) (char= key #\e)))
           (setf (textinput-cursor-pos input) (length value))
           (%ti-adjust-offset input)
           (values input nil))

          ;; Ctrl+k (kill to end)
          ((and ctrl (characterp key) (char= key #\k))
           (let ((killed (subseq value pos)))
             (push killed (textinput-kill-ring input))
             (%ti-apply-change input (subseq value 0 pos) pos))
           (values input nil))

          ;; Ctrl+u (kill to start)
          ((and ctrl (characterp key) (char= key #\u))
           (let ((killed (subseq value 0 pos)))
             (push killed (textinput-kill-ring input))
             (%ti-apply-change input (subseq value pos) 0)
             (%ti-adjust-offset input))
           (values input nil))

          ;; Ctrl+w (kill word backward)
          ((and ctrl (characterp key) (char= key #\w))
           (let ((b (%ti-prev-word-boundary value pos)))
             (when b
               (push (subseq value b pos) (textinput-kill-ring input))
               (%ti-apply-change input (concatenate 'string (subseq value 0 b)
                                                   (subseq value pos)) b)
               (%ti-adjust-offset input)))
           (values input nil))

          ;; Alt+b (move word left)
          ((and alt (characterp key) (char= key #\b))
           (let ((b (%ti-prev-word-boundary value pos)))
             (when b (setf (textinput-cursor-pos input) b) (%ti-adjust-offset input)))
           (values input nil))

          ;; Alt+f (move word right)
          ((and alt (characterp key) (char= key #\f))
           (let ((f (%ti-next-word-boundary value pos)))
             (when f (setf (textinput-cursor-pos input) f) (%ti-adjust-offset input)))
           (values input nil))

          ;; Ctrl+d (delete at cursor)
          ((and ctrl (characterp key) (char= key #\d))
           (when (< pos (length value))
             (%ti-apply-change input (concatenate 'string (subseq value 0 pos)
                                                 (subseq value (1+ pos))) pos))
           (values input nil))

          ;; Ctrl+y (yank)
          ((and ctrl (characterp key) (char= key #\y))
           (let ((text (first (textinput-kill-ring input))))
             (when text
               (%ti-apply-change input (concatenate 'string (subseq value 0 pos) text (subseq value pos))
                                 (+ pos (length text)))))
           (values input nil))

          ;; Undo (Ctrl+z)
          ((and ctrl (characterp key) (char= key #\z))
           (let ((snap (pop (textinput-undo-stack input))))
             (when snap
               (push (cons (textinput-value input) (textinput-cursor-pos input))
                     (textinput-redo-stack input))
               (setf (textinput-value input) (car snap)
                     (textinput-cursor-pos input) (cdr snap))
               (%ti-adjust-offset input)))
           (values input nil))

          ;; Redo (Alt+z)
          ((and alt (characterp key) (char= key #\z))
           (let ((snap (pop (textinput-redo-stack input))))
             (when snap
               (push (cons (textinput-value input) (textinput-cursor-pos input))
                     (textinput-undo-stack input))
               (setf (textinput-value input) (car snap)
                     (textinput-cursor-pos input) (cdr snap))
               (%ti-adjust-offset input)))
           (values input nil))

          ;; Regular character input
          ((and (characterp key) (graphic-char-p key))
           (let* ((char-limit (textinput-char-limit input))
                  (allowed (or (zerop char-limit) (< (length value) char-limit))))
             (when allowed
               (%ti-apply-change input
                                 (concatenate 'string (subseq value 0 pos) (string key) (subseq value pos))
                                 (1+ pos))
               (%ti-adjust-offset input)))
           (values input nil))

          (t (values input nil)))))

    (t (values input nil))))

(defun textinput-view (input)
  "Render the text input."
  (let* ((value (textinput-value input))
         (cursor-pos (textinput-cursor-pos input))
         (placeholder (textinput-placeholder input))
         (prompt (textinput-prompt input))
         (focused (textinput-focused input))
         (width (max 1 (textinput-width input)))
         (is-placeholder (and (zerop (length value)) (not (zerop (length placeholder)))))
         (base (if is-placeholder placeholder value))
         (masked (if (and (eq (textinput-echo-mode input) :password)
                          (> (length base) 0)
                          (not is-placeholder))  ; Don't mask placeholder
                     (make-string (length base) :initial-element (textinput-echo-char input))
                     base))
         (clamped-cursor (min (length masked) cursor-pos))
         (new-offset (progn (%ti-adjust-offset input) (textinput-offset input)))
         (start new-offset)
         (end (min (length masked) (+ start width)))
         (visible (subseq masked start end))
         (cursor-in-window (- clamped-cursor start))
         ;; Apply reverse video to character at cursor position
         (display-plain (if (and focused (>= cursor-in-window 0) (<= cursor-in-window (length visible)))
                            (let* ((cursor-char (if (< cursor-in-window (length visible))
                                                    (string (char visible cursor-in-window))
                                                    " "))  ; Use space if at end
                                   (reversed-char (format nil "~C[7m~A~C[27m" #\Escape cursor-char #\Escape)))
                              (concatenate 'string
                                           (subseq visible 0 cursor-in-window)
                                           reversed-char
                                           (if (< cursor-in-window (length visible))
                                               (subseq visible (1+ cursor-in-window))
                                               "")))
                            visible))
         ;; Apply styling to final display (use bright-black/gray for placeholders)
         (display (if is-placeholder
                      (tuition:render-styled
                       (tuition:make-style :foreground tuition:*fg-bright-black*)
                       display-plain)
                      display-plain)))
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
