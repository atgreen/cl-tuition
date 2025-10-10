;;; textinput-component.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Demonstrates using the full-featured textinput component

(defpackage #:tuition-example-textinput-component
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-textinput-component)

;;; Model
(defclass form-model ()
  ((name-input :accessor form-name-input)
   (email-input :accessor form-email-input)
   (password-input :accessor form-password-input)
   (active-field :initform :name :accessor form-active-field)
   (submitted :initform nil :accessor form-submitted)
   (submitted-data :initform nil :accessor form-submitted-data)
   (validation-error :initform nil :accessor form-validation-error)))

;;; Init
(defmethod tui:init ((model form-model))
  ;; Create three text inputs with different configurations
  (setf (form-name-input model)
        (tui.textinput:make-textinput
         :prompt "Name: "
         :placeholder "Enter your name"
         :width 30
         :char-limit 50))

  (setf (form-email-input model)
        (tui.textinput:make-textinput
         :prompt "Email: "
         :placeholder "you@example.com"
         :width 30
         :char-limit 100
         ;; Note: validator runs on every keystroke. For email validation,
         ;; it's better to validate on submit rather than during typing.
         ;; Uncomment to see validator in action (only allows digits):
         ;; :validator (lambda (s) (every #'digit-char-p s))
         ))

  (setf (form-password-input model)
        (tui.textinput:make-textinput
         :prompt "Password: "
         :placeholder "password"
         :width 30
         :echo-mode :password
         :echo-char #\*))

  ;; Focus the first field
  (tui.textinput:textinput-focus (form-name-input model))
  (tui.textinput:textinput-blur (form-email-input model))
  (tui.textinput:textinput-blur (form-password-input model))

  nil)

;;; Update
(defmethod tui:update ((model form-model) msg)
  (cond
    ;; Already submitted - quit on any key
    ((form-submitted model)
     (values model (tui:quit-cmd)))

    ;; Handle key messages
    ((typep msg 'tui:key-msg)
     (let ((key (tui:key-msg-key msg))
           (ctrl (tui:key-msg-ctrl msg)))
       (cond
         ;; Quit on Ctrl+C
         ((and ctrl (characterp key) (char= key #\c))
          (values model (tui:quit-cmd)))

         ;; Tab - cycle through fields
         ((eq key :tab)
          (case (form-active-field model)
            (:name
             (setf (form-active-field model) :email)
             (tui.textinput:textinput-blur (form-name-input model))
             (tui.textinput:textinput-focus (form-email-input model)))
            (:email
             (setf (form-active-field model) :password)
             (tui.textinput:textinput-blur (form-email-input model))
             (tui.textinput:textinput-focus (form-password-input model)))
            (:password
             (setf (form-active-field model) :name)
             (tui.textinput:textinput-blur (form-password-input model))
             (tui.textinput:textinput-focus (form-name-input model))))
          (values model nil))

         ;; Enter - submit form (with validation)
         ((eq key :enter)
          (let ((email (tui.textinput:textinput-value (form-email-input model))))
            ;; Validate email on submit (not during typing)
            (if (and (> (length email) 0) (find #\@ email))
                (progn
                  (setf (form-submitted model) t)
                  (setf (form-validation-error model) nil)
                  (setf (form-submitted-data model)
                        (list :name (tui.textinput:textinput-value (form-name-input model))
                              :email email
                              :password (tui.textinput:textinput-value (form-password-input model)))))
                ;; Show validation error if email is invalid
                (setf (form-validation-error model) "Email must contain @ symbol")))
          (values model nil))

         ;; Pass message to active field
         (t
          ;; Clear validation error on any keystroke
          (setf (form-validation-error model) nil)
          (let ((active-input (case (form-active-field model)
                                (:name (form-name-input model))
                                (:email (form-email-input model))
                                (:password (form-password-input model)))))
            (multiple-value-bind (new-input cmd)
                (tui.textinput:textinput-update active-input msg)
              (declare (ignore cmd))
              (case (form-active-field model)
                (:name (setf (form-name-input model) new-input))
                (:email (setf (form-email-input model) new-input))
                (:password (setf (form-password-input model) new-input)))
              (values model nil)))))))

    ;; Pass paste messages to active field
    ((typep msg 'tui:paste-msg)
     (let ((active-input (case (form-active-field model)
                           (:name (form-name-input model))
                           (:email (form-email-input model))
                           (:password (form-password-input model)))))
       (multiple-value-bind (new-input cmd)
           (tui.textinput:textinput-update active-input msg)
         (declare (ignore cmd))
         (case (form-active-field model)
           (:name (setf (form-name-input model) new-input))
           (:email (setf (form-email-input model) new-input))
           (:password (setf (form-password-input model) new-input)))
         (values model nil))))

    (t (values model nil))))

;;; View
(defmethod tui:view ((model form-model))
  (if (form-submitted model)
      ;; Show submitted data
      (let ((data (form-submitted-data model)))
        (format nil "~%Form Submitted!~%~%~
                     Name: ~A~%~
                     Email: ~A~%~
                     Password: ~A~%~%~
                     Press any key to exit.~%"
                (getf data :name)
                (getf data :email)
                (make-string (length (getf data :password)) :initial-element #\*)))

      ;; Show form
      (format nil "~%Registration Form~%~
                   ================~%~%~
                   ~A~%~
                   ~A~%~
                   ~A~%~@[~%ERROR: ~A~%~]~%~
                   Instructions:~%~
                   - TAB: Switch fields~%~
                   - ENTER: Submit (validates email)~%~
                   - Ctrl+C: Quit~%~
                   - Home/End, Ctrl+A/E: Jump to start/end~%~
                   - Alt+B/F: Move by word~%~
                   - Ctrl+K: Kill to end~%~
                   - Ctrl+U: Kill to start~%~
                   - Ctrl+W: Kill word backward~%~
                   - Ctrl+Y: Yank (paste killed text)~%~
                   - Ctrl+Z: Undo, Alt+Z: Redo~%~
                   - Alt+Backspace: Delete word backward~%~
                   - Alt+D: Delete word forward~%~
                   ~%~
                   Active field: ~A~%"
              (tui.textinput:textinput-view (form-name-input model))
              (tui.textinput:textinput-view (form-email-input model))
              (tui.textinput:textinput-view (form-password-input model))
              (form-validation-error model)
              (form-active-field model))))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'form-model))))
    (tui:run program)))

#+nil
(main)
