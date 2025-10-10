;;; textinput.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Text input example - demonstrates simple text input

(defpackage #:tuition-example-textinput
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-textinput)

;;; Model
(defclass textinput-model ()
  ((input :initform "" :accessor textinput-input)
   (cursor :initform 0 :accessor textinput-cursor)))

;;; Init
(defmethod tui:init ((model textinput-model))
  nil)

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model textinput-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Quit on ctrl+c or escape
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c))
       (values model (tui:quit-cmd)))
      ((eq key :escape)
       (values model (tui:quit-cmd)))
      ;; Submit on enter
      ((eq key :enter)
       (values model (tui:quit-cmd)))
      ;; Backspace
      ((eq key :backspace)
       (when (> (textinput-cursor model) 0)
         (setf (textinput-input model)
               (concatenate 'string
                            (subseq (textinput-input model) 0 (1- (textinput-cursor model)))
                            (subseq (textinput-input model) (textinput-cursor model))))
         (decf (textinput-cursor model)))
       (values model nil))
      ;; Delete (forward delete)
      ((eq key :delete)
       (when (< (textinput-cursor model) (length (textinput-input model)))
         (setf (textinput-input model)
               (concatenate 'string
                            (subseq (textinput-input model) 0 (textinput-cursor model))
                            (subseq (textinput-input model) (1+ (textinput-cursor model))))))
       (values model nil))
      ;; Left arrow
      ((eq key :left)
       (when (> (textinput-cursor model) 0)
         (decf (textinput-cursor model)))
       (values model nil))
      ;; Right arrow
      ((eq key :right)
       (when (< (textinput-cursor model) (length (textinput-input model)))
         (incf (textinput-cursor model)))
       (values model nil))
      ;; Home - move to beginning
      ((eq key :home)
       (setf (textinput-cursor model) 0)
       (values model nil))
      ;; End - move to end
      ((eq key :end)
       (setf (textinput-cursor model) (length (textinput-input model)))
       (values model nil))
      ;; Regular character input
      ((and (characterp key) (graphic-char-p key))
       (setf (textinput-input model)
             (concatenate 'string
                          (subseq (textinput-input model) 0 (textinput-cursor model))
                          (string key)
                          (subseq (textinput-input model) (textinput-cursor model))))
       (incf (textinput-cursor model))
       (values model nil))
      (t (values model nil)))))

;;; View
(defmethod tui:view ((model textinput-model))
  (let* ((input (textinput-input model))
         (cursor-pos (textinput-cursor model))
         (display (if (zerop (length input))
                      "Pikachu"  ; placeholder
                      (concatenate 'string
                                  (subseq input 0 cursor-pos)
                                  "█"  ; cursor
                                  (if (< cursor-pos (length input))
                                      (subseq input cursor-pos)
                                      "")))))
    (format nil "~%What's your favorite Pokémon?~%~%> ~A~%~%(esc to quit)~%"
            display)))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'textinput-model))))
    (tui:run program)))

#+nil
(main)
