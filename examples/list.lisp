;;;; SPDX-License-Identifier: MIT
;;;; List example - demonstrates scrollable list selection

(defpackage #:tuition-example-list
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-list)

;;; Model
(defclass list-model ()
  ((items :initform '("Ramen" "Tomato Soup" "Hamburgers" "Cheeseburgers"
                      "Currywurst" "Okonomiyaki" "Pasta" "Fillet Mignon"
                      "Caviar" "Just Wine")
          :accessor list-items)
   (selected :initform 0 :accessor list-selected)
   (choice :initform nil :accessor list-choice)
   (quitting :initform nil :accessor list-quitting)))

;;; Init
(defmethod tui:init ((model list-model))
  nil)

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model list-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Quit on q or ctrl+c
      ((and (characterp key) (char= key #\q))
       (setf (list-quitting model) t)
       (values model (tui:quit-cmd)))
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c))
       (setf (list-quitting model) t)
       (values model (tui:quit-cmd)))
      ;; Select on enter
      ((eq key :enter)
       (setf (list-choice model)
             (nth (list-selected model) (list-items model)))
       (values model (tui:quit-cmd)))
      ;; Move up
      ((or (eq key :up) (and (characterp key) (char= key #\k)))
       (when (> (list-selected model) 0)
         (decf (list-selected model)))
       (values model nil))
      ;; Move down
      ((or (eq key :down) (and (characterp key) (char= key #\j)))
       (when (< (list-selected model) (1- (length (list-items model))))
         (incf (list-selected model)))
       (values model nil))
      (t (values model nil)))))

;;; View
(defmethod tui:view ((model list-model))
  (cond
    ;; Show choice
    ((list-choice model)
     (format nil "~%~%  ~A? Sounds good to me.~%~%" (list-choice model)))

    ;; Show quit message
    ((list-quitting model)
     (format nil "~%~%  Not hungry? That's cool.~%~%"))

    ;; Show list
    (t
     (with-output-to-string (s)
       (format s "~%  What do you want for dinner?~%~%")
       (loop for item in (list-items model)
             for i from 0
             do (format s "~A ~D. ~A~%"
                       (if (= i (list-selected model)) ">" " ")
                       (1+ i)
                       item))
       (format s "~%  ↑/k up • ↓/j down • enter select • q quit~%")))))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'list-model))))
    (tui:run program)))

#+nil
(main)
