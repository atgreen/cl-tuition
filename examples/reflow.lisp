;;; reflow.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Reflow example - demonstrates wrap, truncate, indent

(defpackage #:tuition-example-reflow
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-reflow)

(eval-when (:load-toplevel :execute)
  (asdf:load-system :tuition))

;;; Model
(defclass reflow-model ()
  ((width :initform 40 :accessor model-width)
   (ellipsis :initform "..." :accessor model-ellipsis)
   (text :initform "I went to the woods because I wished to live deliberately, to front only the essential facts of life, and see if I could not learn what it had to teach."
         :accessor model-text)))

;;; Init
(defmethod tui:init ((model reflow-model))
  nil)

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model reflow-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((and (characterp key) (char= key #\q)) (values model (tui:quit-cmd)))
      ((eq key :left)
       (setf (model-width model) (max 10 (1- (model-width model))))
       (values model nil))
      ((eq key :right)
       (setf (model-width model) (min 100 (1+ (model-width model))))
       (values model nil))
      (t (values model nil)))))

;;; View
(defmethod tui:view ((model reflow-model))
  (let* ((w (model-width model))
         (para (model-text model))
         (wrapped (tui:wrap-text para w :indent 2 :continuation-indent 4))
         (trunc (tui:truncate-text (tui:bold "Some styled text to truncate cleanly") 20 :ellipsis (model-ellipsis model)))
         (ind (tui:indent-lines "Line A
Line B" 4)))
    (format nil "~%Reflow Demo~%~%~
                  Wrap (width ~D):~%~A~%~%~
                  Truncate (20):~%~A~%~%~
                  Indent:~%~A~%~%~
                  Controls: ←/→ adjust width • q quit~%"
            w wrapped trunc ind)))

;;; Main
(defun main ()
  (let ((program (tui:make-program (make-instance 'reflow-model))))
    (tui:run program)))

(eval-when (:load-toplevel :execute)
  (main))

#+nil
(main)
