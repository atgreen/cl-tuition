;;; borders.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Borders example - demonstrates border styles

(asdf:load-system :tuition)

((defpackage #:tuition-example-borders
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-borders)

;;; Model
(defclass borders-model ()
  ((quitting :initform nil :accessor model-quitting)))

;;; Init
(defmethod tui:init ((model borders-model))
  nil)

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model borders-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((and (characterp key) (char= key #\q))
       (setf (model-quitting model) t)
       (values model (tui:quit-cmd)))
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c))
       (setf (model-quitting model) t)
       (values model (tui:quit-cmd)))
      (t (values model nil)))))

;;; View - demonstrate various border styles
(defmethod tui:view ((model borders-model))
  (let ((title (tui:bold "Border Styles Demo"))
        (normal-box (tui:render-border "Normal Border" tui:*border-normal*))
        (rounded-box (tui:render-border "Rounded Border" tui:*border-rounded*))
        (thick-box (tui:render-border "Thick Border" tui:*border-thick*))
        (double-box (tui:render-border "Double Border" tui:*border-double*))
        (ascii-box (tui:render-border "ASCII Border" tui:*border-ascii*))
        (colored-box (tui:render-border "Colored Border" tui:*border-normal*
                                       :fg-color tui:*fg-bright-magenta*)))

    (format nil "~A~%~%~
                 ~A~%~%~
                 ~A~%~%~
                 ~A~%~%~
                 ~A~%~%~
                 ~A~%~%~
                 ~A~%~%~
                 Press q to quit~%"
            title
            normal-box
            rounded-box
            thick-box
            double-box
            ascii-box
            colored-box)))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'borders-model))))
    (tui:run program)))

(eval-when (:load-toplevel :execute)
  (main))
