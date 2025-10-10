;;;; SPDX-License-Identifier: MIT
;;;; Styled example - demonstrates the styling system

(defpackage #:tuition-example-styled
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-styled)

;;; Model
(defclass styled-model ()
  ((quitting :initform nil :accessor model-quitting)))

;;; Init
(defmethod tui:init ((model styled-model))
  nil)

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model styled-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((and (characterp key) (char= key #\q))
       (setf (model-quitting model) t)
       (values model (tui:quit-cmd)))
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c))
       (setf (model-quitting model) t)
       (values model (tui:quit-cmd)))
      (t (values model nil)))))

;;; View - demonstrate various styles
(defmethod tui:view ((model styled-model))
  (let ((title-style (tui:make-style
                      :bold t
                      :foreground tui:*fg-bright-magenta*
                      :padding-top 1
                      :padding-bottom 1))
        (heading-style (tui:make-style
                        :foreground tui:*fg-bright-cyan*
                        :bold t))
        (box-style (tui:make-style
                    :foreground tui:*fg-bright-white*
                    :background tui:*bg-blue*
                    :padding 1
                    :margin-top 1
                    :width 40
                    :align :center))
        (error-style (tui:make-style
                      :foreground tui:*fg-bright-red*
                      :bold t))
        (success-style (tui:make-style
                        :foreground tui:*fg-bright-green*
                        :bold t)))

    (format nil "~A~%~%~
                 ~A~%~
                 ~A~%~
                 ~A~%~
                 ~A~%~
                 ~A~%~%~
                 ~A~%~
                 ~A~%~
                 ~A~%~
                 ~A~%~%~
                 Press q to quit~%"
            (tui:render-styled title-style "Tuition Styling Demo")
            (tui:render-styled heading-style "Text Formatting:")
            (tui:bold "This is bold text")
            (tui:italic "This is italic text")
            (tui:underline "This is underlined text")
            (tui:render-styled heading-style "Colors:")
            (tui:colored "Red text" :fg tui:*fg-bright-red*)
            (tui:colored "Green text" :fg tui:*fg-bright-green*)
            (tui:colored "Blue text" :fg tui:*fg-bright-blue*)
            (tui:colored "Yellow on black" :fg tui:*fg-bright-yellow* :bg tui:*bg-black*))))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'styled-model))))
    (tui:run program)))

#+nil
(main)
