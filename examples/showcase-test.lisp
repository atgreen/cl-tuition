;;; Minimal test for showcase
(asdf:load-system :tuition)

(defpackage :tuition.examples.showcase-test
  (:use :cl)
  (:local-nicknames (#:tui #:tuition)))

(in-package :tuition.examples.showcase-test)

(defclass simple-model ()
  ((height :initform 24 :accessor model-height)
   (width :initform 80 :accessor model-width)))

(defmethod tui:init ((model simple-model))
  (format t "Init called: ~Dx~D~%" (model-width model) (model-height model))
  (finish-output)
  nil)

(defmethod tui:update-message ((model simple-model) (msg tui:key-msg))
  (format t "Key pressed: ~A~%" (tui:key-msg-key msg))
  (finish-output)
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c))
       (values model (tui:quit-cmd)))
      (t (values model nil)))))

(defmethod tui:update-message ((model simple-model) (msg tui:window-size-msg))
  (setf (model-height model) (tui:window-size-msg-height msg))
  (setf (model-width model) (tui:window-size-msg-width msg))
  (format t "Window resize: ~Dx~D~%" (model-width model) (model-height model))
  (finish-output)
  (values model nil))

(defmethod tui:view ((model simple-model))
  (format t "View called: ~Dx~D~%" (model-width model) (model-height model))
  (finish-output)
  (format nil "Hello World! ~Dx~D~%Press Ctrl+C to quit"
          (model-width model) (model-height model)))

(defun main ()
  (format t "Starting program...~%")
  (finish-output)
  (tui:init-global-zone-manager)
  (let ((program (tui:make-program (make-instance 'simple-model)
                                   :alt-screen t
                                   :mouse :cell-motion)))
    (format t "Running program...~%")
    (finish-output)
    (tui:run program))
  (format t "Program ended.~%")
  (finish-output))

(eval-when (:load-toplevel :execute)
  (main))
