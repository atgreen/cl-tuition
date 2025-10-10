;;;; SPDX-License-Identifier: MIT
;;;; Zones test - simple button test

(defpackage #:tuition-example-zones-test
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-zones-test)

(defvar *log-file* "/tmp/tuition-zones-test.log")

(defun log-msg (format-string &rest args)
  "Log a message to the debug log file."
  (with-open-file (stream *log-file*
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "~?~%" format-string args)))

;;; Model
(defclass zones-model ()
  ((selected :initform nil :accessor model-selected)
   (click-count :initform 0 :accessor model-click-count)
   (prefix :initform (tui:zone-new-prefix) :accessor model-prefix)))

;;; Init
(defmethod tui:init ((model zones-model))
  (tui:init-global-zone-manager)
  (log-msg "Init: prefix=~A" (model-prefix model))
  nil)

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model zones-model) (msg tui:key-msg))
  (if (and (characterp (tui:key-msg-key msg))
           (char= (tui:key-msg-key msg) #\q))
      (values model (tui:quit-cmd))
      (values model nil)))

(defmethod tui:update-message ((model zones-model) (msg tui:mouse-msg))
  (log-msg "Mouse: x=~D y=~D (1-based terminal coords) action=~A button=~A"
           (tui:mouse-msg-x msg)
           (tui:mouse-msg-y msg)
           (tui:mouse-msg-action msg)
           (tui:mouse-msg-button msg))
  (log-msg "  Converts to: x=~D y=~D (0-based internal coords)"
           (1- (tui:mouse-msg-x msg))
           (1- (tui:mouse-msg-y msg)))
  (when (and (eql (tui:mouse-msg-action msg) :press)
             (eql (tui:mouse-msg-button msg) :left))
    (let* ((prefix (model-prefix model))
           (zone-id (format nil "~Abtn1" prefix))
           (zone (tui:zone-get zone-id)))
      (log-msg "Checking zone ~A: ~A" zone-id zone)
      (when zone
        (log-msg "Zone bounds: start=(~D,~D) end=(~D,~D) (0-based)"
                 (tui::zone-info-start-x zone)
                 (tui::zone-info-start-y zone)
                 (tui::zone-info-end-x zone)
                 (tui::zone-info-end-y zone))
        (if (tui:zone-in-bounds-p zone msg)
            (progn
              (log-msg "CLICKED!")
              (incf (model-click-count model)))
            (log-msg "Click outside zone bounds")))))
  (values model nil))

;;; View
(defmethod tui:view ((model zones-model))
  (let* ((prefix (model-prefix model))
         (zone-id (format nil "~Abtn1" prefix))

         ;; Create a simple styled button WITHOUT padding
         ;; Add spacing manually instead
         (button-text " [ Click Me ] ")
         (styled (tui:render-styled
                  (tui:make-style :foreground tui:*fg-bright-white*
                                  :background tui:*bg-blue*)
                  button-text))
         (marked (tui:zone-mark zone-id styled))

         ;; Build view
         (view-text (format nil "Zone Test~%~%~A~%~%Clicks: ~D~%~%Press q to quit"
                           marked
                           (model-click-count model)))

         ;; Scan and return
         (result (tui:zone-scan view-text)))

    (log-msg "View rendered, zone-id=~A" zone-id)
    (log-msg "Button text: '~A'" button-text)
    (log-msg "Styled has ~D newlines" (count #\Newline styled))
    (log-msg "Styled: ~S" styled)
    (log-msg "Marked has ~D newlines" (count #\Newline marked))
    (log-msg "View-text line by line:")
    (loop for line in (uiop:split-string view-text :separator '(#\Newline))
          for i from 0
          do (log-msg "  Line ~D: '~A'" i line))
    result))

;;; Main
(defun main ()
  ;; Clear log
  (with-open-file (stream *log-file*
                          :direction :output
                          :if-exists :supersede)
    (format stream "=== Zones Test Log ===~%"))

  (let ((program (tui:make-program (make-instance 'zones-model)
                                   :alt-screen t
                                   :mouse :cell-motion)))
    (tui:run program)))

#+nil
(main)
