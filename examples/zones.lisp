;;; zones.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Zones example - demonstrates mouse zone tracking

(defpackage #:tuition-example-zones
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-zones)

;;; Model
(defclass zones-model ()
  ((selected :initform nil :accessor model-selected)
   (buttons :initform '("Button 1" "Button 2" "Button 3" "Reset")
            :accessor model-buttons)
   (click-count :initform 0 :accessor model-click-count)
   (zone-prefix :initform (tui:zone-new-prefix) :accessor model-zone-prefix)))

;;; Init
(defmethod tui:init ((model zones-model))
  ;; Initialize global zone manager
  (tui:init-global-zone-manager)
  nil)

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model zones-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((and (characterp key) (char= key #\q)) (values model (tui:quit-cmd)))
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c)) (values model (tui:quit-cmd)))
      (t (values model nil)))))

(defmethod tui:update-message ((model zones-model) (msg tui:mouse-msg))
  (when (and (eql (tui:mouse-msg-action msg) :press)
             (eql (tui:mouse-msg-button msg) :left))
    ;; Check each button zone
    (let ((prefix (model-zone-prefix model)))
      (loop for button in (model-buttons model)
            for i from 0
            for zone-id = (format nil "~Abtn-~D" prefix i)
            for zone = (tui:zone-get zone-id)
            do (when (tui:zone-in-bounds-p zone msg)
                 (cond
                   ((string= button "Reset")
                    (setf (model-click-count model) 0)
                    (setf (model-selected model) nil))
                   (t
                    (incf (model-click-count model))
                    (setf (model-selected model) button)))
                 (return)))))
  (values model nil))

;;; View
(defmethod tui:view ((model zones-model))
  (let* ((prefix (model-zone-prefix model))
         (selected (model-selected model))
         (buttons '())
         (result '()))

    ;; Create buttons with zone markers
    ;; NOTE: Don't use :padding as it creates newlines which break zones
    ;; Add spaces manually instead
    (loop for button in (model-buttons model)
          for i from 0
          for zone-id = (format nil "~Abtn-~D" prefix i)
          for is-selected = (and selected (string= button selected))
          for button-with-space = (format nil " ~A " button)
          for style = (tui:make-style
                       :foreground tui:*fg-white*
                       :background (if is-selected tui:*bg-blue* tui:*bg-black*)
                       :bold is-selected)
          for rendered = (tui:render-styled style button-with-space)
          for marked = (tui:zone-mark zone-id rendered)
          do (push marked buttons))

    ;; Build UI
    (push (tui:bold "Zone Tracking Example") result)
    (push "" result)
    (push "Click on the buttons below:" result)
    (push "" result)

    ;; Render buttons horizontally
    (push (tui:join-horizontal tui:+middle+
                               (format nil " ~{~A~^  ~} " (nreverse buttons)))
          result)

    (push "" result)
    (push (format nil "Selected: ~A" (or selected "None")) result)
    (push (format nil "Total clicks: ~D" (model-click-count model)) result)
    (push "" result)
    (push "Press q to quit" result)

    ;; Wrap entire view with zone scanner
    (tui:zone-scan (format nil "~{~A~%~}" (nreverse result)))))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'zones-model)
                                   :alt-screen t
                                   :mouse :cell-motion)))
    (tui:run program)))

#+nil
(main)
