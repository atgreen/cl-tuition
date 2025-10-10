;;;; SPDX-License-Identifier: MIT
;;;; Zones example with logging

(defpackage #:tuition-example-zones-log
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-zones-log)

(defvar *log-file* "/tmp/tuition-zones-log.log")

(defun log-msg (format-string &rest args)
  (with-open-file (stream *log-file*
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "~?~%" format-string args)))

;;; Model
(defclass zones-model ()
  ((selected :initform nil :accessor model-selected)
   (buttons :initform '("Button 1" "Button 2" "Button 3")
            :accessor model-buttons)
   (click-count :initform 0 :accessor model-click-count)
   (zone-prefix :initform (tui:zone-new-prefix) :accessor model-zone-prefix)))

;;; Init
(defmethod tui:init ((model zones-model))
  (tui:init-global-zone-manager)
  nil)

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model zones-model) (msg tui:key-msg))
  (if (and (characterp (tui:key-msg-key msg))
           (char= (tui:key-msg-key msg) #\q))
      (values model (tui:quit-cmd))
      (values model nil)))

(defmethod tui:update-message ((model zones-model) (msg tui:mouse-msg))
  (when (and (eql (tui:mouse-msg-action msg) :press)
             (eql (tui:mouse-msg-button msg) :left))
    (let ((prefix (model-zone-prefix model)))
      (log-msg "Mouse click at x=~D y=~D" (tui:mouse-msg-x msg) (tui:mouse-msg-y msg))
      (loop for button in (model-buttons model)
            for i from 0
            for zone-id = (format nil "~Abtn-~D" prefix i)
            for zone = (tui:zone-get zone-id)
            do (log-msg "  Zone ~A: ~A" zone-id
                        (if zone
                            (format nil "start=(~D,~D) end=(~D,~D)"
                                    (tui::zone-info-start-x zone)
                                    (tui::zone-info-start-y zone)
                                    (tui::zone-info-end-x zone)
                                    (tui::zone-info-end-y zone))
                            "NIL"))
               (when (and zone (tui:zone-in-bounds-p zone msg))
                 (log-msg "  -> CLICKED ~A!" button)
                 (incf (model-click-count model))
                 (setf (model-selected model) button)
                 (return)))))
  (values model nil))

;;; View
(defmethod tui:view ((model zones-model))
  (let* ((prefix (model-zone-prefix model))
         (selected (model-selected model))
         (buttons '())
         (result '()))

    ;; Create buttons
    (loop for button in (model-buttons model)
          for i from 0
          for zone-id = (format nil "~Abtn-~D" prefix i)
          for is-selected = (and selected (string= button selected))
          for button-with-space = (format nil " ~A " button)
          for style = (tui:make-style
                       :foreground (if is-selected tui:*fg-bright-white* tui:*fg-white*)
                       :background (if is-selected tui:*bg-blue* tui:*bg-black*)
                       :bold is-selected)
          for rendered = (tui:render-styled style button-with-space)
          for marked = (tui:zone-mark zone-id rendered)
          do (log-msg "Button ~D: '~A' -> visible-length=~D, styled-length=~D, marked-length=~D"
                      i button
                      (tui::visible-length rendered)
                      (length rendered)
                      (length marked))
             (log-msg "  Styled: ~S" rendered)
             (log-msg "  Marked: ~S" marked)
             (push marked buttons))

    ;; Build UI
    (push (tui:bold "Zone Tracking Example") result)
    (push "" result)
    (push "Click on the buttons below:" result)
    (push "" result)

    ;; Join buttons
    (let* ((joined (format nil " ~{~A~^  ~} " (nreverse buttons))))
      (log-msg "Joined buttons line: ~S" joined)
      (log-msg "  Length: ~D, visible-length: ~D" (length joined) (tui::visible-length joined))
      (push joined result))

    (push "" result)
    (push (format nil "Selected: ~A" (or selected "None")) result)
    (push (format nil "Total clicks: ~D" (model-click-count model)) result)
    (push "" result)
    (push "Press q to quit" result)

    ;; Scan and return
    (let ((final (format nil "~{~A~%~}" (nreverse result))))
      (log-msg "Final view has ~D lines" (count #\Newline final))
      (tui:zone-scan final))))

;;; Main
(defun main ()
  (with-open-file (stream *log-file*
                          :direction :output
                          :if-exists :supersede)
    (format stream "=== Zones Log ===~%"))

  (let ((program (tui:make-program (make-instance 'zones-model)
                                   :alt-screen t
                                   :mouse :cell-motion)))
    (tui:run program)))

#+nil
(main)
