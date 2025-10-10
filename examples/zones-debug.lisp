;;; zones-debug.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Zones debug - test zone-scan output

(defpackage #:tuition-example-zones-debug
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-zones-debug)

(defvar *log-file* "/tmp/tuition-zones-debug.log")

(defun log-msg (format-string &rest args)
  "Log a message to the debug log file."
  (with-open-file (stream *log-file*
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "~A: ~?~%"
            (get-universal-time)
            format-string args)))

;;; Model
(defclass zones-model ()
  ((selected :initform nil :accessor model-selected)
   (buttons :initform '("Button 1" "Button 2" "Button 3")
            :accessor model-buttons)
   (click-count :initform 0 :accessor model-click-count)
   (zone-prefix :initform (tui:zone-new-prefix) :accessor model-zone-prefix)))

;;; Init
(defmethod tui:init ((model zones-model))
  (log-msg "Initializing zone manager")
  (tui:init-global-zone-manager)
  (log-msg "Zone manager initialized, prefix: ~A" (model-zone-prefix model))
  nil)

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model zones-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (if (and (characterp key) (char= key #\q))
        (progn
          (log-msg "Quit requested")
          (values model (tui:quit-cmd)))
        (values model nil))))

(defmethod tui:update-message ((model zones-model) (msg tui:mouse-msg))
  (log-msg "Mouse event: x=~D y=~D action=~A button=~A"
           (tui:mouse-msg-x msg)
           (tui:mouse-msg-y msg)
           (tui:mouse-msg-action msg)
           (tui:mouse-msg-button msg))
  (values model nil))

;;; View
(defmethod tui:view ((model zones-model))
  (let* ((prefix (model-zone-prefix model))
         (text-before-mark "Hello World")
         (marked (tui:zone-mark "test" text-before-mark))
         (scanned (tui:zone-scan marked)))

    (log-msg "Text before mark: '~A' (length=~D)"
             text-before-mark (length text-before-mark))
    (log-msg "Text after mark: '~A' (length=~D)"
             marked (length marked))
    (log-msg "Text after scan: '~A' (length=~D)"
             scanned (length scanned))

    (let* ((simple-view (format nil "Zone Debug Test~%~%~
                                     Original: '~A'~%~
                                     Marked: '~A'~%~
                                     Scanned: '~A'~%~%~
                                     Press q to quit"
                               text-before-mark
                               marked
                               scanned)))
      (log-msg "Final view length: ~D" (length simple-view))
      simple-view)))

;;; Main
(defun main ()
  ;; Clear log file
  (with-open-file (stream *log-file*
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "=== Zone Debug Log ===~%"))

  (log-msg "Starting program")
  (let ((program (tui:make-program (make-instance 'zones-model)
                                   :alt-screen t
                                   :mouse :cell-motion)))
    (tui:run program))
  (log-msg "Program exited"))

#+nil
(main)
