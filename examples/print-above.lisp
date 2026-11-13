;;; print-above.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Print-above-view example - a live status line stays put while permanent
;;;; log lines scroll up into the terminal's scrollback above it.

(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:pathname-parent-directory-pathname (uiop:getcwd))) :inherit-configuration))
(asdf:load-system :tuition)

(defpackage #:tuition-example-print-above
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-print-above)

;;; Model - counts how many lines we have logged so far
(defclass log-model ()
  ((lines-printed :initarg :lines-printed :accessor lines-printed :initform 0)))

;;; Init - start the tick loop
(defmethod tui:init ((model log-model))
  (tui:tick 0.4))

;;; Each tick: print a permanent line above the view, then schedule the next tick
(defmethod tui:update-message ((model log-model) (msg tui:tick-msg))
  (incf (lines-printed model))
  (values model
          (list (tui:println (format nil "log line ~D" (lines-printed model)))
                (tui:tick 0.4))))

;;; Quit on q or Ctrl+C
(defmethod tui:update-message ((model log-model) (msg tui:key-press-msg))
  (let ((key (tui:key-event-code msg)))
    (if (or (and (characterp key) (char= key #\q))
            (and (tui:mod-contains (tui:key-event-mod msg) tui:+mod-ctrl+)
                 (characterp key) (char= key #\c)))
        (values model (tui:quit-cmd))
        (values model nil))))

;;; View - the live status line that stays pinned below the scrolling log
(defmethod tui:view ((model log-model))
  (tui:make-view
   (format nil "~%  ── live status ──~%  printed ~D line~:P above (press q to quit)~%"
           (lines-printed model))))

(defun main ()
  (tui:run (tui:make-program (make-instance 'log-model))))

(eval-when (:load-toplevel :execute)
  (main))

#+nil
(main)
