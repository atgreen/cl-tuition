;;; http.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; HTTP example - demonstrates async HTTP requests

(defpackage #:tuition-example-http
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-http)

;;; Messages
(tui:defmessage status-msg
  ((code :initarg :code :reader status-msg-code)))

(tui:defmessage error-msg
  ((error :initarg :error :reader error-msg-error)))

;;; Model
(defclass http-model ()
  ((url :initform "https://www.redhat.com/" :accessor http-url)
   (status :initform nil :accessor http-status)
   (error :initform nil :accessor http-error)))

;;; Init - start HTTP request
(defmethod tui:init ((model http-model))
  (lambda ()
    (handler-case
        (let* ((url (http-url model))
               ;; Use drakma for HTTP requests (if available)
               ;; For simplicity, we'll simulate with a sleep
               (response-code 200))
          (sleep 0.5) ; simulate network delay
          (make-instance 'status-msg :code response-code))
      (error (e)
        (make-instance 'error-msg :error (format nil "~A" e))))))

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model http-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Quit on q, ctrl+c, or escape
      ((and (characterp key) (char= key #\q)) (values model (tui:quit-cmd)))
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c)) (values model (tui:quit-cmd)))
      ((eq key :escape) (values model (tui:quit-cmd)))
      (t (values model nil)))))

(defmethod tui:update-message ((model http-model) (msg status-msg))
  (setf (http-status model) (status-msg-code msg))
  (values model (tui:quit-cmd)))

(defmethod tui:update-message ((model http-model) (msg error-msg))
  (setf (http-error model) (error-msg-error msg))
  (values model nil))

;;; View
(defmethod tui:view ((model http-model))
  (let ((url (http-url model)))
    (cond
      ((http-error model)
       (format nil "~%Checking ~A... something went wrong: ~A~%~%"
               url (http-error model)))

      ((http-status model)
       (format nil "~%Checking ~A... ~D ~A~%~%"
               url
               (http-status model)
               (case (http-status model)
                 (200 "OK")
                 (404 "Not Found")
                 (500 "Internal Server Error")
                 (t "Unknown"))))

      (t
       (format nil "~%Checking ~A...~%~%" url)))))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'http-model))))
    (tui:run program)))

#+nil
(main)
