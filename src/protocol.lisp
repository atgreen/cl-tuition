;;; protocol.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Core protocol for the Elm Architecture pattern

(in-package #:tuition)

;;; Messages
(deftype msg () t)
(deftype cmd () '(or null function list))

(defclass message () ()
  (:documentation "Root class of all messages."))

(defmacro defmessage (name slots &key print-name)
  "Define a message class NAME with SLOTS and a simple printer.
SLOTS are DEFCLASS slot specs. PRINT-NAME overrides the printed tag."
  (let ((pname (or print-name name)))
    `(progn
       (defclass ,name (message)
         ,slots)
       (defmethod print-object ((m ,name) stream)
         (print-unreadable-object (m stream :type t :identity t)
           (format stream "~A" ',pname))))))

;;; Model protocol
(defgeneric init (model)
  (:documentation "Initialize the model and return an optional initial command."))

(defgeneric update (model msg)
  (:documentation "Handle a message and return (values new-model cmd)."))

(defgeneric update-message (model msg)
  (:documentation "Dispatch helper that can be specialized by message classes. Default returns (values model nil)."))

(defmethod update ((model t) msg)
  (update-message model msg))

(defmethod update-message ((model t) (msg t))
  (declare (ignore msg))
  (values model nil))

(defgeneric view (model)
  (:documentation "Render the model to a string for display."))

;;; Built-in messages (CLOS)
(defmessage quit-msg () :print-name quit)

(defmessage key-msg
  ((key :initarg :key :reader key-msg-key)
   (alt :initarg :alt :initform nil :reader key-msg-alt)
   (ctrl :initarg :ctrl :initform nil :reader key-msg-ctrl)))

;; Bracketed paste message (ESC [ 200 ~ ... ESC [ 201 ~)
(defmessage paste-msg
  ((text :initarg :text :reader paste-msg-text)))

(defmessage window-size-msg
  ((width :initarg :width :reader window-size-msg-width)
   (height :initarg :height :reader window-size-msg-height)))

(defmessage mouse-msg
  ((x :initarg :x :reader mouse-msg-x)
   (y :initarg :y :reader mouse-msg-y)
   (button :initarg :button :reader mouse-msg-button)
   (shift :initarg :shift :initform nil :reader mouse-msg-shift)
   (alt :initarg :alt :initform nil :reader mouse-msg-alt)
   (ctrl :initarg :ctrl :initform nil :reader mouse-msg-ctrl)
   (action :initarg :action :reader mouse-msg-action)))

;; Tick message for timer/animation updates
(defmessage tick-msg
  ((time :initarg :time :initform (get-internal-real-time) :reader tick-msg-time)))

;; Suspend/resume messages
(defmessage suspend-msg () :print-name suspend)
(defmessage resume-msg () :print-name resume)

;;; Constructors and predicates (compat names)
(defun make-quit-msg ()
  (make-instance 'quit-msg))

(defun quit-msg-p (obj) (typep obj 'quit-msg))

(defun make-key-msg (&key key alt ctrl)
  (make-instance 'key-msg :key key :alt alt :ctrl ctrl))

(defun key-msg-p (obj) (typep obj 'key-msg))

(defun make-paste-msg (&key text)
  (make-instance 'paste-msg :text text))

(defun paste-msg-p (obj) (typep obj 'paste-msg))

(defun make-window-size-msg (&key width height)
  (make-instance 'window-size-msg :width width :height height))

(defun window-size-msg-p (obj) (typep obj 'window-size-msg))

(defun make-mouse-msg (&key x y button shift alt ctrl action)
  (make-instance 'mouse-msg :x x :y y :button button
                         :shift shift :alt alt :ctrl ctrl :action action))

(defun mouse-msg-p (obj) (typep obj 'mouse-msg))

(defun make-tick-msg (&key time)
  (make-instance 'tick-msg :time (or time (get-internal-real-time))))

(defun tick-msg-p (obj) (typep obj 'tick-msg))

(defun make-suspend-msg ()
  (make-instance 'suspend-msg))

(defun suspend-msg-p (obj) (typep obj 'suspend-msg))

(defun make-resume-msg ()
  (make-instance 'resume-msg))

(defun resume-msg-p (obj) (typep obj 'resume-msg))

;;; Command utilities
(defun quit-cmd ()
  "Return a command that quits the program."
  (lambda () (make-quit-msg)))

(defun batch (&rest cmds)
  "Batch multiple commands to run concurrently."
  (remove nil cmds))

(defun cmd-sequence (&rest cmds)
  "Sequence multiple commands to run in order."
  (cons :sequence (remove nil cmds)))

(defun tick (duration &optional (fn nil fn-supplied-p))
  "Create a command that waits for DURATION seconds then produces a message.
   If FN is provided, calls it to produce the message.
   If FN is not provided, returns a tick-msg.

   For recurring ticks (animations, timers), return another tick command from your
   update function when handling the tick message."
  (lambda ()
    (sleep duration)
    (if fn-supplied-p
        (funcall fn)
        (make-tick-msg))))
