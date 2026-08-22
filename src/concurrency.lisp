;;; concurrency.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Single-threaded support: FIFO queues and a trivial-channels stub.
;;;;
;;;; Loaded only when :tuition-single-threaded is on *features* before load.
;;;; Opt in with:
;;;;
;;;;   (push :tuition-single-threaded *features*)
;;;;   (asdf:load-system :tuition)

(in-package #:tuition)

(defstruct st-queue
  (head nil)
  (tail nil))

(defun st-queue-push (queue item)
  (let ((cell (cons item nil)))
    (if (st-queue-tail queue)
        (setf (cdr (st-queue-tail queue)) cell)
        (setf (st-queue-head queue) cell))
    (setf (st-queue-tail queue) cell))
  item)

(defun st-queue-pop (queue)
  (let ((head (st-queue-head queue)))
    (when head
      (prog1 (car head)
        (setf (st-queue-head queue) (cdr head))
        (unless (st-queue-head queue)
          (setf (st-queue-tail queue) nil))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :trivial-channels
    (:use :cl)
    (:export #:make-channel #:sendmsg #:getmsg)))

(in-package :trivial-channels)

(defstruct channel
  (queue (tuition::make-st-queue)))

(defun sendmsg (channel msg)
  (tuition::st-queue-push (channel-queue channel) msg))

(defun getmsg (channel)
  (tuition::st-queue-pop (channel-queue channel)))

(in-package #:tuition)
