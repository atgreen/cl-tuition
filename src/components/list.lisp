;;; components/list.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; List component - reusable scrollable list

(defpackage #:tuition.components.list
  (:use #:cl)
  (:nicknames #:tui.list)
  (:export
   ;; Model
   #:list-view
   #:make-list-view

   ;; Accessors
   #:list-items
   #:list-selected
   #:list-height

   ;; Operations
   #:list-init
   #:list-update
   #:list-view-render
   #:list-move-up
   #:list-move-down
   #:list-get-selected
   #:list-set-items))

(in-package #:tuition.components.list)

;;; List model
(defclass list-view ()
  ((items :initarg :items
          :initform '()
          :accessor list-items
          :documentation "List of items (strings)")
   (selected :initform 0
             :accessor list-selected
             :documentation "Currently selected index")
   (height :initarg :height
           :initform 10
           :accessor list-height
           :documentation "Visible height of the list")
   (offset :initform 0
           :accessor list-offset
           :documentation "Scroll offset"))
  (:documentation "A scrollable list component."))

(defun make-list-view (&key (items '()) (height 10))
  "Create a new list."
  (make-instance 'list-view :items items :height height))

;;; Component operations

(defun list-init (list-view)
  "Initialize the list. Returns nil (no command needed)."
  (declare (ignore list-view))
  nil)

(defun list-update (list-view msg)
  "Update the list with a key message. Returns (values new-list cmd)."
  (if (typep msg 'tuition:key-msg)
      (let ((key (tuition:key-msg-key msg)))
        (cond
          ;; Move up
          ((or (eq key :up) (and (characterp key) (char= key #\k)))
           (list-move-up list-view)
           (values list-view nil))

          ;; Move down
          ((or (eq key :down) (and (characterp key) (char= key #\j)))
           (list-move-down list-view)
           (values list-view nil))

          (t (values list-view nil))))
      (values list-view nil)))

(defun list-view-render (list-view)
  "Render the list."
  (let* ((items (list-items list-view))
         (selected (list-selected list-view))
         (height (list-height list-view))
         (offset (list-offset list-view))
         (visible-items (subseq items
                                (min offset (max 0 (- (length items) height)))
                                (min (length items)
                                     (+ offset height)))))
    (with-output-to-string (s)
      (loop for item in visible-items
            for i from offset
            do (format s "~A ~A~%"
                      (if (= i selected) ">" " ")
                      item)))))

;;; Helper functions

(defun list-move-up (list-view)
  "Move selection up."
  (when (> (list-selected list-view) 0)
    (decf (list-selected list-view))
    ;; Adjust scroll offset if needed
    (when (< (list-selected list-view) (list-offset list-view))
      (setf (list-offset list-view) (list-selected list-view)))))

(defun list-move-down (list-view)
  "Move selection down."
  (let ((items (list-items list-view)))
    (when (< (list-selected list-view) (1- (length items)))
      (incf (list-selected list-view))
      ;; Adjust scroll offset if needed
      (let ((max-visible (+ (list-offset list-view) (list-height list-view))))
        (when (>= (list-selected list-view) max-visible)
          (incf (list-offset list-view)))))))

(defun list-get-selected (list-view)
  "Get the currently selected item."
  (let ((items (list-items list-view))
        (selected (list-selected list-view)))
    (when (and (>= selected 0) (< selected (length items)))
      (nth selected items))))

(defun list-set-items (list-view items)
  "Set the list items and reset selection."
  (setf (list-items list-view) items)
  (setf (list-selected list-view) 0)
  (setf (list-offset list-view) 0))
