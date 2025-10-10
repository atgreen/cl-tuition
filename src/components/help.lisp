;;; components/help.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Help component for auto-generated help text

(defpackage #:tuition.components.help
  (:use #:cl)
  (:nicknames #:tui.help)
  (:documentation "Help view component for displaying key bindings and shortcuts.")
  (:export
   ;; Help creation
   #:help
   #:make-help
   #:help-binding

   ;; Accessors
   #:help-width
   #:help-show-all
   #:help-short-separator
   #:help-full-separator
   #:help-ellipsis

   ;; Operations
   #:help-init
   #:help-update
   #:help-view
   #:help-short-view
   #:help-full-view

   ;; Helper
   #:make-binding))

(in-package #:tuition.components.help)

;;; Help binding structure
(defstruct help-binding
  "Represents a key binding for help display."
  (key "" :type string)
  (description "" :type string)
  (enabled t :type boolean))

(defun make-binding (key description &optional (enabled t))
  "Create a new help binding."
  (make-help-binding :key key :description description :enabled enabled))

;;; Help model
(defclass help ()
  ((width :initarg :width :accessor help-width
          :initform 0
          :documentation "Maximum width (0 = no limit)")
   (show-all :initarg :show-all :accessor help-show-all
             :initform nil
             :documentation "Whether to show full help")
   (short-separator :initarg :short-separator :accessor help-short-separator
                    :initform " • "
                    :documentation "Separator for short help")
   (full-separator :initarg :full-separator :accessor help-full-separator
                   :initform "    "
                   :documentation "Separator for full help columns")
   (ellipsis :initarg :ellipsis :accessor help-ellipsis
             :initform "…"
             :documentation "Ellipsis when truncating"))
  (:documentation "A help view component for displaying key bindings."))

(defun make-help (&key (width 0) (show-all nil))
  "Create a new help view."
  (make-instance 'help :width width :show-all show-all))

;;; Helper functions

(defun help-should-add-item-p (help total-width item-width)
  "Check if an item should be added given width constraints.
Returns (values should-add-p tail-string)."
  (let ((max-width (help-width help)))
    (if (and (> max-width 0) (> (+ total-width item-width) max-width))
        ;; Too wide - check if we can add ellipsis
        (let* ((ellipsis (help-ellipsis help))
               (tail (concatenate 'string " " ellipsis))
               (tail-width (length tail)))
          (if (< (+ total-width tail-width) max-width)
              (values nil tail)
              (values nil "")))
        ;; Fits
        (values t ""))))

(defun help-short-view (help bindings)
  "Render short (single line) help from a list of bindings."
  (when (null bindings)
    (return-from help-short-view ""))

  (let ((separator (help-short-separator help))
        (total-width 0)
        (result nil))

    (loop for binding in bindings
          for i from 0
          when (help-binding-enabled binding)
          do (let* ((sep (if (and (> total-width 0) (< i (length bindings)))
                            separator
                            ""))
                    (str (format nil "~A~A ~A"
                                sep
                                (help-binding-key binding)
                                (help-binding-description binding)))
                    (str-width (length str)))

               (multiple-value-bind (should-add tail)
                   (help-should-add-item-p help total-width str-width)
                 (unless should-add
                   (unless (string= tail "")
                     (push tail result))
                   (return))

                 (incf total-width str-width)
                 (push str result))))

    (format nil "~{~A~}" (nreverse result))))

(defun help-column-should-render-p (bindings)
  "Check if a column has any enabled bindings."
  (some #'help-binding-enabled bindings))

(defun help-full-view (help groups)
  "Render full (multi-column) help from a list of binding groups.
Each group is a list of bindings that form a column."
  (when (null groups)
    (return-from help-full-view ""))

  (let ((separator (help-full-separator help))
        (total-width 0)
        (columns nil))

    (loop for group in groups
          for i from 0
          when (and group (help-column-should-render-p group))
          do (let* ((sep (if (and (> total-width 0) (< i (length groups)))
                            separator
                            ""))
                    (enabled-bindings (serapeum:filter #'help-binding-enabled group))
                    (keys (mapcar #'help-binding-key enabled-bindings))
                    (descs (mapcar #'help-binding-description enabled-bindings))
                    (key-column (format nil "~{~A~^~%~}" keys))
                    (desc-column (format nil "~{~A~^~%~}" descs))
                    (column (format nil "~A~A ~A" sep key-column desc-column))
                    (column-width (apply #'max
                                        (mapcar #'length
                                               (tuition:split-string-by-newline column)))))

               (multiple-value-bind (should-add tail)
                   (help-should-add-item-p help total-width column-width)
                 (unless should-add
                   (unless (string= tail "")
                     (push tail columns))
                   (return))

                 (incf total-width column-width)
                 (push column columns))))

    ;; Join columns horizontally
    (if (null columns)
        ""
        (let ((column-lines (mapcar #'tuition:split-string-by-newline (nreverse columns)))
              (max-lines 0))
          ;; Find max number of lines
          (dolist (col-lines column-lines)
            (setf max-lines (max max-lines (length col-lines))))

          ;; Build result line by line
          (let ((result nil))
            (dotimes (line-idx max-lines)
              (let ((line-parts nil))
                (dolist (col-lines column-lines)
                  (push (if (< line-idx (length col-lines))
                           (nth line-idx col-lines)
                           "")
                        line-parts))
                (push (format nil "~{~A~}" (nreverse line-parts)) result)))
            (format nil "~{~A~^~%~}" (nreverse result)))))))

;;; TEA protocol implementation

(defun help-init (help)
  "Initialize the help view. Returns nil (no command)."
  (declare (ignore help))
  nil)

(defun help-update (help msg)
  "Update help with a message. Returns (values new-help cmd).
Note: help view typically doesn't handle messages directly."
  (declare (ignore msg))
  (values help nil))

(defun help-view (help bindings)
  "Render the help view. Takes either a list of bindings (short) or
a list of binding lists (full)."
  (if (help-show-all help)
      ;; Full help - expect list of lists
      (help-full-view help bindings)
      ;; Short help - expect flat list
      (if (and (consp bindings) (consp (first bindings)))
          ;; Got list of lists, flatten it for short view
          (help-short-view help (apply #'append bindings))
          ;; Got flat list
          (help-short-view help bindings))))
