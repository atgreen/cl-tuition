;;;; SPDX-License-Identifier: MIT
;;;; Paginator component for pagination UI

(defpackage #:tuition.components.paginator
  (:use #:cl)
  (:nicknames #:tui.paginator)
  (:export
   ;; Paginator creation
   #:paginator
   #:make-paginator

   ;; Display types
   #:+arabic+
   #:+dots+

   ;; Accessors
   #:paginator-type
   #:paginator-page
   #:paginator-per-page
   #:paginator-total-pages
   #:paginator-active-dot
   #:paginator-inactive-dot
   #:paginator-arabic-format

   ;; Operations
   #:paginator-init
   #:paginator-update
   #:paginator-view
   #:paginator-prev-page
   #:paginator-next-page
   #:paginator-set-total-pages
   #:paginator-get-slice-bounds
   #:paginator-items-on-page

   ;; Status checks
   #:paginator-on-first-page-p
   #:paginator-on-last-page-p))

(in-package #:tuition.components.paginator)

;;; Display type constants
(defconstant +arabic+ :arabic
  "Display pagination as numbers (1/10)")

(defconstant +dots+ :dots
  "Display pagination as dots (• ○ ○)")

;;; Paginator model
(defclass paginator ()
  ((type :initarg :type :accessor paginator-type
         :initform +arabic+
         :documentation "Display type (arabic or dots)")
   (page :initarg :page :accessor paginator-page
         :initform 0
         :documentation "Current page number (0-indexed)")
   (per-page :initarg :per-page :accessor paginator-per-page
             :initform 10
             :documentation "Number of items per page")
   (total-pages :initarg :total-pages :accessor paginator-total-pages
                :initform 1
                :documentation "Total number of pages")
   (active-dot :initarg :active-dot :accessor paginator-active-dot
               :initform "•"
               :documentation "Character for active page in dots mode")
   (inactive-dot :initarg :inactive-dot :accessor paginator-inactive-dot
                 :initform "○"
                 :documentation "Character for inactive pages in dots mode")
   (arabic-format :initarg :arabic-format :accessor paginator-arabic-format
                  :initform "~D/~D"
                  :documentation "Format string for arabic mode"))
  (:documentation "A pagination component for navigating pages of content."))

(defun make-paginator (&key (type +arabic+) (per-page 10) (total-pages 1))
  "Create a new paginator."
  (make-instance 'paginator
                 :type type
                 :per-page per-page
                 :total-pages total-pages))

;;; Helper functions

(defun paginator-set-total-pages (paginator items)
  "Calculate and set total pages from number of items. Returns total pages."
  (when (< items 1)
    (return-from paginator-set-total-pages (paginator-total-pages paginator)))

  (let* ((per-page (paginator-per-page paginator))
         (n (ceiling items per-page)))
    (setf (paginator-total-pages paginator) n)
    n))

(defun paginator-get-slice-bounds (paginator length)
  "Get start and end indices for the current page of a slice.
Returns (values start end)."
  (let* ((page (paginator-page paginator))
         (per-page (paginator-per-page paginator))
         (start (* page per-page))
         (end (min (+ start per-page) length)))
    (values start end)))

(defun paginator-items-on-page (paginator total-items)
  "Get the number of items on the current page."
  (when (< total-items 1)
    (return-from paginator-items-on-page 0))

  (multiple-value-bind (start end)
      (paginator-get-slice-bounds paginator total-items)
    (- end start)))

(defun paginator-prev-page (paginator)
  "Navigate to the previous page."
  (when (> (paginator-page paginator) 0)
    (decf (paginator-page paginator)))
  paginator)

(defun paginator-next-page (paginator)
  "Navigate to the next page."
  (unless (paginator-on-last-page-p paginator)
    (incf (paginator-page paginator)))
  paginator)

(defun paginator-on-first-page-p (paginator)
  "Check if on the first page."
  (= (paginator-page paginator) 0))

(defun paginator-on-last-page-p (paginator)
  "Check if on the last page."
  (= (paginator-page paginator)
     (1- (paginator-total-pages paginator))))

;;; Rendering

(defun paginator-dots-view (paginator)
  "Render pagination as dots."
  (let ((total (paginator-total-pages paginator))
        (current (paginator-page paginator))
        (active (paginator-active-dot paginator))
        (inactive (paginator-inactive-dot paginator))
        (result '()))
    (dotimes (i total)
      (push (if (= i current) active inactive) result))
    (format nil "~{~A~}" (nreverse result))))

(defun paginator-arabic-view (paginator)
  "Render pagination as arabic numbers."
  (let ((fmt (paginator-arabic-format paginator))
        (current (1+ (paginator-page paginator))) ; Convert to 1-indexed for display
        (total (paginator-total-pages paginator)))
    (format nil fmt current total)))

;;; TEA protocol implementation

(defun paginator-init (paginator)
  "Initialize the paginator. Returns nil (no command)."
  (declare (ignore paginator))
  nil)

(defun paginator-update (paginator msg)
  "Update paginator with a message. Returns (values new-paginator cmd)."
  (cond
    ;; Key messages
    ((tuition:key-msg-p msg)
     (let ((key (tuition:key-msg-key msg)))
       (cond
         ;; Next page: Right arrow, l, Page Down
         ((or (eq key :right)
              (eq key :page-down)
              (and (characterp key) (char= key #\l)))
          (values (paginator-next-page paginator) nil))

         ;; Previous page: Left arrow, h, Page Up
         ((or (eq key :left)
              (eq key :page-up)
              (and (characterp key) (char= key #\h)))
          (values (paginator-prev-page paginator) nil))

         ;; No match
         (t (values paginator nil)))))

    ;; Default: no change
    (t (values paginator nil))))

(defun paginator-view (paginator)
  "Render the paginator to a string."
  (case (paginator-type paginator)
    (:dots (paginator-dots-view paginator))
    (t (paginator-arabic-view paginator))))
