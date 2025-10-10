;;; table.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Table rendering sub-package inspired by Lipgloss

(defpackage #:tuition.table
  (:use #:cl)
  (:nicknames #:tui.table)
  (:documentation "Table rendering utilities for displaying tabular data.")
  (:import-from #:tuition
                #:width
                #:height
                #:visible-length
                #:split-string-by-newline
                #:render-border
                #:*border-normal*
                #:*border-hidden*)
  (:export
   ;; Table class
   #:table
   #:make-table

   ;; Configuration
   #:table-headers
   #:table-rows
   #:table-border
   #:table-border-style
   #:table-style-func
   #:table-width
   #:table-height

   ;; Building tables
   #:table-row
   #:table-clear-rows

   ;; Rendering
   #:table-render

   ;; Constants
   #:+header-row+))

(in-package #:tuition.table)

;;; Constants
(defconstant +header-row+ -1
  "Special row index for the header row in style functions.")

;;; Table class
(defclass table ()
  ((headers :initform nil :accessor table-headers
            :documentation "List of header strings")
   (rows :initform nil :accessor table-rows
         :documentation "List of rows, each row is a list of cell strings")
   (border :initform tuition:*border-normal* :accessor table-border
           :documentation "Border style to use")
   (border-style :initform nil :accessor table-border-style
                 :documentation "Style object for border rendering")
   (style-func :initform nil :accessor table-style-func
               :documentation "Function (row col) -> style for cell styling")
   (widths :initform nil :accessor table-widths
           :documentation "Column widths (nil means auto-calculate)")
   (width :initform nil :accessor table-width
          :documentation "Total table width")
   (height :initform nil :accessor table-height
           :documentation "Total table height")
   (border-top :initform t :accessor table-border-top)
   (border-bottom :initform t :accessor table-border-bottom)
   (border-left :initform t :accessor table-border-left)
   (border-right :initform t :accessor table-border-right)
   (border-header :initform t :accessor table-border-header
                  :documentation "Whether to draw border between header and body"))
  (:documentation "A table for rendering tabular data."))

(defun make-table (&key headers rows border border-style style-func)
  "Create a new table."
  (let ((tbl (make-instance 'table)))
    (when headers (setf (table-headers tbl) headers))
    (when rows (setf (table-rows tbl) rows))
    (when border (setf (table-border tbl) border))
    (when border-style (setf (table-border-style tbl) border-style))
    (when style-func (setf (table-style-func tbl) style-func))
    tbl))

;;; Building tables
(defun table-row (table &rest cells)
  "Add a row to the table."
  (alexandria:appendf (table-rows table) (list cells))
  table)

(defun table-clear-rows (table)
  "Clear all rows from the table."
  (setf (table-rows table) nil)
  table)

;;; Column width calculation
(defun calculate-column-widths (table)
  "Calculate the width of each column based on content."
  (let* ((headers (table-headers table))
         (rows (table-rows table))
         (num-cols (if headers
                       (length headers)
                       (if rows (length (first rows)) 0)))
         (widths (make-list num-cols :initial-element 0)))

    ;; Measure headers
    (when headers
      (loop for header in headers
            for i from 0
            do (setf (nth i widths)
                     (max (nth i widths)
                          (tuition:visible-length (or header ""))))))

    ;; Measure all rows
    (dolist (row rows)
      (loop for cell in row
            for i from 0
            do (when (< i num-cols)
                 (setf (nth i widths)
                       (max (nth i widths)
                            (tuition:visible-length (or cell "")))))))

    widths))

;;; Cell rendering
(defun render-cell (table text width row col)
  "Render a cell with the given text, applying style if available."
  (let* ((style-func (table-style-func table))
         (styled-text (if style-func
                          (alexandria:if-let (style (funcall style-func row col))
                            (tuition:render-styled style text)
                            text)
                          text))
         (visible-len (tuition:visible-length styled-text))
         (padding (max 0 (- width visible-len))))
    (format nil " ~A~A " styled-text (make-string padding :initial-element #\Space))))

;;; Table rendering
(defun table-render (table)
  "Render the table to a string."
  (let* ((headers (table-headers table))
         (rows (table-rows table))
         (border (table-border table))
         (widths (or (table-widths table) (calculate-column-widths table)))
         (num-cols (length widths))
         (result nil))

    ;; Helper to render a horizontal border
    (labels ((render-h-border (left mid right)
               (with-output-to-string (s)
                 (when (table-border-left table)
                   (write-string left s))
                 (loop for w in widths
                       for i from 0
                       do (write-string (make-string (+ w 2) :initial-element
                                                    (char (slot-value border 'tuition::top) 0))
                                       s)
                          (when (and (< i (1- num-cols))
                                    (or (table-border-left table)
                                        (table-border-right table)))
                            (write-string mid s)))
                 (when (table-border-right table)
                   (write-string right s))))

             (render-row (cells row-idx)
               (with-output-to-string (s)
                 (when (table-border-left table)
                   (write-string (slot-value border 'tuition::left) s))
                 (loop for cell in cells
                       for col-idx from 0
                       for w in widths
                       do (write-string (render-cell table (or cell "") w row-idx col-idx) s)
                          (when (and (< col-idx (1- num-cols))
                                    (or (table-border-left table)
                                        (table-border-right table)))
                            (write-string (slot-value border 'tuition::left) s)))
                 (when (table-border-right table)
                   (write-string (slot-value border 'tuition::right) s)))))

      ;; Top border
      (when (table-border-top table)
        (push (render-h-border (slot-value border 'tuition::top-left)
                              (slot-value border 'tuition::top)
                              (slot-value border 'tuition::top-right))
              result))

      ;; Header row
      (when headers
        (push (render-row headers +header-row+) result)
        (when (table-border-header table)
          (push (render-h-border (slot-value border 'tuition::left)
                                (slot-value border 'tuition::top)
                                (slot-value border 'tuition::right))
                result)))

      ;; Data rows
      (loop for row in rows
            for row-idx from 0
            do (push (render-row row row-idx) result))

      ;; Bottom border
      (when (table-border-bottom table)
        (push (render-h-border (slot-value border 'tuition::bottom-left)
                              (slot-value border 'tuition::bottom)
                              (slot-value border 'tuition::bottom-right))
              result))

      (format nil "~{~A~^~%~}" (nreverse result)))))
