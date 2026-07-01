;;; table.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Table rendering utilities for formatted text output

(defpackage #:tuition.render.table
  (:use #:cl)
  (:nicknames #:tui.render.table #:tui.r.table)
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

(in-package #:tuition.render.table)

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
   (widths :initarg :widths :initform nil :accessor table-widths
           :documentation "Column widths (nil means auto-calculate). When set
narrower than a cell's content, the cell soft-wraps onto multiple lines")
   (width :initform nil :accessor table-width
          :documentation "Total table width")
   (height :initform nil :accessor table-height
           :documentation "Total table height")
   (border-top :initform t :accessor table-border-top)
   (border-bottom :initform t :accessor table-border-bottom)
   (border-left :initform t :accessor table-border-left)
   (border-right :initform t :accessor table-border-right)
   (border-header :initform t :accessor table-border-header
                  :documentation "Whether to draw border between header and body")
   (border-row :initform nil :accessor table-border-row
               :documentation "Whether to draw border between each row"))
  (:documentation "A table for rendering tabular data."))

(defun make-table (&key headers rows border border-style style-func border-row
                        widths
                        (border-top t border-top-p) (border-bottom t border-bottom-p)
                        (border-left t border-left-p) (border-right t border-right-p))
  "Create a new table."
  (let ((tbl (make-instance 'table)))
    (when headers (setf (table-headers tbl) headers))
    (when rows (setf (table-rows tbl) rows))
    (when border (setf (table-border tbl) border))
    (when border-style (setf (table-border-style tbl) border-style))
    (when style-func (setf (table-style-func tbl) style-func))
    (when border-row (setf (table-border-row tbl) border-row))
    (when widths (setf (table-widths tbl) widths))
    (when border-top-p (setf (table-border-top tbl) border-top))
    (when border-bottom-p (setf (table-border-bottom tbl) border-bottom))
    (when border-left-p (setf (table-border-left tbl) border-left))
    (when border-right-p (setf (table-border-right tbl) border-right))
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
(defun styled-cell-width (table text row col)
  "Calculate the visible width of a cell after applying its style."
  (let* ((style-func (table-style-func table))
         (styled-text (if style-func
                          (alexandria:if-let (style (funcall style-func row col))
                            (tuition:render-styled style text)
                            text)
                          text)))
    (tuition:visible-length styled-text)))

(defun calculate-column-widths (table)
  "Calculate the width of each column based on styled content."
  (let* ((headers (table-headers table))
         (rows (table-rows table))
         (num-cols (if headers
                       (length headers)
                       (if rows (length (first rows)) 0)))
         (widths (make-list num-cols :initial-element 0)))

    ;; Measure styled headers
    (when headers
      (loop for header in headers
            for i from 0
            do (setf (nth i widths)
                     (max (nth i widths)
                          (styled-cell-width table (or header "") +header-row+ i)))))

    ;; Measure all styled rows
    (loop for row in rows
          for row-idx from 0
          do (loop for cell in row
                   for col-idx from 0
                   do (when (< col-idx num-cols)
                        (setf (nth col-idx widths)
                              (max (nth col-idx widths)
                                   (styled-cell-width table (or cell "") row-idx col-idx))))))

    widths))

;;; Cell rendering
(defun render-cell-lines (table text width row col)
  "Return a list of cell lines (each padded to WIDTH), soft-wrapping when the
styled content is wider than WIDTH.  Ports the wrapping behaviour of lipgloss
table (#620): a row's height grows to the tallest wrapped cell."
  (let* ((style-func (table-style-func table))
         (styled (if style-func
                     (alexandria:if-let (style (funcall style-func row col))
                       (let ((cell-style (tuition:copy-style style)))
                         ;; Apply WIDTH so alignment (e.g. :center) is honored.
                         (setf (tuition:style-width cell-style) width)
                         (tuition:render-styled cell-style text))
                       text)
                     text))
         (lines (if (<= (tuition:visible-length styled) width)
                    (list styled)
                    (tuition:split-string-by-newline
                     (tuition:wrap-text styled width :break-words t
                                        :normalize-spaces nil)))))
    (mapcar (lambda (line)
              (let ((pad (max 0 (- width (tuition:visible-length line)))))
                (if (plusp pad)
                    (concatenate 'string line (make-string pad :initial-element #\Space))
                    line)))
            lines)))

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
    (labels ((render-h-border (left mid right fill-char)
               (with-output-to-string (s)
                 (when (table-border-left table)
                   (write-string left s))
                 (loop for w in widths
                       for i from 0
                       do (write-string (make-string w :initial-element
                                                    (char fill-char 0))
                                       s)
                          (when (< i (1- num-cols))
                            (write-string mid s)))
                 (when (table-border-right table)
                   (write-string right s))))

             (render-row (cells row-idx)
               (let* ((cell-line-list
                        (loop for cell in cells
                              for col-idx from 0
                              for w in widths
                              collect (render-cell-lines table (or cell "") w row-idx col-idx)))
                      ;; Row height grows to the tallest wrapped cell (>= 1).
                      (row-height (reduce #'max cell-line-list
                                          :key #'length :initial-value 1)))
                 (with-output-to-string (s)
                   (dotimes (h row-height)
                     (when (table-border-left table)
                       (write-string (slot-value border 'tuition::left) s))
                     (loop for col-idx below num-cols
                           for w in widths
                           for lines in cell-line-list
                           for line = (or (nth h lines)
                                          (make-string w :initial-element #\Space))
                           do (write-string line s)
                              (when (< col-idx (1- num-cols))
                                (write-string (slot-value border 'tuition::left) s)))
                     (when (table-border-right table)
                       (write-string (slot-value border 'tuition::right) s))
                     (when (< h (1- row-height))
                       (write-char #\Newline s)))))))

      ;; Top border
      (when (table-border-top table)
        (push (render-h-border (slot-value border 'tuition::top-left)
                              (slot-value border 'tuition::middle-top)
                              (slot-value border 'tuition::top-right)
                              (slot-value border 'tuition::top))
              result))

      ;; Header row
      (when headers
        (push (render-row headers +header-row+) result)
        (when (table-border-header table)
          (push (render-h-border (slot-value border 'tuition::middle-left)
                                (slot-value border 'tuition::middle)
                                (slot-value border 'tuition::middle-right)
                                (slot-value border 'tuition::top))
                result)))

      ;; Data rows
      (loop for row in rows
            for row-idx from 0
            do (when (and (table-border-row table) (> row-idx 0))
                 (push (render-h-border (slot-value border 'tuition::middle-left)
                                       (slot-value border 'tuition::middle)
                                       (slot-value border 'tuition::middle-right)
                                       (slot-value border 'tuition::top))
                       result))
               (push (render-row row row-idx) result))

      ;; Bottom border
      (when (table-border-bottom table)
        (push (render-h-border (slot-value border 'tuition::bottom-left)
                              (slot-value border 'tuition::middle-bottom)
                              (slot-value border 'tuition::bottom-right)
                              (slot-value border 'tuition::bottom))
              result))

      (format nil "~{~A~^~%~}" (nreverse result)))))
