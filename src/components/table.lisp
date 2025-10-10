;;;; SPDX-License-Identifier: MIT
;;;; Table component - table rendering

(defpackage #:tuition.components.table
  (:use #:cl)
  (:nicknames #:tui.table)
  (:export
   ;; Model
   #:table
   #:make-table

   ;; Operations
   #:table-headers
   #:table-rows
   #:table-add-row
   #:table-render
   #:table-border
   #:table-set-widths))

(in-package #:tuition.components.table)

;;; Table model
(defclass table ()
  ((headers :initarg :headers
            :initform '()
            :accessor table-headers
            :documentation "List of header strings")
   (rows :initarg :rows
         :initform '()
         :accessor table-rows
         :documentation "List of row lists")
   (border :initarg :border
           :initform tuition:*border-normal*
           :accessor table-border
           :documentation "Border style")
   (widths :initarg :widths
           :initform nil
           :accessor table-widths
           :documentation "Column widths (nil = auto)")
   (header-style :initarg :header-style
                 :initform nil
                 :accessor table-header-style
                 :documentation "Style for headers")
   (row-style :initarg :row-style
              :initform nil
              :accessor table-row-style
              :documentation "Style for rows")
   (border-color :initarg :border-color
                 :initform nil
                 :accessor table-border-color
                 :documentation "Color for borders"))
  (:documentation "A table component."))

(defun make-table (&key headers rows border
                       (border-style tuition:*border-normal*)
                       header-style row-style border-color)
  "Create a new table."
  (make-instance 'table
                 :headers (or headers '())
                 :rows (or rows '())
                 :border (or border border-style)
                 :header-style header-style
                 :row-style row-style
                 :border-color border-color))

(defun table-add-row (table row)
  "Add a row to the table."
  (setf (table-rows table)
        (append (table-rows table) (list row))))

(defun table-set-widths (table widths)
  "Set column widths."
  (setf (table-widths table) widths))

(defun calculate-column-widths (table)
  "Calculate optimal column widths."
  (let* ((headers (table-headers table))
         (rows (table-rows table))
         (num-cols (max (length headers)
                       (if rows (length (first rows)) 0)))
         (widths (make-list num-cols :initial-element 0)))

    ;; Measure headers
    (loop for header in headers
          for i from 0
          do (setf (nth i widths)
                  (max (nth i widths)
                       (tuition::visible-length (format nil "~A" header)))))

    ;; Measure rows
    (dolist (row rows)
      (loop for cell in row
            for i from 0
            do (setf (nth i widths)
                    (max (nth i widths)
                         (tuition::visible-length (format nil "~A" cell))))))

    widths))

(defun pad-cell (text width)
  "Pad text to WIDTH, isolating bidi content to protect separators.
Wraps cell text in Unicode FSI/PDI markers so RTL content doesn't
reorder border characters."
  (let* ((text-str (format nil "~A" text))
         (isolated (tuition::bidi-isolate-ltr text-str))
         (visible-len (tuition::visible-length text-str))
         (padding (max 0 (- width visible-len))))
    (format nil "~A~A" isolated (make-string padding :initial-element #\Space))))

(defun render-row (cells widths border left right separator)
  "Render a single row with explicit column separators.
LEFT/RIGHT are border glyphs; SEPARATOR is the vertical divider glyph.
We wrap separators with LTR isolates to avoid bidi reordering."
  (let* ((lri (string (code-char #x2066)))
         (pdi (string (code-char #x2069)))
         (sep (format nil "~A~A~A" lri (or separator " ") pdi))
         (l-left (format nil "~A~A~A" lri left pdi))
         (l-right (format nil "~A~A~A" lri right pdi)))
    (with-output-to-string (s)
      (format s "~A " l-left)
      (loop for cell in cells
            for width in widths
            for i from 0
            do (progn
                 (princ (pad-cell cell width) s)
                 (when (< i (1- (length widths)))
                   (format s " ~A " sep))))
      (format s " ~A" l-right))))

(defun table-render (table)
  "Render the table."
  (let* ((border (table-border table))
         (headers (table-headers table))
         (rows (table-rows table))
         (widths (or (table-widths table)
                    (calculate-column-widths table)))
         (result '())
         (border-left (slot-value border 'tuition::left))
         (border-right (slot-value border 'tuition::right))
         (border-top (slot-value border 'tuition::top))
         (border-bottom (slot-value border 'tuition::bottom))
         (border-tl (slot-value border 'tuition::top-left))
         (border-tr (slot-value border 'tuition::top-right))
         (border-bl (slot-value border 'tuition::bottom-left))
         (border-br (slot-value border 'tuition::bottom-right))
         (border-ml (slot-value border 'tuition::middle-left))
         (border-mr (slot-value border 'tuition::middle-right))
         (border-mt (slot-value border 'tuition::middle-top))
         (border-mb (slot-value border 'tuition::middle-bottom)))

    ;; Top border
    (push (format nil "~A~{~A~^ ~}~A"
                 border-tl
                 (loop for width in widths
                       collect (make-string (+ width 2)
                                           :initial-element
                                           (char border-top 0)))
                 border-tr)
          result)

    ;; Headers
    (when headers
      (push (render-row headers widths border
                       border-left border-right border-left)
            result)

      ;; Header separator
      (push (format nil "~A~{~A~^ ~}~A"
                   border-ml
                   (loop for width in widths
                         collect (make-string (+ width 2)
                                             :initial-element
                                             (char border-top 0)))
                   border-mr)
            result))

    ;; Data rows
    (dolist (row rows)
      (push (render-row row widths border
                       border-left border-right border-left)
            result))

    ;; Bottom border
    (push (format nil "~A~{~A~^ ~}~A"
                 border-bl
                 (loop for width in widths
                       collect (make-string (+ width 2)
                                           :initial-element
                                           (char border-bottom 0)))
                 border-br)
          result)

    (format nil "~{~A~^~%~}" (nreverse result))))
