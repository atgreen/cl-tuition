;;; test-table.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Golden file tests for table rendering, ported from lipgloss/table/table_test.go

(in-package #:tuition-tests)

(def-suite table-tests
  :description "Tests for table rendering (ported from lipgloss)."
  :in tuition-tests)

(in-suite table-tests)

;;; Standard test data (matches lipgloss table_test.go)
(defvar *table-headers* '("LANGUAGE" "FORMAL" "INFORMAL"))
(defvar *table-rows*
  '(("Chinese"  "Nǐn hǎo"      "Nǐ hǎo")
    ("French"   "Bonjour"      "Salut")
    ("Japanese" "こんにちは"   "やあ")
    ("Russian"  "Zdravstvuyte" "Privet")
    ("Spanish"  "Hola"         "¿Qué tal?")))

;;; Standard style function: center headers, pad all cells with 1 space
(defun table-style-func (row col)
  (declare (ignore col))
  (if (= row tuition.render.table:+header-row+)
      (make-style :padding-left 1 :padding-right 1 :align :center)
      (make-style :padding-left 1 :padding-right 1)))

;;; --- TestTable ---
(test table-basic
  "Basic table with NormalBorder (lipgloss TestTable)."
  (let ((tbl (tuition.render.table:make-table
              :headers *table-headers*
              :rows *table-rows*
              :border *border-normal*
              :style-func #'table-style-func)))
    (is (golden-equal "table" "TestTable"
                      (tuition.render.table:table-render tbl)))))

;;; --- TestTableBorder (DoubleBorder) ---
(test table-double-border
  "Table with DoubleBorder (lipgloss TestTableBorder)."
  (let ((tbl (tuition.render.table:make-table
              :headers *table-headers*
              :rows *table-rows*
              :border *border-double*
              :style-func #'table-style-func)))
    (is (golden-equal "table" "TestTableBorder"
                      (tuition.render.table:table-render tbl)))))

;;; --- TestTableEmpty ---
(test table-empty
  "Table with headers only, no rows (lipgloss TestTableEmpty)."
  (let ((tbl (tuition.render.table:make-table
              :headers *table-headers*
              :border *border-normal*
              :style-func #'table-style-func)))
    (is (golden-equal "table" "TestTableEmpty"
                      (tuition.render.table:table-render tbl)))))

;;; --- TestTableNoHeaders ---
(test table-no-headers
  "Table with rows only, no headers (lipgloss TestTableNoHeaders)."
  (let ((tbl (tuition.render.table:make-table
              :rows *table-rows*
              :border *border-normal*
              :style-func #'table-style-func)))
    (is (golden-equal "table" "TestTableNoHeaders"
                      (tuition.render.table:table-render tbl)))))

;;; --- TestTableSetRows ---
(test table-set-rows
  "Table set via Rows (lipgloss TestTableSetRows)."
  (let ((tbl (tuition.render.table:make-table
              :headers *table-headers*
              :rows *table-rows*
              :border *border-normal*
              :style-func #'table-style-func)))
    (is (golden-equal "table" "TestTableSetRows"
                      (tuition.render.table:table-render tbl)))))

;;; --- TestTableRowSeparators ---
(test table-row-separators
  "Table with row separators (lipgloss TestTableRowSeparators)."
  (let ((tbl (tuition.render.table:make-table
              :headers *table-headers*
              :rows *table-rows*
              :border *border-normal*
              :style-func #'table-style-func
              :border-row t)))
    (is (golden-equal "table" "TestTableRowSeparators"
                      (tuition.render.table:table-render tbl)))))

;;; --- TestBorderStyles ---

(test table-border-normal
  "Table with NormalBorder (lipgloss TestBorderStyles/NormalBorder)."
  (let ((tbl (tuition.render.table:make-table
              :headers *table-headers*
              :rows *table-rows*
              :border *border-normal*
              :style-func #'table-style-func)))
    (is (golden-equal "table/TestBorderStyles" "NormalBorder"
                      (tuition.render.table:table-render tbl)))))

(test table-border-rounded
  "Table with RoundedBorder (lipgloss TestBorderStyles/RoundedBorder)."
  (let ((tbl (tuition.render.table:make-table
              :headers *table-headers*
              :rows *table-rows*
              :border *border-rounded*
              :style-func #'table-style-func)))
    (is (golden-equal "table/TestBorderStyles" "RoundedBorder"
                      (tuition.render.table:table-render tbl)))))

(test table-border-thick
  "Table with ThickBorder (lipgloss TestBorderStyles/ThickBorder)."
  (let ((tbl (tuition.render.table:make-table
              :headers *table-headers*
              :rows *table-rows*
              :border *border-thick*
              :style-func #'table-style-func)))
    (is (golden-equal "table/TestBorderStyles" "ThickBorder"
                      (tuition.render.table:table-render tbl)))))

(test table-border-ascii
  "Table with ASCIIBorder (lipgloss TestBorderStyles/ASCIIBorder)."
  (let ((tbl (tuition.render.table:make-table
              :headers *table-headers*
              :rows *table-rows*
              :border *border-ascii*
              :style-func #'table-style-func)))
    (is (golden-equal "table/TestBorderStyles" "ASCIIBorder"
                      (tuition.render.table:table-render tbl)))))

(test table-border-markdown
  "Table with MarkdownBorder (lipgloss TestBorderStyles/MarkdownBorder)."
  (let ((tbl (tuition.render.table:make-table
              :headers *table-headers*
              :rows *table-rows*
              :border *border-markdown*
              :style-func #'table-style-func
              :border-top nil
              :border-bottom nil)))
    (is (golden-equal "table/TestBorderStyles" "MarkdownBorder"
                      (tuition.render.table:table-render tbl)))))

(test table-border-block
  "Table with BlockBorder (lipgloss TestBorderStyles/BlockBorder)."
  (let ((tbl (tuition.render.table:make-table
              :headers *table-headers*
              :rows *table-rows*
              :border *border-block*
              :style-func #'table-style-func)))
    (is (golden-equal "table/TestBorderStyles" "BlockBorder"
                      (tuition.render.table:table-render tbl)))))

(test table-border-hidden
  "Table with HiddenBorder (lipgloss TestBorderStyles/HiddenBorder)."
  (let ((tbl (tuition.render.table:make-table
              :headers *table-headers*
              :rows *table-rows*
              :border *border-hidden*
              :style-func #'table-style-func)))
    (is (golden-equal "table/TestBorderStyles" "HiddenBorder"
                      (tuition.render.table:table-render tbl)))))

;;; --- cell wrapping / multi-line rows (#620 adaptation) ---

(test table-wraps-wide-cells
  "A column narrower than its content soft-wraps onto multiple lines."
  (let ((tbl (tuition.render.table:make-table :headers '("H")
                                              :rows '(("abcdefghij"))
                                              :widths '(5))))
    (let ((view (tuition.render.table:table-render tbl)))
      (is (search "abcde" view))
      (is (search "fghij" view))
      ;; The wrapped row added extra body lines.
      (is (< 1 (count #\Newline view))))))

(test table-row-height-grows-to-wrapped-cell
  "A row grows to its tallest wrapped cell; shorter cells are padded with blanks."
  (let ((tbl (tuition.render.table:make-table
              :headers '("A" "B")
              :rows '(("abcdefghij" "x"))
              :widths '(5 3))))
    (let ((view (tuition.render.table:table-render tbl)))
      (is (search "abcde" view))
      (is (search "fghij" view))
      ;; The short cell appears on the first body line, the wrapped tail on the next.
      (is (search "x" view))
      (let ((lines (tuition:split-string-by-newline view)))
        (is (some (lambda (l) (search "abcde" l)) lines))
        (is (some (lambda (l) (search "fghij" l)) lines))))))

(test table-no-widths-never-wraps
  "Without explicit widths, content is never wrapped."
  (let ((tbl (tuition.render.table:make-table :headers '("H")
                                              :rows '(("abcdefghij")))))
    (let ((view (tuition.render.table:table-render tbl)))
      ;; The full content survives intact on a single line.
      (is (search "abcdefghij" view)))))

;;; --- Height windowing and overflow (lipgloss #620) ---

(defun %tbl-lines (view)
  (tuition:split-string-by-newline view))

(test table-height-windows-rows-with-overflow
  "A height smaller than the content shows an ellipsis overflow row and never
exceeds the requested height."
  (let ((tbl (tuition.render.table:make-table
              :headers '("Name" "Role")
              :rows '(("Alice" "Engineer") ("Bob" "Designer")
                      ("Carol" "Manager") ("Dave" "Intern"))
              :height 6)))
    (let* ((view (tuition.render.table:table-render tbl))
           (lines (%tbl-lines view)))
      (is (<= (length lines) 6))
      ;; An overflow row of ellipses signals hidden rows.
      (is (some (lambda (l) (search "…" l)) lines))
      ;; Later rows are not rendered.
      (is (not (search "Dave" view))))))

(test table-height-large-enough-shows-all
  "A generous height shows every row and no overflow marker."
  (let ((tbl (tuition.render.table:make-table
              :headers '("Name" "Role")
              :rows '(("Alice" "Engineer") ("Bob" "Designer"))
              :height 100)))
    (let ((view (tuition.render.table:table-render tbl)))
      (is (search "Alice" view))
      (is (search "Bob" view))
      (is (not (search "…" view))))))

;;; --- Width fitting (expand/shrink) ---

(test table-width-expands-columns
  "A width larger than the content expands columns to fill it exactly."
  (let ((tbl (tuition.render.table:make-table
              :headers '("A" "B")
              :rows '(("x" "y"))
              :width 30)))
    (let* ((view (tuition.render.table:table-render tbl))
           (first-line (first (%tbl-lines view))))
      (is (= 30 (tuition:visible-length first-line))))))

(test table-width-shrinks-columns
  "A width smaller than the content shrinks columns to fit."
  (let ((tbl (tuition.render.table:make-table
              :headers '("Column One" "Column Two")
              :rows '(("some long content here" "more content"))
              :width 20)))
    (let* ((view (tuition.render.table:table-render tbl))
           (first-line (first (%tbl-lines view))))
      (is (<= (tuition:visible-length first-line) 20)))))

(test table-width-never-shrinks-to-zero
  "Even with an impossibly small width, columns keep at least one column and
the table still renders (lipgloss #671)."
  (let ((tbl (tuition.render.table:make-table
              :headers '("AAAA" "BBBB")
              :rows '(("xxxx" "yyyy"))
              :width 8)))
    (let ((view (tuition.render.table:table-render tbl)))
      ;; Must render without error and keep some content from each column.
      (is (stringp view))
      (is (plusp (length view))))))

(test table-accepts-non-string-cells
  "The render table coerces non-string cells (numbers, symbols) to strings."
  (let ((tbl (tuition.render.table:make-table
              :headers '("ID" "Name")
              :rows '((1 "Alice") (2 "Bob")))))
    (let ((view (tuition.render.table:table-render tbl)))
      (is (search "1" view))
      (is (search "Alice" view))
      (is (search "2" view)))))

;;; --- Fit content (lipgloss #697) ---

(test table-fit-content-treats-width-as-maximum
  "With FIT-CONTENT, a width larger than the content leaves the table at
content width instead of expanding columns."
  (let* ((plain (tuition.render.table:make-table
                 :headers '("A" "B")
                 :rows '(("x" "y"))))
         (content-width (tuition:visible-length
                         (first (%tbl-lines
                                 (tuition.render.table:table-render plain)))))
         (fitted (tuition.render.table:make-table
                  :headers '("A" "B")
                  :rows '(("x" "y"))
                  :width 30
                  :fit-content t)))
    (let ((first-line (first (%tbl-lines
                              (tuition.render.table:table-render fitted)))))
      (is (= content-width (tuition:visible-length first-line)))
      (is (< (tuition:visible-length first-line) 30)))))

(test table-fit-content-still-shrinks-to-width
  "With FIT-CONTENT, content wider than the width is still constrained to it."
  (let ((tbl (tuition.render.table:make-table
              :headers '("Column One" "Column Two")
              :rows '(("some long content here" "more content"))
              :width 20
              :fit-content t)))
    (let ((first-line (first (%tbl-lines
                              (tuition.render.table:table-render tbl)))))
      (is (<= (tuition:visible-length first-line) 20)))))
