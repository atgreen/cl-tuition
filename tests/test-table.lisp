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
