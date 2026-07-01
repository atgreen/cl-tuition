;;; test-tree.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Golden file tests for tree rendering, ported from lipgloss/tree/tree_test.go

(in-package #:tuition-tests)

(def-suite tree-tests
  :description "Tests for tree rendering (ported from lipgloss)."
  :in tuition-tests)

(in-suite tree-tests)

;;; --- TestTree/before (default enumerator) ---
(test tree-default-enumerator
  "Tree with default enumerator (lipgloss TestTree/before)."
  (let ((tree (tuition.render.tree:make-tree)))
    (let ((quux-sub (tuition.render.tree:make-tree :root "Quux")))
      (tuition.render.tree:tree-child quux-sub "Foo" "Bar")
      (let ((bar-sub (tuition.render.tree:make-tree :root "Bar")))
        (tuition.render.tree:tree-child bar-sub "Qux" quux-sub "Quuux")
        (tuition.render.tree:tree-child tree "Foo" bar-sub "Baz")))
    (is (golden-equal "tree/TestTree" "before"
                      (tuition.render.tree:tree-render tree)))))

;;; --- TestTree/after (rounded enumerator) ---
(test tree-rounded-enumerator
  "Tree with rounded enumerator (lipgloss TestTree/after)."
  (let ((tree (tuition.render.tree:make-tree)))
    (let ((quux-sub (tuition.render.tree:make-tree :root "Quux")))
      (tuition.render.tree:tree-child quux-sub "Foo" "Bar")
      (let ((bar-sub (tuition.render.tree:make-tree :root "Bar")))
        (tuition.render.tree:tree-child bar-sub "Qux" quux-sub "Quuux")
        (tuition.render.tree:tree-child tree "Foo" bar-sub "Baz")))
    (setf (tuition.render.tree::tree-enumerator tree) #'tuition.render.tree:rounded-enumerator)
    (is (golden-equal "tree/TestTree" "after"
                      (tuition.render.tree:tree-render tree)))))

;;; --- TestTreeRoot ---
(test tree-with-root
  "Tree with named root (lipgloss TestTreeRoot)."
  (let ((tree (tuition.render.tree:make-tree :root "Root")))
    (let ((bar-sub (tuition.render.tree:make-tree :root "Bar")))
      (tuition.render.tree:tree-child bar-sub "Qux" "Quuux")
      (tuition.render.tree:tree-child tree "Foo" bar-sub "Baz"))
    (is (golden-equal "tree" "TestTreeRoot"
                      (tuition.render.tree:tree-render tree)))))

;;; --- TestTreeLastNodeIsSubTree ---
(test tree-last-node-subtree
  "Tree where last child is a subtree (lipgloss TestTreeLastNodeIsSubTree)."
  (let ((tree (tuition.render.tree:make-tree)))
    (let ((quux-sub (tuition.render.tree:make-tree :root "Quux")))
      (tuition.render.tree:tree-child quux-sub "Foo" "Bar")
      (let ((bar-sub (tuition.render.tree:make-tree :root "Bar")))
        (tuition.render.tree:tree-child bar-sub "Qux" quux-sub "Quuux")
        (tuition.render.tree:tree-child tree "Foo" bar-sub)))
    (is (golden-equal "tree" "TestTreeLastNodeIsSubTree"
                      (tuition.render.tree:tree-render tree)))))

;;; --- TestTreeStartsWithSubtree ---
(test tree-starts-with-subtree
  "Tree where first child is a subtree (lipgloss TestTreeStartsWithSubtree)."
  (let ((tree (tuition.render.tree:make-tree)))
    (let ((bar-sub (tuition.render.tree:make-tree :root "Bar")))
      (tuition.render.tree:tree-child bar-sub "Qux" "Quuux")
      (tuition.render.tree:tree-child tree bar-sub "Baz"))
    (is (golden-equal "tree" "TestTreeStartsWithSubtree"
                      (tuition.render.tree:tree-render tree)))))

;;; --- TestTreeMultilineNode ---
(test tree-multiline-node
  "Tree with multi-line nodes (lipgloss TestTreeMultilineNode)."
  (let ((tree (tuition.render.tree:make-tree :root (format nil "Big~%Root~%Node"))))
    (let ((quux-sub (tuition.render.tree:make-tree :root "Quux")))
      (tuition.render.tree:tree-child quux-sub "Foo" "Bar")
      (let ((bar-sub (tuition.render.tree:make-tree :root "Bar")))
        (tuition.render.tree:tree-child bar-sub
                                        (format nil "Line 1~%Line 2~%Line 3~%Line 4")
                                        quux-sub "Quuux")
        (tuition.render.tree:tree-child tree "Foo" bar-sub (format nil "Baz~%Line 2"))))
    (is (golden-equal "tree" "TestTreeMultilineNode"
                      (tuition.render.tree:tree-render tree)))))

;;; --- TestTreeAddTwoSubTreesWithoutName ---
(test tree-two-subtrees-no-name
  "Tree with two nameless subtrees (lipgloss TestTreeAddTwoSubTreesWithoutName)."
  (let ((tree (tuition.render.tree:make-tree))
        (sub1 (tuition.render.tree:make-tree))
        (sub2 (tuition.render.tree:make-tree)))
    (tuition.render.tree:tree-child sub1 "Qux" "Qux" "Qux" "Qux" "Qux")
    (tuition.render.tree:tree-child sub2 "Quux" "Quux" "Quux" "Quux" "Quux")
    (tuition.render.tree:tree-child tree "Bar" "Foo" sub1 sub2 "Baz")
    (is (golden-equal "tree" "TestTreeAddTwoSubTreesWithoutName"
                      (tuition.render.tree:tree-render tree)))))

;;; --- indenter styling + indent margin (#446) ---

(test tree-indenter-style-colors-guides
  "INDENTER-STYLE colors the continuation guides (the │ characters)."
  (let ((tree (tuition.render.tree:make-tree
               :root "root"
               :indenter-style (make-style :foreground (parse-hex-color "#FF0000")))))
    ;; First child is multiline and not last, so its continuation line draws a
    ;; styled "│" guide.
    (tuition.render.tree:tree-child tree (format nil "a~%b") "c")
    (let ((view (tuition.render.tree:tree-render tree)))
      (is (search (format nil "~C[38;2;255;0;0m" #\Escape) view))
      (is (search "│" view)))))

(test tree-no-indenter-style-is-plain
  "Without INDENTER-STYLE the guides are plain (no escape sequences)."
  (let ((tree (tuition.render.tree:make-tree :root "root")))
    (tuition.render.tree:tree-child tree (format nil "a~%b") "c")
    (is (not (find #\Escape (tuition.render.tree:tree-render tree))))))

(test tree-indent-applies-left-margin
  "INDENT adds a left margin to every rendered line."
  (let ((tree (tuition.render.tree:make-tree :root "root" :indent 2)))
    (tuition.render.tree:tree-child tree "a")
    (let ((view (tuition.render.tree:tree-render tree)))
      ;; Every line begins with two spaces.
      (is (search "  root" view))
      (is (search "  └── a" view)))))

(test tree-indent-zero-is-unchanged
  "INDENT 0 produces no left margin."
  (let ((tree (tuition.render.tree:make-tree :root "root")))
    (tuition.render.tree:tree-child tree "a")
    (let ((view (tuition.render.tree:tree-render tree)))
      (is (search "root" view))
      (is (not (search "  root" view))))))
