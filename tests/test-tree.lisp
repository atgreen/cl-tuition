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
