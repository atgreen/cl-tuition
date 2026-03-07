;;; tree.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Tree rendering utilities for formatted text output

(defpackage #:tuition.render.tree
  (:use #:cl)
  (:nicknames #:tui.render.tree #:tui.r.tree)
  (:documentation "Tree rendering utilities for hierarchical data structures.")
  (:export
   ;; Tree creation
   #:make-tree
   #:tree-root
   #:tree-child

   ;; Enumerators
   #:default-enumerator
   #:rounded-enumerator

   ;; Rendering
   #:tree-render))

(in-package #:tuition.render.tree)

;;; Tree structure
(defclass tui-tree ()
  ((root :initform nil :accessor tree-root-text)
   (children :initform nil :accessor tree-children)
   (enumerator :initform #'default-enumerator :accessor tree-enumerator)
   (enumerator-style :initform nil :accessor tree-enumerator-style)
   (root-style :initform nil :accessor tree-root-style)
   (item-style :initform nil :accessor tree-item-style)
   (indent :initform 0 :accessor tree-indent))
  (:documentation "A renderable tree structure."))

(defun make-tree (&key root)
  "Create a new tree with optional root text."
  (let ((tree (make-instance 'tui-tree)))
    (when root (setf (tree-root-text tree) root))
    tree))

(defun tree-root (tree root-text)
  "Set the root text of the tree."
  (setf (tree-root-text tree) root-text)
  tree)

(defun tree-child (tree &rest children)
  "Add children to the tree. Children can be strings or other trees."
  (alexandria:appendf (tree-children tree) children)
  tree)

;;; Built-in enumerators
(defun default-enumerator (depth last-child-p)
  "Default tree enumerator with ASCII box characters."
  (declare (ignore depth))
  (if last-child-p
      (values "└── " "    ")
      (values "├── " "│   ")))

(defun rounded-enumerator (depth last-child-p)
  "Rounded tree enumerator with curved last-child connector."
  (declare (ignore depth))
  (if last-child-p
      (values "╰── " "    ")
      (values "├── " "│   ")))

;;; Rendering helpers

(defun flatten-inline-children (children)
  "Flatten nameless subtrees into inline items for rendering.
   Returns a list where each element is either a string/named-tree or
   a child of a nameless subtree (at the current depth level)."
  (let ((flat nil))
    (dolist (child children)
      (if (and (typep child 'tui-tree) (null (tree-root-text child)))
          ;; Nameless subtree: inline its children
          (dolist (grandchild (tree-children child))
            (push grandchild flat))
          (push child flat)))
    (nreverse flat)))

(defun render-child-item (child depth prefix enum-func enum-style item-style
                          last-child-p result)
  "Render a single child item (string or named tree) with proper branch/continuation."
  (multiple-value-bind (branch continuation)
      (funcall enum-func depth last-child-p)
    (let ((styled-branch (if enum-style
                              (tuition:render-styled enum-style branch)
                              branch)))
      (cond
        ;; Named nested tree
        ((typep child 'tui-tree)
         (let* ((child-prefix (concatenate 'string prefix styled-branch))
                (child-text (tree-render child (1+ depth) child-prefix enum-func))
                (child-lines (tuition:split-string-by-newline child-text)))
           ;; First line already has the branch
           (push (first child-lines) result)
           ;; Remaining lines need continuation
           (dolist (line (rest child-lines))
             (push (format nil "~A~A~A"
                          prefix
                          (if last-child-p "    " continuation)
                          (serapeum:drop (+ (length prefix)
                                            (tuition:visible-length styled-branch))
                                         line))
                   result))))
        ;; String child (possibly multiline)
        (t
         (let* ((item-text (if item-style
                                (tuition:render-styled item-style (format nil "~A" child))
                                (format nil "~A" child)))
                (item-lines (tuition:split-string-by-newline item-text))
                ;; Pad lines to max width within this item
                (max-line-width (apply #'max (mapcar #'tuition:visible-length item-lines)))
                (padded-lines (mapcar (lambda (line)
                                        (let ((pad (- max-line-width (tuition:visible-length line))))
                                          (if (> pad 0)
                                              (concatenate 'string line (make-string pad :initial-element #\Space))
                                              line)))
                                      item-lines)))
           ;; First line with branch prefix
           (push (format nil "~A~A~A" prefix styled-branch (first padded-lines)) result)
           ;; Continuation lines with continuation prefix
           (dolist (line (rest padded-lines))
             (push (format nil "~A~A~A" prefix continuation line) result)))))))
  result)

(defun render-children (children depth prefix enum-func enum-style item-style result)
  "Render a list of tree children, handling nameless subtrees by inlining them."
  (let ((i 0)
        (n (length children)))
    (loop while (< i n)
          do (let ((child (nth i children)))
               (cond
                 ;; Nameless subtree: collect consecutive nameless subtrees and flatten
                 ((and (typep child 'tui-tree) (null (tree-root-text child)))
                  (let ((inline-children nil))
                    ;; Gather all consecutive nameless subtrees' children
                    (loop while (and (< i n)
                                    (let ((c (nth i children)))
                                      (and (typep c 'tui-tree)
                                           (null (tree-root-text c)))))
                          do (dolist (gc (tree-children (nth i children)))
                               (push gc inline-children))
                             (incf i))
                    (setf inline-children (nreverse inline-children))
                    ;; Determine if there are more children after this group
                    (let ((more-after (< i n)))
                      ;; Get the continuation prefix for the inline group
                      (multiple-value-bind (branch continuation)
                          (funcall enum-func depth (not more-after))
                        (declare (ignore branch))
                        (let ((inline-prefix (concatenate 'string prefix continuation))
                              (inline-n (length inline-children)))
                          ;; Render each inline child
                          (loop for ic in inline-children
                                for ic-idx from 0
                                for ic-last-p = (= ic-idx (1- inline-n))
                                do (setf result
                                         (render-child-item ic (1+ depth) inline-prefix
                                                            enum-func enum-style item-style
                                                            ic-last-p result))))))))
                 ;; Normal child (string or named tree)
                 (t
                  (let ((last-child-p (= i (1- n))))
                    (setf result
                          (render-child-item child depth prefix
                                            enum-func enum-style item-style
                                            last-child-p result)))
                  (incf i))))))
  result)

(defun tree-render (tree &optional (depth 0) (prefix "") parent-enum-func)
  "Render a tree to a string."
  (let ((root-text (tree-root-text tree))
        (children (tree-children tree))
        (enum-func (or parent-enum-func (tree-enumerator tree)))
        (enum-style (tree-enumerator-style tree))
        (root-style (tree-root-style tree))
        (item-style (tree-item-style tree))
        (result nil))

    ;; Render root
    (when root-text
      (let ((styled-root (if root-style
                             (tuition:render-styled root-style root-text)
                             root-text)))
        (push (format nil "~A~A" prefix styled-root) result)))

    ;; Render children
    (when children
      (setf result (render-children children depth prefix enum-func enum-style item-style result)))

    (format nil "~{~A~^~%~}" (nreverse result))))
