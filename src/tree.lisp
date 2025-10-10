;;;; SPDX-License-Identifier: MIT
;;;; Tree rendering sub-package inspired by Lipgloss

(defpackage #:tuition.tree
  (:use #:cl)
  (:nicknames #:tui.tree)
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

(in-package #:tuition.tree)

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
  (setf (tree-children tree)
        (append (tree-children tree) children))
  tree)

;;; Built-in enumerators
(defun default-enumerator (depth last-child-p)
  "Default tree enumerator with ASCII box characters."
  (declare (ignore depth))
  (if last-child-p
      (values "└── " "    ")
      (values "├── " "│   ")))

(defun rounded-enumerator (depth last-child-p)
  "Rounded tree enumerator with curved characters."
  (declare (ignore depth))
  (if last-child-p
      (values "╰── " "    ")
      (values "╭── " "│   ")))

;;; Rendering
(defun tree-render (tree &optional (depth 0) (prefix ""))
  "Render a tree to a string."
  (let ((root-text (tree-root-text tree))
        (children (tree-children tree))
        (enum-func (tree-enumerator tree))
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
      (loop for child in children
            for idx from 0
            for last-child-p = (= idx (1- (length children)))
            do (multiple-value-bind (branch continuation)
                   (funcall enum-func depth last-child-p)
                 (let ((styled-branch (if enum-style
                                          (tuition:render-styled enum-style branch)
                                          branch)))
                   (cond
                     ;; Nested tree
                     ((typep child 'tui-tree)
                      (let* ((child-prefix (concatenate 'string prefix styled-branch))
                             (child-text (tree-render child (1+ depth) child-prefix))
                             (child-lines (tuition:split-string-by-newline child-text)))
                        ;; First line already has the branch
                        (push (car child-lines) result)
                        ;; Remaining lines need continuation
                        (dolist (line (cdr child-lines))
                          (push (format nil "~A~A~A"
                                       prefix
                                       (if last-child-p "    " continuation)
                                       (subseq line (+ (length prefix) (tuition:visible-length styled-branch))))
                                result))))

                     ;; String child
                     (t
                      (let ((item-text (if item-style
                                           (tuition:render-styled item-style (format nil "~A" child))
                                           (format nil "~A" child))))
                        (push (format nil "~A~A~A" prefix styled-branch item-text) result))))))))

    (format nil "~{~A~^~%~}" (nreverse result))))
