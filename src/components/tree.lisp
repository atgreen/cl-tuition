;;; components/tree.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
;;;
;;;; Interactive tree component (ports the bubbles tree bubble, #893).
;;;;
;;;; Unlike TUITION.RENDER.TREE, which renders a static tree to a string,
;;;; this component holds a tree of TREE-NODE objects with open/closed and
;;;; hidden state, a selection cursor, and viewport scrolling, and responds
;;;; to key messages.

(defpackage #:tuition.components.tree
  (:use #:cl)
  (:nicknames #:tui.tree)
  (:export
   ;; Nodes
   #:tree-node
   #:make-node
   #:node-value
   #:node-children
   #:node-add-child
   #:node-open-p
   #:node-open
   #:node-close
   #:node-toggle
   #:node-hidden-p
   #:node-set-hidden
   #:node-size

   ;; Model
   #:tree
   #:make-tree
   #:tree-root
   #:tree-width
   #:tree-height
   #:tree-yoffset
   #:tree-scroll-off
   #:tree-open-character
   #:tree-closed-character
   #:tree-cursor-character
   #:tree-cursor-style
   #:tree-selected-style
   #:tree-node-style
   #:tree-root-style
   #:tree-parent-style
   #:tree-enumerator-style

   ;; Operations
   #:tree-init
   #:tree-update
   #:tree-view
   #:tree-down
   #:tree-up
   #:tree-page-down
   #:tree-page-up
   #:tree-half-page-down
   #:tree-half-page-up
   #:tree-go-to-top
   #:tree-go-to-bottom
   #:tree-set-yoffset
   #:tree-toggle-current-node
   #:tree-open-current-node
   #:tree-close-current-node
   #:tree-selected-node
   #:tree-node-at
   #:tree-visible-nodes
   #:tree-viewport-yoffset
   #:tree-set-viewport-yoffset))

(in-package #:tuition.components.tree)

;;; Nodes

(defclass tree-node ()
  ((value :initarg :value :initform "" :accessor node-value
          :documentation "The node's display value (a string; may be multi-line)")
   (children :initarg :children :initform nil :accessor node-children
             :documentation "List of child TREE-NODEs")
   (open-p :initarg :open-p :initform t :accessor node-open-p
           :documentation "Whether the node's children are shown")
   (hidden-p :initarg :hidden-p :initform nil :accessor node-hidden-p
             :documentation "Whether the node (and its subtree) is hidden"))
  (:documentation "A node in an interactive tree."))

(defun make-node (value &rest children)
  "Create a tree node with VALUE and CHILDREN.  Children may be TREE-NODEs
or values; a non-node value becomes a leaf node."
  (let ((node (make-instance 'tree-node :value value)))
    (apply #'node-add-child node children)
    node))

(defun node-add-child (node &rest children)
  "Append CHILDREN to NODE, coercing non-node values to leaf nodes."
  (setf (node-children node)
        (append (node-children node)
                (mapcar (lambda (child)
                          (if (typep child 'tree-node)
                              child
                              (make-instance 'tree-node
                                             :value (if (stringp child)
                                                        child
                                                        (princ-to-string child)))))
                        children)))
  node)

(defun node-open (node)
  "Open the node so its children are shown."
  (setf (node-open-p node) t)
  node)

(defun node-close (node)
  "Close the node, hiding its children."
  (setf (node-open-p node) nil)
  node)

(defun node-toggle (node)
  "Toggle the node's open/closed state."
  (setf (node-open-p node) (not (node-open-p node)))
  node)

(defun node-set-hidden (node hidden)
  "Hide or show NODE (and with it, its whole subtree)."
  (setf (node-hidden-p node) hidden)
  node)

(defun %visible-children (node)
  (remove-if #'node-hidden-p (node-children node)))

(defun node-size (node)
  "Number of visible nodes in NODE's subtree.  A closed node counts as 1."
  (cond ((node-hidden-p node) 0)
        ((node-open-p node)
         (1+ (reduce #'+ (%visible-children node)
                     :key #'node-size :initial-value 0)))
        (t 1)))

;;; Model

(defclass tree ()
  ((root :initarg :root :initform nil :accessor tree-root
         :documentation "The root TREE-NODE")
   (width :initarg :width :initform 0 :accessor tree-width
          :documentation "Component width (advisory; lines are not wrapped)")
   (height :initarg :height :initform 10 :accessor tree-height
           :documentation "Viewport height in lines (0 = show everything)")
   (yoffset :initform 0 :accessor tree-yoffset
            :documentation "Index of the selected node among the visible nodes")
   (viewport-yoffset :initform 0 :accessor tree-viewport-yoffset
                     :documentation "Index of the first visible rendered line")
   (scroll-off :initarg :scroll-off :initform 5 :accessor tree-scroll-off
               :documentation "Minimal number of lines kept visible above and
below the selected node when scrolling")
   (open-character :initarg :open-character :initform "▼"
                   :accessor tree-open-character
                   :documentation "Indicator for an open node")
   (closed-character :initarg :closed-character :initform "▶"
                     :accessor tree-closed-character
                     :documentation "Indicator for a closed node")
   (cursor-character :initarg :cursor-character :initform "→"
                     :accessor tree-cursor-character
                     :documentation "Cursor marking the selected node (\"\" hides it)")
   (cursor-style :initarg :cursor-style :initform nil :accessor tree-cursor-style
                 :documentation "Style for the cursor character")
   (selected-style :initarg :selected-style
                   :initform (tuition:make-style :bold t)
                   :accessor tree-selected-style
                   :documentation "Style for the selected node's value")
   (node-style :initarg :node-style :initform nil :accessor tree-node-style
               :documentation "Style for leaf node values")
   (root-style :initarg :root-style :initform nil :accessor tree-root-style
               :documentation "Style for the root node's value")
   (parent-style :initarg :parent-style :initform nil :accessor tree-parent-style
                 :documentation "Style for non-root nodes that have children")
   (enumerator-style :initarg :enumerator-style :initform nil
                     :accessor tree-enumerator-style
                     :documentation "Style for branch and indent guides"))
  (:documentation "An interactive, navigable tree component."))

(defun make-tree (&key root (width 0) (height 10) (scroll-off 5)
                    (open-character "▼") (closed-character "▶")
                    (cursor-character "→")
                    cursor-style (selected-style (tuition:make-style :bold t))
                    node-style root-style parent-style enumerator-style)
  "Create a new interactive tree with ROOT (a TREE-NODE, see MAKE-NODE)."
  (make-instance 'tree
                 :root root :width width :height height :scroll-off scroll-off
                 :open-character open-character
                 :closed-character closed-character
                 :cursor-character cursor-character
                 :cursor-style cursor-style
                 :selected-style selected-style
                 :node-style node-style
                 :root-style root-style
                 :parent-style parent-style
                 :enumerator-style enumerator-style))

;;; Selection and lookup

(defun tree-visible-nodes (tree)
  "The visible nodes in render order: hidden nodes and the children of
closed nodes are excluded."
  (let ((root (tree-root tree))
        (acc '()))
    (when (and root (not (node-hidden-p root)))
      (labels ((walk (node)
                 (push node acc)
                 (when (node-open-p node)
                   (dolist (child (%visible-children node))
                     (walk child)))))
        (walk root)))
    (nreverse acc)))

(defun tree-node-at (tree yoffset)
  "The visible node at YOFFSET, or NIL when out of range."
  (nth yoffset (tree-visible-nodes tree)))

(defun tree-selected-node (tree)
  "The currently selected node, or NIL for an empty tree."
  (tree-node-at tree (tree-yoffset tree)))

;;; Layout: flatten the visible tree into rendered lines.

(defun %styled (style text)
  (if style (tuition:render-styled style text) text))

(defun %node-value-lines (tree node selected-p root-p)
  "NODE's rendered value lines: indicator plus styled value."
  (let* ((style (cond (selected-p (tree-selected-style tree))
                      (root-p (tree-root-style tree))
                      ((node-children node) (tree-parent-style tree))
                      (t (tree-node-style tree))))
         (indicator (when (node-children node)
                      (concatenate 'string
                                   (if (node-open-p node)
                                       (tree-open-character tree)
                                       (tree-closed-character tree))
                                   " ")))
         (value-lines (tuition:split-string-by-newline (node-value node))))
    (when (null value-lines) (setf value-lines (list "")))
    (loop for line in value-lines
          for first = t then nil
          collect (if first
                      (concatenate 'string (or indicator "") (%styled style line))
                      (%styled style line)))))

(defun %tree-layout (tree)
  "Flatten the visible tree.  Returns (VALUES LINES OWNER-STARTS) where
LINES is a vector of rendered line strings (without the cursor column) and
OWNER-STARTS is a vector of the same length holding T on each line that
begins a node."
  (let ((lines '())
        (starts '())
        (estyle (tree-enumerator-style tree))
        (selected (tree-selected-node tree))
        (root (tree-root tree)))
    (labels ((emit (text start-p)
               (push text lines)
               (push start-p starts))
             (walk-children (node prefix)
               (let* ((children (%visible-children node))
                      (n (length children)))
                 (loop for child in children
                       for i from 0
                       for last-p = (= i (1- n))
                       for branch = (%styled estyle (if last-p "└── " "├── "))
                       for cont = (%styled estyle (if last-p "    " "│   "))
                       do (let ((value-lines
                                  (%node-value-lines tree child
                                                     (eq child selected) nil)))
                            (emit (concatenate 'string prefix branch
                                               (first value-lines))
                                  t)
                            (dolist (line (rest value-lines))
                              (emit (concatenate 'string prefix cont line) nil))
                            (when (node-open-p child)
                              (walk-children child
                                             (concatenate 'string prefix cont))))))))
      (when (and root (not (node-hidden-p root)))
        (let ((value-lines (%node-value-lines tree root (eq root selected) t)))
          (emit (first value-lines) t)
          (dolist (line (rest value-lines))
            (emit line nil)))
        (when (node-open-p root)
          (walk-children root ""))))
    (values (coerce (nreverse lines) 'vector)
            (coerce (nreverse starts) 'vector))))

(defun %selected-line-offset (tree)
  "The rendered line index of the selected node's first line."
  (multiple-value-bind (lines starts) (%tree-layout tree)
    (declare (ignore lines))
    (let ((target (tree-yoffset tree))
          (seen -1))
      (loop for i from 0 below (length starts)
            do (when (aref starts i)
                 (incf seen)
                 (when (= seen target)
                   (return-from %selected-line-offset i)))))
    0))

;;; Movement and scrolling

(defun %tree-move (tree movement)
  "Move the selection by MOVEMENT visible nodes, then scroll to keep the
selected node in view with the configured scroll-off margin."
  (let ((n (length (tree-visible-nodes tree))))
    (when (plusp n)
      (setf (tree-yoffset tree)
            (max 0 (min (1- n) (+ (tree-yoffset tree) movement))))
      ;; On the initial render (no movement, at the top), show the root.
      (unless (and (zerop (tree-yoffset tree)) (zerop movement))
        (multiple-value-bind (lines starts) (%tree-layout tree)
          (declare (ignore starts))
          (let* ((total (length lines))
                 (height (if (plusp (tree-height tree))
                             (tree-height tree)
                             total))
                 (line-offset (%selected-line-offset tree))
                 (scroll-off (min (tree-scroll-off tree) (floor height 2)))
                 (min-top (max 0 (- line-offset scroll-off)))
                 (min-bottom (min (1- total) (+ line-offset scroll-off)))
                 (top (tree-viewport-yoffset tree)))
            (cond
              ((> top min-top)
               (setf (tree-viewport-yoffset tree) min-top))
              ((< (+ top height) (1+ min-bottom))
               (setf (tree-viewport-yoffset tree)
                     (max 0 (- (1+ min-bottom) height))))))))))
  tree)

(defun tree-down (tree)
  "Move the selection down by one node."
  (%tree-move tree 1))

(defun tree-up (tree)
  "Move the selection up by one node."
  (%tree-move tree -1))

(defun tree-page-down (tree)
  "Move the selection down by one viewport height."
  (%tree-move tree (max 1 (tree-height tree))))

(defun tree-page-up (tree)
  "Move the selection up by one viewport height."
  (%tree-move tree (- (max 1 (tree-height tree)))))

(defun tree-half-page-down (tree)
  "Move the selection down by half a viewport height."
  (%tree-move tree (max 1 (floor (tree-height tree) 2))))

(defun tree-half-page-up (tree)
  "Move the selection up by half a viewport height."
  (%tree-move tree (- (max 1 (floor (tree-height tree) 2)))))

(defun tree-go-to-top (tree)
  "Move the selection to the root."
  (%tree-move tree (- (tree-yoffset tree))))

(defun tree-go-to-bottom (tree)
  "Move the selection to the last visible node."
  (%tree-move tree (length (tree-visible-nodes tree))))

(defun tree-set-yoffset (tree yoffset)
  "Select the visible node at YOFFSET and scroll to it."
  (%tree-move tree (- yoffset (tree-yoffset tree))))

;;; Open/close

(defun %tree-set-open (tree node open)
  (setf (node-open-p node) open)
  ;; Closing a subtree can shrink the visible list past the selection.
  (let ((n (length (tree-visible-nodes tree))))
    (when (plusp n)
      (setf (tree-yoffset tree) (min (tree-yoffset tree) (1- n)))))
  (%tree-move tree 0)
  tree)

(defun tree-toggle-current-node (tree)
  "Toggle the selected node's open/closed state."
  (let ((node (tree-selected-node tree)))
    (when node (%tree-set-open tree node (not (node-open-p node)))))
  tree)

(defun tree-open-current-node (tree)
  "Open the selected node."
  (let ((node (tree-selected-node tree)))
    (when node (%tree-set-open tree node t)))
  tree)

(defun tree-close-current-node (tree)
  "Close the selected node."
  (let ((node (tree-selected-node tree)))
    (when node (%tree-set-open tree node nil)))
  tree)

(defun tree-set-viewport-yoffset (tree yoffset)
  "Scroll the viewport so YOFFSET is the first visible rendered line."
  (setf (tree-viewport-yoffset tree) (max 0 yoffset))
  tree)

;;; TEA protocol

(defun tree-init (tree)
  "Initialize the tree.  Returns NIL (no command)."
  (declare (ignore tree))
  nil)

(defun tree-update (tree msg)
  "Update the tree with a message.  Returns (values new-tree cmd).
Default keys follow the bubbles tree: ↓/j/Ctrl+n and ↑/k/Ctrl+p move,
PgDn/Space/f and PgUp/b page, d/Ctrl+d and u/Ctrl+u half-page, g/Home and
G/End jump, Enter toggles, →/l opens, ←/h closes."
  (when (tuition:key-press-msg-p msg)
    (let ((key (tuition:key-event-code msg))
          (ctrl (tuition:mod-contains (tuition:key-event-mod msg)
                                      tuition:+mod-ctrl+)))
      (cond
        ((or (eq key :down)
             (and (characterp key) (char= key #\j))
             (and ctrl (characterp key) (char= key #\n)))
         (tree-down tree))
        ((or (eq key :up)
             (and (characterp key) (char= key #\k))
             (and ctrl (characterp key) (char= key #\p)))
         (tree-up tree))
        ((or (eq key :page-down)
             (and (characterp key) (char= key #\Space))
             (and (characterp key) (char= key #\f)))
         (tree-page-down tree))
        ((or (eq key :page-up)
             (and (characterp key) (char= key #\b)))
         (tree-page-up tree))
        ((and (characterp key) (or (char= key #\d) (and ctrl (char= key #\d))))
         (tree-half-page-down tree))
        ((and (characterp key) (or (char= key #\u) (and ctrl (char= key #\u))))
         (tree-half-page-up tree))
        ((or (eq key :home) (and (characterp key) (char= key #\g)))
         (tree-go-to-top tree))
        ((or (eq key :end) (and (characterp key) (char= key #\G)))
         (tree-go-to-bottom tree))
        ((eq key :enter)
         (tree-toggle-current-node tree))
        ((or (eq key :right) (and (characterp key) (char= key #\l)))
         (tree-open-current-node tree))
        ((or (eq key :left) (and (characterp key) (char= key #\h)))
         (tree-close-current-node tree)))))
  (values tree nil))

;;; Rendering

(defun tree-view (tree)
  "Render the visible window of the tree, with a cursor column marking the
selected node."
  (multiple-value-bind (lines starts) (%tree-layout tree)
    (let* ((total (length lines))
           (height (if (plusp (tree-height tree)) (tree-height tree) total))
           (top (max 0 (min (tree-viewport-yoffset tree)
                            (max 0 (- total height)))))
           (cursor (tree-cursor-character tree))
           (cursor-width (1+ (tuition:visible-length cursor)))
           (pad (make-string cursor-width :initial-element #\Space))
           (selected-line (%selected-line-offset tree)))
      (setf (tree-viewport-yoffset tree) top)
      (with-output-to-string (s)
        (loop for i from top below (min total (+ top height))
              do (unless (= i top) (write-char #\Newline s))
                 (if (and (plusp (length cursor))
                          (= i selected-line)
                          (aref starts i))
                     (format s "~A " (%styled (tree-cursor-style tree) cursor))
                     (write-string (if (plusp (length cursor)) pad "") s))
                 (write-string (aref lines i) s))))))
