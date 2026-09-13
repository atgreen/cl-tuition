;;; test-tree-component.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
;;;
;;;; Tests for the interactive tree component (src/components/tree.lisp)

(in-package #:tuition-tests)

(def-suite tree-component-tests
  :description "Tests for the interactive tree component."
  :in tuition-tests)

(in-suite tree-component-tests)

;;; --- test helpers ---

(defun trc-make (&rest args)
  "Make the standard test tree used across these tests:
root > (leaf-a, branch > (x, y), leaf-b)."
  (apply #'tui.tree:make-tree
         :root (tui.tree:make-node "root"
                                   "leaf-a"
                                   (tui.tree:make-node "branch" "x" "y")
                                   "leaf-b")
         args))

(defun trc-key (tr code &optional mod)
  "Feed a key-press message to the tree.  Returns the tree."
  (tui.tree:tree-update tr (make-key-press-msg :code code :mod (or mod 0)))
  tr)

(defun trc-selected-value (tr)
  (tui.tree:node-value (tui.tree:tree-selected-node tr)))

;;; --- structure ---

(test tree-node-size-counts-visible
  "NODE-SIZE counts a closed subtree as one node."
  (let ((root (tui.tree:make-node "root"
                                  "leaf"
                                  (tui.tree:make-node "branch" "x" "y"))))
    (is (= 5 (tui.tree:node-size root)))
    (tui.tree:node-close (second (tui.tree:node-children root)))
    (is (= 3 (tui.tree:node-size root)))))

(test tree-visible-nodes-skips-closed-and-hidden
  "Children of closed nodes and hidden nodes are not visible."
  (let ((tr (trc-make)))
    (is (= 6 (length (tui.tree:tree-visible-nodes tr))))
    ;; Close "branch": x and y disappear.
    (tui.tree:node-close (second (tui.tree:node-children (tui.tree:tree-root tr))))
    (is (= 4 (length (tui.tree:tree-visible-nodes tr))))
    ;; Hide "leaf-a".
    (tui.tree:node-set-hidden
     (first (tui.tree:node-children (tui.tree:tree-root tr))) t)
    (is (= 3 (length (tui.tree:tree-visible-nodes tr))))))

(test tree-add-child-appends
  "NODE-ADD-CHILD appends after existing children, coercing values to leaves."
  (let ((node (tui.tree:make-node "root" "first")))
    (tui.tree:node-add-child node "second" 3)
    (is (equal '("first" "second" "3")
               (mapcar #'tui.tree:node-value (tui.tree:node-children node))))))

;;; --- navigation ---

(test tree-down-up-move-selection
  "Down and up move the selection through visible nodes in render order."
  (let ((tr (trc-make)))
    (is (string= "root" (trc-selected-value tr)))
    (tui.tree:tree-down tr)
    (is (string= "leaf-a" (trc-selected-value tr)))
    (tui.tree:tree-down tr)
    (is (string= "branch" (trc-selected-value tr)))
    (tui.tree:tree-down tr)
    (is (string= "x" (trc-selected-value tr)))
    (tui.tree:tree-up tr)
    (is (string= "branch" (trc-selected-value tr)))))

(test tree-selection-clamps-at-ends
  "The selection stops at the first and last visible node."
  (let ((tr (trc-make)))
    (tui.tree:tree-up tr)
    (is (= 0 (tui.tree:tree-yoffset tr)))
    (tui.tree:tree-go-to-bottom tr)
    (is (string= "leaf-b" (trc-selected-value tr)))
    (tui.tree:tree-down tr)
    (is (string= "leaf-b" (trc-selected-value tr)))))

(test tree-go-to-top-and-bottom
  "g/G jump to the root and the last visible node."
  (let ((tr (trc-make)))
    (tui.tree:tree-go-to-bottom tr)
    (is (string= "leaf-b" (trc-selected-value tr)))
    (tui.tree:tree-go-to-top tr)
    (is (string= "root" (trc-selected-value tr)))))

(test tree-key-navigation
  "Key messages drive navigation."
  (let ((tr (trc-make)))
    (trc-key tr :down)
    (is (string= "leaf-a" (trc-selected-value tr)))
    (trc-key tr #\j)
    (is (string= "branch" (trc-selected-value tr)))
    (trc-key tr #\k)
    (is (string= "leaf-a" (trc-selected-value tr)))
    (trc-key tr #\n +mod-ctrl+)
    (is (string= "branch" (trc-selected-value tr)))
    (trc-key tr #\G)
    (is (string= "leaf-b" (trc-selected-value tr)))
    (trc-key tr #\g)
    (is (string= "root" (trc-selected-value tr)))))

;;; --- open / close ---

(test tree-close-hides-children
  "Closing the selected node removes its children from the visible list."
  (let ((tr (trc-make)))
    (tui.tree:tree-set-yoffset tr 2)          ; "branch"
    (tui.tree:tree-close-current-node tr)
    (is (= 4 (length (tui.tree:tree-visible-nodes tr))))
    (tui.tree:tree-down tr)
    (is (string= "leaf-b" (trc-selected-value tr)))))

(test tree-toggle-via-enter
  "Enter toggles the selected node's open state."
  (let ((tr (trc-make)))
    (tui.tree:tree-set-yoffset tr 2)          ; "branch"
    (trc-key tr :enter)
    (is (not (tui.tree:node-open-p (tui.tree:tree-selected-node tr))))
    (trc-key tr :enter)
    (is (tui.tree:node-open-p (tui.tree:tree-selected-node tr)))))

(test tree-close-clamps-selection
  "Closing a subtree that contained the last selection keeps it valid."
  (let ((tr (trc-make)))
    (tui.tree:tree-go-to-bottom tr)            ; "leaf-b" (index 5)
    (tui.tree:tree-set-yoffset tr 2)           ; "branch"
    (tui.tree:tree-close-current-node tr)
    (is (< (tui.tree:tree-yoffset tr)
           (length (tui.tree:tree-visible-nodes tr))))))

;;; --- rendering ---

(test tree-view-renders-structure
  "The view shows branch guides, indicators, and the cursor."
  (let* ((tr (trc-make :height 0 :selected-style nil))
         (view (tui.tree:tree-view tr))
         (lines (tuition:split-string-by-newline view)))
    (is (= 6 (length lines)))
    ;; Cursor on the root line, which carries the open indicator.
    (is (search "→ ▼ root" (first lines)))
    ;; Non-selected lines are padded past the cursor column.
    (is (search "├── leaf-a" (second lines)))
    (is (search "├── ▼ branch" (third lines)))
    (is (search "│   ├── x" (fourth lines)))
    (is (search "│   └── y" (fifth lines)))
    (is (search "└── leaf-b" (sixth lines)))))

(test tree-view-closed-indicator
  "A closed branch renders the closed indicator and hides its children."
  (let ((tr (trc-make :height 0 :selected-style nil)))
    (tui.tree:node-close
     (second (tui.tree:node-children (tui.tree:tree-root tr))))
    (let ((view (tui.tree:tree-view tr)))
      (is (search "▶ branch" view))
      (is (not (search "├── x" view))))))

(test tree-view-cursor-follows-selection
  "The cursor moves with the selection."
  (let* ((tr (trc-make :height 0 :selected-style nil)))
    (tui.tree:tree-down tr)
    (let ((lines (tuition:split-string-by-newline (tui.tree:tree-view tr))))
      (is (search "→ ├── leaf-a" (second lines)))
      (is (not (search "→" (first lines)))))))

(test tree-view-windows-to-height
  "A height smaller than the content windows the rendered lines."
  (let* ((tr (trc-make :height 3 :scroll-off 0))
         (view (tui.tree:tree-view tr))
         (lines (tuition:split-string-by-newline view)))
    (is (= 3 (length lines)))
    ;; Moving to the bottom scrolls the window down.
    (tui.tree:tree-go-to-bottom tr)
    (let ((bottom-lines (tuition:split-string-by-newline (tui.tree:tree-view tr))))
      (is (= 3 (length bottom-lines)))
      (is (search "leaf-b" (car (last bottom-lines)))))))

(test tree-multi-line-values
  "A node value spanning lines renders continuation lines under the value."
  (let* ((tr (tui.tree:make-tree
              :root (tui.tree:make-node "root"
                                        (format nil "two~%lines")
                                        "tail")
              :height 0 :selected-style nil))
         (lines (tuition:split-string-by-newline (tui.tree:tree-view tr))))
    (is (= 4 (length lines)))
    (is (search "├── two" (second lines)))
    (is (search "lines" (third lines)))
    (is (search "└── tail" (fourth lines)))))

(test tree-empty-is-safe
  "An empty tree navigates and renders without error."
  (let ((tr (tui.tree:make-tree :root nil)))
    (tui.tree:tree-down tr)
    (tui.tree:tree-toggle-current-node tr)
    (is (string= "" (tui.tree:tree-view tr)))
    (is (null (tui.tree:tree-selected-node tr)))))
