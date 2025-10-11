;;; markdown.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Markdown rendering example - demonstrates glamour-style markdown rendering

(defpackage #:tuition-example-markdown
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-markdown)

(eval-when (:load-toplevel :execute)
  (asdf:load-system :tuition))

;;; Sample markdown content

(defparameter *sample-markdown* "# Tuition Markdown Renderer

Welcome to the **Tuition** markdown renderer, inspired by [Glamour](https://github.com/charmbracelet/glamour)!

## Features

This renderer supports:

- **Bold** and *italic* text
- `Inline code` with syntax highlighting
- Headers (H1, H2, H3)
- Unordered lists
- Ordered lists
- Block quotes
- Code blocks
- Links
- Horizontal rules

---

## Code Example

Here's a simple Common Lisp example:

```lisp
(defun hello-world ()
  \"Print a friendly greeting.\"
  (format t \"Hello, World!~%\"))

(hello-world)
```

## Lists

### Unordered List

- First item
- Second item
- Third item with **bold** text

### Ordered List

1. Step one
2. Step two
3. Step three

## Quotes

> Lisp is worth learning for the profound enlightenment experience
> you will have when you finally get it; that experience will make you
> a better programmer for the rest of your days.
>
> — Eric S. Raymond

## Styling

You can mix `code`, **bold**, *italic*, and [links](https://common-lisp.net) in the same paragraph!

---

*Press 'q' to quit, '1-4' to change styles*")

;;; Model

(defclass markdown-model ()
  ((content :initform *sample-markdown* :accessor model-content)
   (style :initform :dark :accessor model-style)
   (styles :initform '(:dark :light :pink :ascii) :reader model-styles)))

;;; Init

(defmethod tui:init ((model markdown-model))
  nil)

;;; Update

(defmethod tui:update ((model markdown-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Quit on 'q' or Ctrl+C
      ((or (and (characterp key) (char= key #\q))
           (and (characterp key) (char= key #\c) (tui:key-msg-ctrl msg)))
       (values model (tui:quit-cmd)))

      ;; Change style with number keys
      ((and (characterp key) (char= key #\1))
       (setf (model-style model) :dark)
       (values model nil))

      ((and (characterp key) (char= key #\2))
       (setf (model-style model) :light)
       (values model nil))

      ((and (characterp key) (char= key #\3))
       (setf (model-style model) :pink)
       (values model nil))

      ((and (characterp key) (char= key #\4))
       (setf (model-style model) :ascii)
       (values model nil))

      (t (values model nil)))))

(defmethod tui:update ((model markdown-model) msg)
  (declare (ignore msg))
  (values model nil))

;;; View

(defmethod tui:view ((model markdown-model))
  (let* ((style (model-style model))
         (rendered (tui:render-markdown (model-content model)
                                        :style style
                                        :width 70))
         (style-name (string-capitalize (symbol-name style)))
         (footer (format nil "~%~%Current style: ~A | Press 1-4 to change style, 'q' to quit"
                        style-name)))
    (concatenate 'string rendered footer)))

;;; Main

(defun main ()
  "Run the markdown rendering example."
  (let ((program (tui:make-program (make-instance 'markdown-model))))
    (tui:run program)))

(eval-when (:load-toplevel :execute)
  (main))
