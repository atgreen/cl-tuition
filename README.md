# Tuition

Tuition is a Common Lisp library for building rich, responsive Terminal User Interfaces (TUIs). It blends the simplicity of The Elm Architecture (TEA) with the power of CLOS so you can model state clearly, react to events via generic methods, and render your UI as pure strings.

- Model: a CLOS object representing your application state
- Messages: CLOS classes describing events (keys, mouse, timers, custom)
- Update: generic methods that transform the model in response to messages
- View: a pure function that renders your model to a string

Tuition handles terminal concerns for you (raw mode, alternate screen, input decoding, cursor control) so you can focus on application logic.

## Features

- TEA-style architecture with CLOS: message-specialized `tui:update-message`
- Concurrent commands for non-blocking I/O and timers
- Keyboard and mouse input decoding (with modifiers and motion)
- Terminal control (raw mode, alternate screen, cursor, clear)
- Styling utilities (bold/italic/underline/colors, adaptive colors)
- Layout helpers (horizontal/vertical joins, placement and alignment)
- Borders (normal, rounded, thick, double, block, ASCII, markdown)
- Reflow helpers (wrapping, truncation, ellipsizing, indentation)
- Built-in components: spinner, progress bar, list, table, text input
- Zones for advanced mouse interactions (define and query named regions)

## Installation

Using ocicl:

```bash
cd /path/to/tuition
ocicl install .
```

Or via ASDF:

```lisp
(asdf:load-system :tuition)
```

## Version Information

Tuition uses git-aware version strings. Access the current version:

```lisp
tui:+version+  ; => "0.1.0" or "0.1.0-g1a2b3c4" or "v1.0.0+dirty"
```

## Quick Start

### Hello World

```lisp
(defpackage #:hello-world
  (:use #:cl #:tuition))

(in-package #:hello-world)

(defclass hello-model () ())

(defmethod tui:init ((model hello-model))
  nil) ; no initial command

(defmethod tui:update-message ((model hello-model) (msg tui:key-msg))
  (declare (ignore msg))
  (values model (tui:quit-cmd)))

(defmethod tui:view ((model hello-model))
  (tui:bold "Hello, World! Press any key to exit."))

(defun main ()
  (tui:run (tui:make-program (make-instance 'hello-model))))
```

### Interactive Counter (CLOS-first style)

```lisp
(defpackage #:counter-demo
  (:use #:cl #:tuition))

(in-package #:counter-demo)

(defclass counter-model ()
  ((count :initform 0 :accessor count)))

(defmethod tui:init ((model counter-model)) nil)

(defmethod tui:update-message ((model counter-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((and (characterp key) (char= key #\q))
       (values model (tui:quit-cmd)))
      ((and (characterp key) (char= key #\+))
       (incf (count model))
       (values model nil))
      ((and (characterp key) (char= key #\-))
       (decf (count model))
       (values model nil))
      (t (values model nil)))))

(defmethod tui:view ((model counter-model))
  (format nil "Count: ~D~%~%Press + to increment, - to decrement, q to quit"
          (count model)))

(defun main ()
  (tui:run (tui:make-program (make-instance 'counter-model))))
```

## Core Concepts

### Model

Your application state lives in a CLOS object:

```lisp
(defclass my-app ()
  ((username :initarg :username :accessor username)
   (messages :initform '() :accessor messages)
   (input :initform "" :accessor input)))
```

### Messages and Update

Events are message objects. Prefer specializing `tui:update-message` by message class for clarity and extensibility.

```lisp
;; Built-in key message
(defmethod tui:update-message ((model my-app) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((and (characterp key) (char= key #\q)) (values model (tui:quit-cmd)))
      (t (values model nil)))))

;; Custom message
(tui:defmessage data-loaded
  ((items :initarg :items :accessor items)))

(defmethod tui:update-message ((model my-app) (msg data-loaded))
  (setf (messages model) (items msg))
  (values model nil))

;; Optional fallback when no method matches
(defmethod tui:update ((model my-app) msg)
  (declare (ignore msg))
  (values model nil))
```

### View

The view renders your model to a string. Tuition only needs a string; compose helpers however you like.

```lisp
(defmethod tui:view ((model my-app))
  (let ((header (tui:bold "My Application"))
        (content (format nil "Messages: ~{~A~^, ~}" (messages model)))
        (footer (format nil "User: ~A" (username model))))
    (tui:join-vertical tui:+left+ header content footer)))
```

### Commands

Commands are functions that return messages asynchronously.

```lisp
;; Create a delayed message
(defun tick (seconds msg)
  (lambda ()
    (sleep seconds)
    msg))

;; Built-in helpers
(tui:quit-cmd)
(tui:batch cmd1 cmd2)
(tui:cmd-sequence cmd-a cmd-b cmd-c)
```

### Program Options

```lisp
(tui:make-program model
  :alt-screen t            ; Use alternate screen buffer
  :mouse :cell-motion)     ; :cell-motion | :all-motion | NIL
```

## Terminal Lifecycle

Use `tui:with-raw-terminal` when you want terminal control outside the main program loop. It ensures proper cleanup and offers restarts to recover from setup issues.

```lisp
(tui:with-raw-terminal (:alt-screen t :mouse :cell-motion)
  (format t "Hello in raw mode!~%")
  (finish-output))
```

Restarts during setup:
- `USE-NO-RAW` — continue without entering raw mode
- `RETRY` — retry entering raw mode
- `ABORT` — abort setup and return

## Styling and Layout

### Text Styling

```lisp
(tui:bold "Important text")
(tui:italic "Emphasized")
(tui:underline "Underlined")
(tui:colored "Red text" :red)

;; Compose styles with a style object
(tui:render-styled
  (tui:make-style :foreground tui:*fg-bright-blue*
                  :background tui:*bg-black*
                  :bold t :underline t)
  "Styled text")
```

### Layout and Placement

```lisp
(tui:join-horizontal tui:+top+ "A" "B" "C")
(tui:join-vertical tui:+left+ "Title" "Body" "Footer")
(tui:place 40 10 tui:+center+ tui:+middle+ "Centered block")
```

### Borders

```lisp
(tui:render-border (tui:make-border :style tui:*border-rounded*) "Panel")
```

## Reflow Utilities

```lisp
(tui:wrap-text "A long paragraph to wrap neatly." 40 :indent 2)
(tui:truncate-text (tui:bold "Styled text") 20 :ellipsis "...")
(tui:indent-lines "Line A\nLine B" 4)
```

## Input and Mouse

```lisp
;; Key message helpers
(tui:key-msg-p msg)
(tui:key-msg-key msg)   ; Character or keyword (:up, :down, :enter, ...)
(tui:key-msg-ctrl msg)
(tui:key-msg-alt msg)

;; Mouse message helpers
(tui:mouse-msg-p msg)
(tui:mouse-msg-x msg)
(tui:mouse-msg-y msg)
(tui:mouse-msg-button msg)  ; :left, :right, :middle
(tui:mouse-msg-action msg)  ; :press, :release, :motion
```

## Components

Tuition includes a few reusable building blocks. Each component exposes a small protocol of functions or methods for init, update, and view.

```lisp
;; Spinner
(defparameter *sp* (tuition.components.spinner:make-spinner))
(multiple-value-bind (sp cmd) (tuition.components.spinner:spinner-update *sp* (tuition.components.spinner:make-spinner-tick-msg :id (tuition.components.spinner:spinner-id *sp*)))
  (declare (ignore cmd))
  (tuition.components.spinner:spinner-view sp))

;; Progress
(tuition.components.progress:progress-view
  (tuition.components.progress:make-progress :percent 0.42))

;; List
(let ((lst (tuition.components.list:make-list-view :items '("A" "B" "C"))))
  (tuition.components.list:list-view-render lst))

;; Table
(tuition.components.table:table-render
  (tuition.components.table:make-table
    :headers '("ID" "Name")
    :rows '((1 "Alice") (2 "Bob"))))

;; Text input
(tuition.components.textinput:textinput-view
  (tuition.components.textinput:make-textinput :placeholder "Type here"))
```

## Zones (Mouse Areas)

Zones let you attribute portions of the rendered screen to symbolic identifiers and query hover/clicks reliably.

- Create a `zone-manager`
- Mark regions while rendering
- Query with pointer coordinates to identify the active zone

See `zone.lisp` for the API and the `examples/zones*` demos for usage patterns.

## Examples

The `examples/` directory contains runnable demos. Highlights:

- `simple.lisp` — countdown timer using a custom tick message
- `counter.lisp` — interactive counter
- `spinner.lisp` — animated spinner
- `progress.lisp` — progress bar
- `textinput.lisp` — text input
- `list.lisp` — scrollable selection list
- `table.lisp` — table rendering
- `borders.lisp`, `styled.lisp` — styling and borders
- `reflow.lisp` — wrap, truncate, indent demo
- `mouse.lisp` — mouse input handling
- `window-size.lisp` — terminal geometry
- `http.lisp` — async command example

Run an example:

```bash
sbcl --eval "(asdf:load-system :tuition)" \
     --load examples/simple.lisp \
     --eval "(tuition-example-simple:main)"
```

## Error Handling

Tuition uses conditions for internal errors. You can customize reporting by rebinding `tui:*error-handler*`.

```lisp
(let ((tui:*error-handler*
        (lambda (where c)
          (format *error-output* "[~A] ~A~%" where c))))
  (tui:run (tui:make-program (make-instance 'hello-world::hello-model))))
```

## Dependencies

- `bordeaux-threads` — cross‑platform threading
- `trivial-channels` — thread‑safe message passing

## License

MIT License — see `LICENSE`.

## Acknowledgments

Inspired by The Elm Architecture and the Charmbracelet ecosystem (Bubble Tea, Lip Gloss, Bubbles).
