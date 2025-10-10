# Tuition Examples

This directory contains runnable programs that demonstrate features of the Tuition TUI library.

## Running Examples

- In a Lisp REPL:
  ```lisp
  (asdf:load-system :tuition)
  (load "examples/showcase.lisp")
  (tuition-example-showcase:main)
  ```

- From the command line (SBCL):
  ```bash
  sbcl --eval "(asdf:load-system :tuition)" \
       --load examples/showcase.lisp \
       --eval "(tuition-example-showcase:main)"
  ```

Replace `showcase` with any example filename and package prefix.

## Available Examples

### Showcase

- `showcase.lisp` — A polished, animated overview of features. Press `q` to quit.

### Basics

- `simple.lisp` — Minimal countdown. Press `q` or `Ctrl+C` to quit.
- `counter.lisp` — Counter with arrow keys and `r` to reset; `q` quits.
- `borders.lisp` — Border styles (normal, rounded, ascii). `q` quits.
- `styled.lisp` — Colors, bold/italic, alignment, width. `q` quits.

### Formatting & Rendering

- `reflow.lisp` — Wrap, truncate, indent. Use `←/→` to adjust width; `q` quits.
- `markdown.lisp` — Markdown rendering with multiple themes; press `1–4` to switch, `q` quits.
- `table.lisp` — Simple table with different border styles. `q` quits.

### Lists & Input

- `list.lisp` — Scrollable selection. `↑/k` and `↓/j` navigate, `Enter` selects, `q` quits.
- `textinput.lisp` — Basic text input. Type to edit, arrows move cursor, `Enter/Esc` quit.

### Animation & Components

- `spinner.lisp` — Tick-based spinner animation. `q` or `Ctrl+C` quits.
- `spinner-component.lisp` — Spinner extracted into a reusable component. `q` or `Ctrl+C` quits.
- `progress.lisp` — Progress bar that advances over time; any key quits.
- `spring-animation.lisp` — Spring physics demo with smooth motion.

### Timing

- `stopwatch.lisp` — Stopwatch with start/stop (`s`) and reset (`r`); `q` quits.
- `timer.lisp` — Countdown timer with start/stop (`s`) and reset (`r`); `q` quits.

### Mouse & Zones

- `mouse.lisp` — Mouse tracking (position, buttons, modifiers). Create the program with `:mouse :all-motion`.
- `zones.lisp` — Clickable regions (“zones”) with selection; run with `:mouse :cell-motion`.

### Networking & System

- `http.lisp` — Async HTTP example using commands, with basic error handling.
- `window-size.lisp` — Query current terminal size; press any key.

## Example Structure

Each example follows the Elm Architecture pattern implemented by Tuition:

```lisp
(defclass my-model ()
  ((field :initform value :accessor field)))

(defmethod tui:init ((model my-model))
  ;; Return initial command or NIL
  nil)

(defmethod tui:update-message ((model my-model) (msg tui:key-msg))
  ;; Handle messages, return (values new-model command)
  (values model nil))

(defmethod tui:view ((model my-model))
  ;; Render model to a string
  (format nil "Hello, world!"))

(defun main ()
  (let ((program (tui:make-program (make-instance 'my-model))))
    (tui:run program)))
```

## Key Concepts

- Messages: Structs representing events (key presses, ticks, custom data)
- Commands: Functions that return messages (for async operations)
- Model: Your application state
- Update: Pure function: (model, message) → (new-model, command)
- View: Pure function: model → string
