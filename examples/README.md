# Tuition Examples

This directory contains runnable programs that demonstrate features of the Tuition TUI library.

## Running Examples

Examples run automatically when loaded:

- From the command line (SBCL):
  ```bash
  sbcl --load examples/showcase.lisp
  ```

- In a Lisp REPL:
  ```lisp
  (load "examples/showcase.lisp")
  ```

Replace `showcase` with any example filename. Each example loads the Tuition system and runs automatically via `eval-when`.

## Available Examples

### Showcase

- `showcase.lisp` — Polished, animated overview of Tuition features. Press `q` to quit.
- `showcase-interactive.lisp` — Full interactive demo with clickable tabs, lists, and dialog buttons. Uses mouse zones and `mouse-release-event`.
- `layout.lisp` — Comprehensive layout demonstration (ported from Lip Gloss example) showing tabs, dialogs, lists, color grids, and status bars. No interaction, just displays.

### Basics

- `simple.lisp` — Minimal countdown timer. Press `q` or `Ctrl+C` to quit.
- `counter.lisp` — Interactive counter with `+`/`-` or arrow keys; `r` to reset, `q` quits.
- `borders.lisp` — Border styles (normal, rounded, thick, double, ASCII). `q` quits.
- `styled.lisp` — Colors, bold/italic/underline, alignment, width. `q` quits.
- `keybindings.lisp` — Keybinding system with auto-generated help text. Navigate list with arrow keys, `?`/`h` for help, `Enter` to select, `q` quits.

### Formatting & Rendering

- `reflow.lisp` — Text wrapping, truncation, and indentation. Use `←/→` to adjust width; `q` quits.
- `markdown.lisp` — Markdown rendering with multiple themes; press `1–4` to switch themes, `q` quits.
- `table.lisp` — Table rendering with different border styles. `q` quits.

### Lists & Input

- `list.lisp` — Scrollable selection list. `↑/k` and `↓/j` navigate, `Enter` selects, `q` quits.
- `textinput.lisp` — Basic text input field. Type to edit, arrows move cursor, `Enter`/`Esc` quit.
- `textinput-component.lisp` — Multi-field registration form with full-featured textinput component (password masking, validation, Tab/Shift+Tab navigation, undo/redo, kill/yank). `q` or `Ctrl+C` quits.
- `chat.lisp` — Chat interface demo using textarea and viewport components. Type messages and press `Enter` to send, `Esc` or `Ctrl+C` quits.
- `datepicker.lisp` — Interactive calendar date picker. Arrow keys navigate days/weeks, `[`/`]` for months, `Enter` selects, `q` quits.

### Animation & Components

- `spinner.lisp` — Tick-based spinner animation. `q` or `Ctrl+C` quits.
- `spinner-component.lisp` — Spinner extracted into a reusable component. `q` or `Ctrl+C` quits.
- `progress.lisp` — Progress bar that advances over time; any key quits.
- `spring-animation.lisp` — Spring physics demo with smooth motion; press `SPACE` to change target, `q` to quit.
- `eyes.lisp` — Animated blinking eyes (ported from BubbleTea). Eyes blink randomly with smooth easing animation. `Esc` or `Ctrl+C` quits.

### Timing

- `stopwatch.lisp` — Stopwatch with start/stop (`s`) and reset (`r`); `q` quits.
- `timer.lisp` — Countdown timer with start/stop (`s`) and reset (`r`); `q` quits.

### Mouse & Zones

- `mouse.lisp` — Mouse tracking demonstrating the hierarchical event system (press, release, drag, move, scroll). Uses `:mouse :all-motion`.
- `zones.lisp` — Clickable regions ("zones") with button selection using `mouse-press-event`. Uses `:mouse :cell-motion`.

### Applications

- `file-manager.lisp` — Full-featured terminal file manager. Navigate with arrow keys or `j`/`k`, `Enter` to open files/directories, `Backspace` to go up, `/` to search, `h` for help, `q` quits.

### Networking & System

- `http.lisp` — Async HTTP example using commands, with basic error handling.
- `window-size.lisp` — Query current terminal size; press any key to quit.

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

- Messages: CLOS classes representing events (key presses, mouse events, ticks, custom data)
- Commands: Functions that return messages (for async operations)
- Model: Your application state (CLOS object)
- Update: Generic methods specialized on message types: (model, message) → (new-model, command)
- View: Pure function: model → string
