# Tuition Examples

This directory contains example programs demonstrating various features of the Tuition TUI library.

## Running Examples

Load an example with:
```lisp
(asdf:load-system :tuition)
(load "examples/spinner.lisp")
(tuition-example-spinner:main)
```

Or from the command line:
```bash
sbcl --eval "(asdf:load-system :tuition)" --load examples/spinner.lisp --eval "(tuition-example-spinner:main)"
```

## Available Examples

### Basic Interactions

- **counter.lisp** - Interactive counter with arrow keys
  - ↑/+ increment, ↓/- decrement, r reset, q quit

- **simple.lisp** - Simple countdown timer
  - Basic example showing the Elm Architecture
 
### Formatting

- **reflow.lisp** - Wrap, truncate, and indent text
  - ←/→ adjust wrap width, q quit

### Animations

- **spinner.lisp** - Animated loading spinner
  - Demonstrates tick-based animation and self-updating components

- **progress.lisp** - Animated progress bar
  - Shows incremental progress updates over time

### Timing

- **stopwatch.lisp** - Start/stop/reset stopwatch
  - s start/stop, r reset, q quit
  - Demonstrates time tracking and formatting

- **timer.lisp** - Countdown timer with pause/resume
  - s start/stop, r reset, q quit
  - Shows countdown functionality

### Input

- **textinput.lisp** - Simple text input field
  - Type to enter text, backspace to delete
  - Arrow keys to move cursor, enter/esc to quit

- **list.lisp** - Scrollable list selection
  - ↑/k and ↓/j to navigate, enter to select, q to quit

### Advanced

- **http.lisp** - Async HTTP request example
  - Demonstrates command-based async operations
  - Shows error handling

- **mouse.lisp** - Mouse tracking and events
  - Use `:mouse :all-motion` when creating the program
  - Displays mouse coordinates and button events

- **window-size.lisp** - Terminal size detection
  - Press any key to query current terminal dimensions

## Example Structure

Each example follows the Elm Architecture pattern:

```lisp
(defclass my-model ()
  ((field :initform value :accessor field)))

(defmethod tea:init ((model my-model))
  ;; Return initial command or nil
  nil)

(defmethod tea:update ((model my-model) msg)
  ;; Handle messages, return (values new-model command)
  (values model nil))

(defmethod tea:view ((model my-model))
  ;; Render model to string
  (format nil "Hello, world!"))

(defun main ()
  (let ((program (tea:make-program (make-instance 'my-model))))
    (tea:run program)))
```

## Key Concepts

- **Messages**: Structs representing events (key presses, ticks, custom data)
- **Commands**: Functions that return messages (for async operations)
- **Model**: Your application state
- **Update**: Pure function: (model, message) → (new-model, command)
- **View**: Pure function: model → string
