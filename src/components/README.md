# Tuition Components

Reusable TUI components for Tuition applications.

## Available Components

### Spinner

An animated loading spinner with multiple styles.

```lisp
(use-package :tea.spinner)

;; Create a spinner
(defparameter *spinner* (make-spinner :frames *spinner-dot* :fps 0.1))

;; In your init
(defmethod tea:init ((model my-model))
  (spinner-init *spinner*))

;; In your update - delegate spinner messages
(defmethod tea:update ((model my-model) msg)
  (cond
    ((spinner-tick-msg-p msg)
     (multiple-value-bind (new-spinner cmd)
         (spinner-update (model-spinner model) msg)
       (setf (model-spinner model) new-spinner)
       (values model cmd)))
    ...))

;; In your view
(defmethod tea:view ((model my-model))
  (format nil "Loading... ~A" (spinner-view (model-spinner model))))
```

**Available spinner styles:**
- `*spinner-line*` - Simple line spinner: `| / - \`
- `*spinner-dot*` - Braille dot spinner
- `*spinner-minidot*` - Smaller braille dots
- `*spinner-jump*` - Jumping animation
- `*spinner-pulse*` - Pulsing blocks
- `*spinner-points*` - Three dots
- `*spinner-globe*` - Rotating globe 🌍🌎🌏
- `*spinner-moon*` - Moon phases 🌑🌒🌓...
- `*spinner-monkey*` - See-no-evil monkeys
- `*spinner-meter*` - Meter animation
- `*spinner-hamburger*` - Trigram symbols
- `*spinner-ellipsis*` - Ellipsis ...

### Text Input

Single-line text input field with cursor support.

```lisp
(use-package :tea.textinput)

;; Create a text input
(defparameter *input* (make-textinput
                       :placeholder "Enter your name"
                       :prompt "> "
                       :width 30
                       :char-limit 50))

;; In your update - delegate key messages
(defmethod tea:update ((model my-model) msg)
  (cond
    ((tea:key-msg-p msg)
     (multiple-value-bind (new-input cmd)
         (textinput-update (model-input model) msg)
       (setf (model-input model) new-input)
       (values model cmd)))
    ...))

;; In your view
(defmethod tea:view ((model my-model))
  (textinput-view (model-input model)))

;; Get the value
(textinput-value *input*)

;; Helper functions
(textinput-focus *input*)        ; Focus the input
(textinput-blur *input*)         ; Remove focus
(textinput-set-value *input* "text")  ; Set value
(textinput-reset *input*)        ; Clear input
```

**Keybindings:**
- `←/→` - Move cursor
- `Home/End` - Jump to start/end
- `Backspace/Delete` - Delete characters
- Any printable character - Insert

### Progress Bar

Visual progress indicator with customizable appearance.

```lisp
(use-package :tea.progress)

;; Create a progress bar
(defparameter *progress* (make-progress
                          :percent 0.0
                          :width 40
                          :show-percentage t
                          :full-char #\█
                          :empty-char #\░))

;; In your view
(defmethod tea:view ((model my-model))
  (format nil "Download: ~A" (progress-view (model-progress model))))

;; Update progress
(progress-set-percent *progress* 0.75)     ; Set to 75%
(progress-increment *progress* 0.1)        ; Add 10%

;; Access current value
(progress-percent *progress*)  ; Returns 0.0 to 1.0
```

**Output example:**
```
[████████████████████████████████░░░░░░░░]  75%
```

### List

Scrollable list with selection and keyboard navigation.

```lisp
(use-package :tea.list)

;; Create a list
(defparameter *list* (make-list-view
                      :items '("Apple" "Banana" "Cherry" "Date")
                      :height 10))

;; In your update - delegate key messages
(defmethod tea:update ((model my-model) msg)
  (cond
    ((tea:key-msg-p msg)
     (multiple-value-bind (new-list cmd)
         (list-update (model-list model) msg)
       (setf (model-list model) new-list)
       (values model cmd)))
    ...))

;; In your view
(defmethod tea:view ((model my-model))
  (list-view-render (model-list model)))

;; Get selected item
(list-get-selected *list*)

;; Update items
(list-set-items *list* '("New" "Items"))

;; Manual navigation
(list-move-up *list*)
(list-move-down *list*)
```

**Keybindings:**
- `↑/k` - Move selection up
- `↓/j` - Move selection down

**Output example:**
```
  Apple
> Banana
  Cherry
  Date
```

## Component Pattern

All components follow a consistent pattern:

1. **Creation** - `make-component` function with keyword arguments
2. **Init** - `component-init` returns initial command (or nil)
3. **Update** - `component-update` takes (component msg) returns `(values new-component cmd)`
4. **View** - `component-view` renders to string
5. **Helpers** - Additional functions for common operations

### Integration Example

```lisp
(defclass my-model ()
  ((spinner :initform (tea.spinner:make-spinner) :accessor model-spinner)
   (input :initform (tea.textinput:make-textinput) :accessor model-input)
   (progress :initform (tea.progress:make-progress) :accessor model-progress)))

(defmethod tea:init ((model my-model))
  ;; Return initial command from a component
  (tea.spinner:spinner-init (model-spinner model)))

(defmethod tea:update ((model my-model) msg)
  (cond
    ;; Delegate to appropriate component
    ((tea.spinner:spinner-tick-msg-p msg)
     (multiple-value-bind (new-spinner cmd)
         (tea.spinner:spinner-update (model-spinner model) msg)
       (setf (model-spinner model) new-spinner)
       (values model cmd)))

    ((tea:key-msg-p msg)
     (multiple-value-bind (new-input cmd)
         (tea.textinput:textinput-update (model-input model) msg)
       (setf (model-input model) new-input)
       (values model cmd)))

    (t (values model nil))))

(defmethod tea:view ((model my-model))
  (format nil "~A~%~A~%~A~%"
          (tea.spinner:spinner-view (model-spinner model))
          (tea.textinput:textinput-view (model-input model))
          (tea.progress:progress-view (model-progress model))))
```

## Package Nicknames

For convenience, all components have short nicknames:

- `tea.spinner` = `tuition.components.spinner`
- `tea.textinput` = `tuition.components.textinput`
- `tea.progress` = `tuition.components.progress`
- `tea.list` = `tuition.components.list`

## Design Philosophy

Components are designed to be:

1. **Composable** - Use multiple components together
2. **Delegative** - Pass messages to components, they handle their own state
3. **Pure** - Update functions are pure (no side effects except state changes)
4. **Self-contained** - Each component manages its own state and rendering

## See Also

- [Examples](../examples/README.md) - Working examples using components
- [Bubbles](https://github.com/charmbracelet/bubbles) - Original Go library that inspired these components
