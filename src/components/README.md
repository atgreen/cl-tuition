# Tuition Components

Reusable TUI components for Tuition applications.

## Available Components

### Spinner

An animated loading spinner with multiple styles.

```lisp
(use-package :tui.spinner)

;; Create a spinner
(defparameter *spinner* (make-spinner :frames *spinner-dot* :fps 0.1))

;; In your init
(defmethod tui:init ((model my-model))
  (spinner-init *spinner*))

;; In your update - delegate spinner messages
(defmethod tui:update ((model my-model) msg)
  (cond
    ((spinner-tick-msg-p msg)
     (multiple-value-bind (new-spinner cmd)
         (spinner-update (model-spinner model) msg)
       (setf (model-spinner model) new-spinner)
       (values model cmd)))
    ...))

;; In your view
(defmethod tui:view ((model my-model))
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
(use-package :tui.textinput)

;; Create a text input
(defparameter *input* (make-textinput
                       :placeholder "Enter your name"
                       :prompt "> "
                       :width 30
                       :char-limit 50))

;; In your update - delegate key messages
(defmethod tui:update ((model my-model) msg)
  (cond
    ((tui:key-msg-p msg)
     (multiple-value-bind (new-input cmd)
         (textinput-update (model-input model) msg)
       (setf (model-input model) new-input)
       (values model cmd)))
    ...))

;; In your view
(defmethod tui:view ((model my-model))
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

### Textarea

Multi-line text editor with cursor navigation, word editing, optional
soft-wrapping, and an optional dynamic height.

```lisp
(use-package :tui.textarea)

;; Create a textarea
(defparameter *ta* (make-textarea
                     :width 60 :height 10
                     :placeholder "Type here..."
                     :char-limit 280 :max-lines 1000
                     :soft-wrap t :dynamic-height t))

;; In your update - delegate key messages (only handled when focused)
(defmethod tui:update ((model my-model) msg)
  (multiple-value-bind (new-ta cmd)
      (textarea-update (model-textarea model) msg)
    (setf (model-textarea model) new-ta)
    (values model cmd)))

;; In your view
(defmethod tui:view ((model my-model))
  (textarea-view (model-textarea model)))

;; Programmatic access
(textarea-focus *ta*)                        ; focus (required to receive keys)
(textarea-blur *ta*)
(textarea-value *ta*)                        ; full contents
(textarea-set-value *ta* "line 1
line 2")
(textarea-insert-string *ta* "more text")
(textarea-cursor-position *ta*)              ; => (values row col)
(textarea-line-count *ta*)
```

**Options:**
- `:char-limit` / `:max-lines` — caps enforced on insert (0 / nil = unlimited)
- `:soft-wrap t` — long lines wrap to `width` display columns; cursor and
  scroll tracking move to visual lines
- `:dynamic-height t` — viewport grows/shrinks to fit content, clamped to
  `:min-height` / `:max-height`
- `:show-line-numbers`, `:prompt`, `:placeholder`, `:width`, `:height`

Soft-wrap and dynamic-height default off, so a plain textarea behaves exactly
as before.

**Keybindings:**
- `←/→/↑/↓` - Move cursor
- `Home`/`End` (or `Ctrl-a`/`Ctrl-e`) - Line start/end; `Ctrl-Home`/`Ctrl-End` - Document start/end
- `Alt-b`/`Alt-f` - Move by word; `Alt-Backspace`/`Ctrl-w` and `Alt-d` - Delete by word
- `Ctrl-t` - Transpose characters; `Alt-c`/`Alt-l`/`Alt-u` - Capitalize/lowercase/uppercase word
- `Ctrl-k`/`Ctrl-u` - Delete to end/start of line
- `PageUp`/`PageDown` - Page the viewport
- `Backspace`/`Delete`/`Enter` - Delete and newline
- `Ctrl-v` (or a bracketed paste) - Paste

### Progress Bar

Visual progress indicator with customizable appearance.

```lisp
(use-package :tui.progress)

;; Create a progress bar
(defparameter *progress* (make-progress
                          :percent 0.0
                          :width 40
                          :show-percentage t
                          :full-char #\█
                          :empty-char #\░))

;; In your view
(defmethod tui:view ((model my-model))
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

**Colors and gradient fill**

`make-progress` accepts `:colors` (a list of hex color stops) and `:empty-color`:

- omitted / `nil` — plain fill (the default, as above)
- a single hex string — solid fill
- two or more hex strings — a gradient blend across the filled portion

```lisp
(make-progress :percent 0.75 :width 40
               :colors (list "#5A56E0" "#EE6FF8")  ; purple → pink gradient
               :empty-color "#606060")             ; dim the empty portion
```

Colors are interpolated evenly between stops (ported from bubbles progress #838).
Hex strings are resolved to the terminal's best mode (truecolor, 256, or 16).

### List

Scrollable list with selection and keyboard navigation.

```lisp
(use-package :tui.list)

;; Create a list
(defparameter *list* (make-list-view
                      :items '("Apple" "Banana" "Cherry" "Date")
                      :height 10))

;; In your update - delegate key messages
(defmethod tui:update ((model my-model) msg)
  (cond
    ((tui:key-msg-p msg)
     (multiple-value-bind (new-list cmd)
         (list-update (model-list model) msg)
       (setf (model-list model) new-list)
       (values model cmd)))
    ...))

;; In your view
(defmethod tui:view ((model my-model))
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

### Date Picker

Interactive calendar for date selection with keyboard navigation.

```lisp
(use-package :tui.datepicker)

;; Create a date picker (defaults to today)
(defparameter *picker* (make-datepicker))

;; Create with specific date and selection
(defparameter *picker* (make-datepicker
                        :time (encode-universal-time 0 0 0 15 6 2025)  ; June 15, 2025
                        :selected (encode-universal-time 0 0 0 20 6 2025)))

;; In your update - delegate key messages
(defmethod tui:update ((model my-model) msg)
  (cond
    ((tui:key-msg-p msg)
     (multiple-value-bind (new-picker cmd)
         (datepicker-update (model-picker model) msg)
       (setf (model-picker model) new-picker)
       (values model cmd)))
    ...))

;; In your view
(defmethod tui:view ((model my-model))
  (datepicker-view (model-picker model)))

;; Get/check selected date
(datepicker-selected *picker*)   ; Returns universal-time or NIL

;; Programmatic navigation
(datepicker-tomorrow *picker*)
(datepicker-yesterday *picker*)
(datepicker-next-week *picker*)
(datepicker-last-week *picker*)
(datepicker-next-month *picker*)
(datepicker-last-month *picker*)
(datepicker-next-year *picker*)
(datepicker-last-year *picker*)

;; Selection control
(datepicker-select *picker*)     ; Select focused date
(datepicker-unselect *picker*)   ; Clear selection
```

**Keybindings:**
- `←/h` - Previous day
- `→/l` - Next day
- `↑/k` - Previous week (same weekday)
- `↓/j` - Next week (same weekday)
- `[` / `]` - Previous/next month
- `{` / `}` - Previous/next year
- `Home` - Jump to today
- `Enter/Space` - Select date
- `Escape` - Clear selection

**Output example:**
```
     January 2025
 Su Mo Tu We Th Fr Sa
           1  2  3  4
  5  6  7  8  9 10 11
 12 13 14 15 16 17 18
 19 20 21 22 23 24 25
 26 27 28 29 30 31
```

**Custom Styling:**

```lisp
;; Create custom styles
(defparameter *my-styles*
  (make-datepicker-styles
   :header (tui:make-style :bold t :foreground tui:*fg-cyan*)
   :today (tui:make-style :foreground tui:*fg-green* :bold t)
   :selected (tui:make-style :reverse t :foreground tui:*fg-yellow*)
   :cursor (tui:make-style :underline t)
   :outside-month (tui:make-style :faint t)))

;; Use custom styles
(make-datepicker :styles *my-styles*)
```

**Available style slots:**
| Slot | Default | Purpose |
|------|---------|---------|
| `:header` | none | Month/year header |
| `:day-names` | none | "Su Mo Tu We..." row |
| `:day` | none | Normal days |
| `:today` | bold | Today's date |
| `:selected` | reverse | Selected date |
| `:cursor` | underline | Cursor position |
| `:selected-cursor` | bold+reverse | Cursor on selection |
| `:outside-month` | faint | Days outside month |

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
  ((spinner :initform (tui.spinner:make-spinner) :accessor model-spinner)
   (input :initform (tui.textinput:make-textinput) :accessor model-input)
   (progress :initform (tui.progress:make-progress) :accessor model-progress)))

(defmethod tui:init ((model my-model))
  ;; Return initial command from a component
  (tui.spinner:spinner-init (model-spinner model)))

(defmethod tui:update ((model my-model) msg)
  (cond
    ;; Delegate to appropriate component
    ((tui.spinner:spinner-tick-msg-p msg)
     (multiple-value-bind (new-spinner cmd)
         (tui.spinner:spinner-update (model-spinner model) msg)
       (setf (model-spinner model) new-spinner)
       (values model cmd)))

    ((tui:key-msg-p msg)
     (multiple-value-bind (new-input cmd)
         (tui.textinput:textinput-update (model-input model) msg)
       (setf (model-input model) new-input)
       (values model cmd)))

    (t (values model nil))))

(defmethod tui:view ((model my-model))
  (format nil "~A~%~A~%~A~%"
          (tui.spinner:spinner-view (model-spinner model))
          (tui.textinput:textinput-view (model-input model))
          (tui.progress:progress-view (model-progress model))))
```

## Package Nicknames

For convenience, all components have short nicknames:

- `tui.spinner` = `tuition.components.spinner`
- `tui.textinput` = `tuition.components.textinput`
- `tui.progress` = `tuition.components.progress`
- `tui.list` = `tuition.components.list`
- `tui.datepicker` = `tuition.components.datepicker`

## Design Philosophy

Components are designed to be:

1. **Composable** - Use multiple components together
2. **Delegative** - Pass messages to components, they handle their own state
3. **Pure** - Update functions are pure (no side effects except state changes)
4. **Self-contained** - Each component manages its own state and rendering

## See Also

- [Examples](../examples/README.md) - Working examples using components
- [Bubbles](https://github.com/charmbracelet/bubbles) - Original Go library that inspired these components
