# Mouse Zone Tracking

Tuition includes a powerful zone tracking system which makes it easy to handle mouse events in complex TUI applications with multiple clickable components.

## The Problem

When building complex TUIs with multiple nested components, tracking which component was clicked becomes tedious. You need to:

- Calculate the position of each component
- Track position changes when layout shifts
- Manually check if mouse coordinates fall within each component's bounds

## The Solution

**Zone tracking** allows you to wrap components with invisible markers. The system:

1. Wraps components with zero-width ANSI escape sequences (invisible markers)
2. Scans the final view output to detect marker positions
3. Stores bounding boxes for each marked zone
4. Provides simple `zone-in-bounds-p` checks for mouse events

The markers don't affect width calculations (when using `visible-length`) and are stripped from the final output.

## Features

- ✅ **Fast** - Uses simple string scanning and hash tables
- ✅ **Invisible** - Markers don't affect `visible-length()` width calculations
- ✅ **Simple** - Easy bounds checking with `zone-in-bounds-p`
- ✅ **Relative positioning** - Get mouse position relative to zone with `zone-pos`
- ✅ **Unique prefixes** - Prevent ID collisions with `zone-new-prefix`

## Basic Usage

### 1. Initialize the Zone Manager

In your root model's `init` method:

```lisp
(defmethod init ((model root-model))
  (init-global-zone-manager)
  nil)
```

### 2. Enable Mouse Support

Create your program with mouse tracking:

```lisp
(let ((program (make-program model
                             :alt-screen t
                             :mouse-cell-motion t)))
  (run program))
```

### 3. Mark Components in View

Wrap clickable components with `zone-mark`:

```lisp
(defmethod view ((model button-model))
  (let* ((text "Click Me!")
         (styled (render-styled (make-style :background *bg-blue*
                                            :padding 1)
                                text))
         (marked (zone-mark "my-button" styled)))
    marked))
```

### 4. Scan at Root Level

Wrap your root view output with `zone-scan`:

```lisp
(defmethod view ((model root-model))
  (let ((content (format nil "~A~%~A"
                        (view (model-header model))
                        (view (model-button model)))))
    (zone-scan content)))
```

### 5. Check Bounds in Update

In your update method, check if mouse events are within zones:

```lisp
(defmethod update ((model button-model) msg)
  (cond
    ((mouse-msg-p msg)
     (when (and (eql (mouse-msg-action msg) :press)
                (eql (mouse-msg-button msg) :left))
       (let ((zone (zone-get "my-button")))
         (when (zone-in-bounds-p zone msg)
           ;; Button was clicked!
           (setf (model-active model) t))))
     (values model nil))
    (t (values model nil))))
```

## Complete Example

```lisp
(defpackage #:example-zones
  (:use #:cl #:tuition))

(in-package #:example-zones)

(defclass app-model ()
  ((buttons :initform '("Save" "Cancel" "Help")
            :accessor model-buttons)
   (selected :initform nil :accessor model-selected)
   (prefix :initform (zone-new-prefix) :accessor model-prefix)))

(defmethod init ((model app-model))
  (init-global-zone-manager)
  nil)

(defmethod update ((model app-model) msg)
  (cond
    ((key-msg-p msg)
     (when (and (characterp (key-msg-key msg))
                (char= (key-msg-key msg) #\q))
       (values model (quit-cmd))))

    ((mouse-msg-p msg)
     (when (and (eql (mouse-msg-action msg) :press)
                (eql (mouse-msg-button msg) :left))
       (let ((prefix (model-prefix model)))
         ;; Check each button
         (loop for btn in (model-buttons model)
               for i from 0
               for zone-id = (format nil "~Abtn-~D" prefix i)
               for zone = (zone-get zone-id)
               do (when (zone-in-bounds-p zone msg)
                    (setf (model-selected model) btn)
                    (return)))))
     (values model nil))

    (t (values model nil))))

(defmethod view ((model app-model))
  (let* ((prefix (model-prefix model))
         (buttons '()))

    ;; Create styled and marked buttons
    (loop for btn in (model-buttons model)
          for i from 0
          for zone-id = (format nil "~Abtn-~D" prefix i)
          for is-selected = (string= btn (or (model-selected model) ""))
          for style = (make-style
                       :background (if is-selected *bg-blue* *bg-black*)
                       :foreground (if is-selected *fg-bright-white* *fg-white*)
                       :padding 1
                       :bold is-selected)
          for rendered = (render-styled style btn)
          for marked = (zone-mark zone-id rendered)
          do (push marked buttons))

    ;; Build view
    (let ((output (format nil "Click a button:~%~%  ~{~A~^  ~}~%~%Selected: ~A~%~%Press q to quit"
                         (nreverse buttons)
                         (or (model-selected model) "None"))))
      ;; Scan and return
      (zone-scan output))))

(defun main ()
  (let ((program (make-program (make-instance 'app-model)
                               :alt-screen t
                               :mouse-cell-motion t)))
    (run program)))
```

## Advanced Usage

### Unique Prefixes

When you have multiple instances of the same component, use `zone-new-prefix` to prevent ID collisions:

```lisp
(defclass list-item ()
  ((text :initarg :text :accessor item-text)
   (id-prefix :initform (zone-new-prefix) :accessor item-id-prefix)))

(defmethod view ((item list-item))
  (let* ((zone-id (format nil "~Aitem" (item-id-prefix item)))
         (styled (render-styled (make-style :padding 1) (item-text item)))
         (marked (zone-mark zone-id styled)))
    marked))
```

### Relative Position

Get mouse coordinates relative to a zone (useful for input boxes):

```lisp
(defmethod update ((model textbox-model) msg)
  (when (mouse-msg-p msg)
    (let ((zone (zone-get "my-textbox")))
      (multiple-value-bind (x y) (zone-pos zone msg)
        (when (>= x 0)
          ;; x and y are relative to textbox top-left corner
          (setf (model-cursor-pos model) x))))))
```

### Enabling/Disabling

Toggle zone tracking dynamically:

```lisp
;; Disable zone tracking
(zone-set-enabled nil)

;; Re-enable
(zone-set-enabled t)
```

### Clearing Zones

Remove stored zone data:

```lisp
;; Clear specific zone
(zone-clear "button-1")
```

## Tips

### Only Scan at Root

Call `zone-scan` only in your root/outermost model's `view` method. Don't scan in child components.

✅ **Good:**
```lisp
(defmethod view ((model root-model))
  (zone-scan (format nil "~A~%~A"
                    (view header-model)
                    (view content-model))))
```

❌ **Bad:**
```lisp
(defmethod view ((model child-model))
  (zone-scan "..."))  ; Don't scan in children!
```

### Width Calculations

Use `visible-length` for width measurements, not `length`. Zone markers are invisible to `visible-length`:

```lisp
(visible-length (zone-mark "id" "hello"))  ; => 5 (correct)
(length (zone-mark "id" "hello"))          ; => 19 (includes markers)
```

### Organic Shapes

Zone bounds are rectangular. For circular or irregular shapes, the zone will include the entire bounding box. Ensure proper padding around shaped components.

### Zone Markers Format

Zone markers use the format: `ESC[<number>z`

This uses a private ANSI control sequence that terminals ignore but doesn't interfere with other ANSI codes or lipgloss width calculations.

## API Reference

### Functions

- `(init-global-zone-manager)` - Initialize the global zone manager
- `(zone-mark id text &optional manager)` - Mark text with zone markers
- `(zone-scan text &optional manager)` - Scan text and extract zone positions
- `(zone-get id &optional manager)` - Get zone-info for an ID
- `(zone-clear id &optional manager)` - Clear zone data for an ID
- `(zone-new-prefix &optional manager)` - Generate unique ID prefix
- `(zone-set-enabled enabled &optional manager)` - Enable/disable zone tracking
- `(zone-enabled-p &optional manager)` - Check if enabled

### Methods

- `(zone-in-bounds-p zone msg)` - Check if mouse message is within zone bounds
- `(zone-pos zone msg)` - Get relative position within zone (returns x, y or -1, -1)

### Classes

- `zone-manager` - Manages zone tracking state
- `zone-info` - Stores zone position data
  - `zone-info-start-x` - Left edge (0-based)
  - `zone-info-start-y` - Top edge (0-based)
  - `zone-info-end-x` - Right edge (0-based)
  - `zone-info-end-y` - Bottom edge (0-based)

### Variables

- `*zone-manager*` - Global zone manager instance

## See Also

- [BubbleZone](https://github.com/lrstanley/bubblezone) - The Go library that inspired this implementation
- Mouse example: `examples/zones.lisp`
