# Tuition Styling Guide

Terminal styling and layout utilities for Tuition.

## Quick Start

```lisp
(use-package :tuition)

;; Simple text formatting
(bold "This is bold")
(italic "This is italic")
(underline "This is underlined")

;; Colors
(colored "Red text" :fg *fg-bright-red*)
(colored "Green on blue" :fg *fg-green* :bg *bg-blue*)

;; Advanced styling
(let ((style (make-style :foreground *fg-bright-magenta*
                         :bold t
                         :padding 2
                         :width 30
                         :align :center)))
  (render-styled style "Hello, World!"))
```

## Colors

### Foreground Colors

**Standard Colors (30-37):**
- `*fg-black*`, `*fg-red*`, `*fg-green*`, `*fg-yellow*`
- `*fg-blue*`, `*fg-magenta*`, `*fg-cyan*`, `*fg-white*`

**Bright Colors (90-97):**
- `*fg-bright-black*`, `*fg-bright-red*`, `*fg-bright-green*`, `*fg-bright-yellow*`
- `*fg-bright-blue*`, `*fg-bright-magenta*`, `*fg-bright-cyan*`, `*fg-bright-white*`

### Background Colors

**Standard Colors (40-47):**
- `*bg-black*`, `*bg-red*`, `*bg-green*`, `*bg-yellow*`
- `*bg-blue*`, `*bg-magenta*`, `*bg-cyan*`, `*bg-white*`

**Bright Colors (100-107):**
- `*bg-bright-black*`, `*bg-bright-red*`, `*bg-bright-green*`, `*bg-bright-yellow*`
- `*bg-bright-blue*`, `*bg-bright-magenta*`, `*bg-bright-cyan*`, `*bg-bright-white*`

### Color Example

```lisp
(colored "Error!" :fg *fg-bright-red*)
(colored "Success!" :fg *fg-bright-green*)
(colored "Warning" :fg *fg-bright-yellow* :bg *bg-black*)
```

## Text Formatting

### Simple Formatting Functions

```lisp
;; Bold
(bold "Important text")

;; Italic
(italic "Emphasized text")

;; Underline
(underline "Linked text")

;; Combined with colors
(bold (colored "Bold red" :fg *fg-bright-red*))
```

### Style Objects

Create reusable styles with `make-style`:

```lisp
(defparameter *title-style*
  (make-style :foreground *fg-bright-magenta*
              :bold t
              :underline t))

(render-styled *title-style* "My Title")
```

### Style Options

**Text Attributes:**
- `:bold` - Bold text
- `:italic` - Italic text
- `:underline` - Underlined text
- `:blink` - Blinking text
- `:reverse` - Reverse video (swap fg/bg)
- `:strikethrough` - Strikethrough text

**Colors:**
- `:foreground` - Text color
- `:background` - Background color

**Spacing:**
- `:padding` - Uniform padding (shorthand)
- `:padding-left`, `:padding-right`, `:padding-top`, `:padding-bottom`
- `:margin` - Uniform margin (shorthand)
- `:margin-left`, `:margin-right`, `:margin-top`, `:margin-bottom`

**Layout:**
- `:width` - Fixed width
- `:align` - Horizontal alignment (`:left`, `:center`, `:right`)

## Padding and Margins

### Padding

Padding adds space inside the styled area:

```lisp
;; Uniform padding
(make-style :padding 2)

;; Different padding on each side
(make-style :padding-left 4
            :padding-right 4
            :padding-top 1
            :padding-bottom 1)
```

### Margins

Margins add space outside the styled area:

```lisp
;; Uniform margin
(make-style :margin 1)

;; Top margin only
(make-style :margin-top 2)
```

## Width and Alignment

### Fixed Width

```lisp
;; Left-aligned in 40 character width
(make-style :width 40 :align :left)

;; Centered in 40 character width
(make-style :width 40 :align :center)

;; Right-aligned in 40 character width
(make-style :width 40 :align :right)
```

### Example

```lisp
(let ((style (make-style :width 30 :align :center :bold t)))
  (render-styled style "Centered Title"))
;; Output: "      Centered Title       "
```

## Layout Utilities

### Joining Blocks Horizontally

Place text blocks side by side:

```lisp
(join-horizontal :top
  "Block 1\nLine 2\nLine 3"
  "Block 2\nLine 2")
```

Alignment options:
- `:top` - Align tops
- `:middle` - Align middles
- `:bottom` - Align bottoms
- `0.0` to `1.0` - Custom position (0.0 = top, 0.5 = middle, 1.0 = bottom)

### Joining Blocks Vertically

Stack text blocks:

```lisp
(join-vertical :center
  "First block"
  "Second block"
  "Third block")
```

Alignment options:
- `:left` - Align lefts
- `:center` - Align centers
- `:right` - Align rights
- `0.0` to `1.0` - Custom position

### Placing Text in Space

```lisp
;; Center horizontally in 80 characters
(place-horizontal 80 :center "Hello")

;; Place at bottom of 20 lines
(place-vertical 20 :bottom "Footer")

;; Place in bottom-right of 80x24 space
(place 80 24 :right :bottom "Status")
```

## Complete Example

```lisp
(defmethod view ((model my-model))
  (let ((title-style (make-style
                      :foreground *fg-bright-magenta*
                      :bold t
                      :padding-top 1
                      :padding-bottom 1
                      :width 60
                      :align :center))
        (box-style (make-style
                    :foreground *fg-bright-white*
                    :background *bg-blue*
                    :padding 2
                    :margin 1))
        (error-style (make-style
                      :foreground *fg-bright-red*
                      :bold t)))

    (join-vertical :left
      (render-styled title-style "My Application")
      (render-styled box-style "Main content area")
      (colored "Status: " :fg *fg-bright-green*)
      (when (model-error model)
        (render-styled error-style (model-error model))))))
```

## Tips and Best Practices

### 1. Create Style Constants

Define your styles once and reuse them:

```lisp
(defparameter *heading-style*
  (make-style :foreground *fg-bright-cyan*
              :bold t
              :padding-bottom 1))

(defparameter *error-style*
  (make-style :foreground *fg-bright-red*
              :bold t))

(defparameter *success-style*
  (make-style :foreground *fg-bright-green*
              :bold t))
```

### 2. Compose Styles

Build complex layouts from simple pieces:

```lisp
(let ((left-panel (render-styled *panel-style* (format-menu model)))
      (right-panel (render-styled *panel-style* (format-details model))))
  (join-horizontal :top left-panel right-panel))
```

### 3. Conditional Styling

Apply different styles based on state:

```lisp
(let ((style (if (model-selected-p model)
                *selected-style*
                *normal-style*)))
  (render-styled style text))
```

### 4. ANSI Escape Codes

Be aware that styled text contains ANSI escape codes. Use `visible-length` instead of `length` when measuring:

```lisp
;; Wrong - includes escape codes
(length (bold "text"))  ; => much more than 4

;; Right - visible characters only
(visible-length (bold "text"))  ; => 4
```

## Compatibility

The styling system uses standard ANSI escape codes and should work in:
- Modern terminal emulators (iTerm2, Terminal.app, GNOME Terminal, etc.)
- SSH sessions
- tmux/screen (with proper TERM settings)

Some features may degrade gracefully in limited terminals.

## See Also

- [Lipgloss](https://github.com/charmbracelet/lipgloss) - Original Go library
- [ANSI Escape Codes](https://en.wikipedia.org/wiki/ANSI_escape_code) - Technical reference
