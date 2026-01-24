# Borders and Shadows

Tuition provides border rendering with title bars, opaque drop shadows, and transparent shadow compositing for creating depth in TUI dialogs and buttons.

## Borders

### Basic Usage

```lisp
(render-border "Hello, World!" *border-rounded*)
;; ╭─────────────╮
;; │Hello, World!│
;; ╰─────────────╯
```

### Border Styles

- `*border-normal*` - Standard 90-degree corners (`┌─┐│└─┘`)
- `*border-rounded*` - Rounded corners (`╭─╮│╰─╯`)
- `*border-thick*` - Thick lines (`┏━┓┃┗━┛`)
- `*border-double*` - Double lines (`╔═╗║╚═╝`)
- `*border-block*` - Solid blocks (`███`)
- `*border-hidden*` - Invisible (spaces)
- `*border-ascii*` - ASCII only (`+-+|+-+`)
- `*border-markdown*` - Markdown table style

### Colored Borders

```lisp
(render-border content *border-rounded*
               :fg-color *fg-bright-white*
               :bg-color *bg-blue*)
```

### Title Bars

Place a title label on the top border line:

```lisp
(render-border content *border-double*
               :title (colored " Dialog " :fg *fg-bright-yellow*)
               :title-position :center)
;; ╔═════════ Dialog ═════════╗
;; ║ content                  ║
;; ╚══════════════════════════╝
```

Parameters:
- `:title` - Title text (can include ANSI styling)
- `:title-position` - `:left`, `:center`, or `:right` (default `:center`)

At least one border character is preserved on each side of the title.

## Opaque Shadows

`render-shadow` adds block-character drop shadows to a standalone text block:

```lisp
(render-shadow bordered-text)
;; ╔══════════╗
;; ║ Content  ║██
;; ╚══════════╝██
;;  ▀▀▀▀▀▀▀▀▀▀▀▀▀
```

Parameters:
- `:width` - Shadow thickness in characters (default 2)
- `:offset` - Rows/cols before shadow starts (default 1, creates depth)
- `:color` - Shadow color (default `*fg-bright-black*`)
- `:style` - Character preset (default `:dark`)

### Shadow Styles

- `:dark` - Right edge `█`, bottom edge `▀` (half-height, natural look)
- `:solid` - `█` everywhere
- `:light` - `░` (light shade)
- `:medium` - `▒` (medium shade)
- `:heavy` - `▓` (heavy shade)

## Transparent Shadows

`composite-with-shadow` composites a foreground onto a background with a transparent drop shadow where the underlying text remains visible but darkened:

```lisp
(composite-with-shadow dialog background
                       :x-position +center+
                       :y-position +middle+)
;; Background text shows through the shadow region in dark gray
```

This is the preferred approach when compositing dialogs or buttons onto a background, as it creates a natural depth effect without fully obscuring the content behind.

Parameters (in addition to standard composite positioning):
- `:shadow-width` - Shadow thickness in characters (default 2)
- `:shadow-offset` - Rows/cols before shadow starts (default 1)
- `:shadow-color` - Foreground color for darkened text (default `*fg-bright-black*`)
- `:shadow-bg-color` - Background color for shadow region (default `*bg-black*`)

### Buttons with Shadows

For small elements like buttons, use `composite-with-shadow` with reduced shadow size:

```lisp
(composite-with-shadow button-text background
                       :x-position 5
                       :y-position 2
                       :shadow-width 1
                       :shadow-offset 0)
```

## Complete Dialog Example

```lisp
(let* ((bg (build-background 60 15))
       (content (render-styled
                 (make-style :foreground *fg-white*
                             :background *bg-blue*
                             :padding 1 :width 28)
                 "Dialog content here."))
       (title (colored " Confirm " :fg *fg-bright-yellow*))
       (dialog (render-border content *border-double*
                              :fg-color *fg-bright-white*
                              :bg-color *bg-blue*
                              :title title)))
  (composite-with-shadow dialog bg
                         :x-position +center+
                         :y-position +middle+))
```
