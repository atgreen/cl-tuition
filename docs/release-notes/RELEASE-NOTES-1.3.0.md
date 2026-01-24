# tuition 1.3.0 Release Notes

I'm pleased to announce tuition 1.3.0, a feature release of the Common Lisp library for building terminal user interfaces.

## What's New

### Border Title Bars

`render-border` now supports embedding a title label on the top border line.

**Features:**
- `:title` parameter accepts plain or ANSI-styled text
- `:title-position` controls placement: `:left`, `:center`, or `:right`
- Title is automatically truncated if it exceeds available space
- At least one border character is preserved on each side

**Usage:**
```lisp
(render-border content *border-double*
               :title (colored " Dialog " :fg *fg-bright-yellow*)
               :title-position :center)
;; ╔═════════ Dialog ═════════╗
;; ║ content                  ║
;; ╚══════════════════════════╝
```

### Opaque Drop Shadows

`render-shadow` adds block-character drop shadows to standalone text blocks.

**Features:**
- Configurable width, offset, color, and style
- Five shadow style presets: `:dark`, `:solid`, `:light`, `:medium`, `:heavy`
- `:dark` style uses half-block (`▀`) bottom edge for a natural look

**Usage:**
```lisp
(render-shadow bordered-dialog)
(render-shadow button :width 1 :offset 0 :style :solid)
```

### Transparent Shadow Compositing

`composite-with-shadow` layers a foreground element onto a background with a transparent drop shadow where the underlying content remains visible but darkened.

**Features:**
- Background characters in the shadow region are preserved but re-colored
- Shadow foreground and background colors are configurable
- Works with all positioning options from `composite`
- Suitable for dialogs, buttons, and any overlaid element

**Usage:**
```lisp
;; Dialog with transparent shadow
(composite-with-shadow dialog background
                       :x-position +center+
                       :y-position +middle+)

;; Button with small transparent shadow
(composite-with-shadow button background
                       :x-position 5 :y-position 2
                       :shadow-width 1 :shadow-offset 0)
```

**Example:**
```bash
sbcl --load examples/overlay.lisp
```

## Documentation

- New `doc/BORDERS.md` covers borders, title bars, opaque shadows, and transparent shadow compositing
- README updated with shadow and title bar examples

## Installation

### Via ocicl
```bash
ocicl install tuition
```

### From Source
```bash
git clone https://github.com/atgreen/tuition.git
cd tuition
# Load in your Lisp environment
```

---

For more information, visit the [tuition repository](https://github.com/atgreen/tuition) or read the [README](https://github.com/atgreen/tuition/blob/master/README.md).
