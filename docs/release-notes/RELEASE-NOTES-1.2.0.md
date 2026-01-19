# tuition 1.2.0 Release Notes

I'm pleased to announce tuition 1.2.0, a feature release of the Common Lisp library for building terminal user interfaces.

## What's New

### Overlay Compositing

A new overlay system for layering text blocks on top of each other, inspired by [rmhubbert/bubbletea-overlay](https://github.com/rmhubbert/bubbletea-overlay).

**Features:**
- `composite` function for layering foreground text on backgrounds with full positioning control
- ANSI-aware string manipulation preserves escape codes correctly
- Support for `:left`, `:center`, `:right`, `:top`, `:middle`, `:bottom` positioning
- Fractional positioning (0.0-1.0) and absolute pixel positioning
- X/Y offset fine-tuning for precise placement
- Background color extraction and restoration across overlay boundaries

**Usage:**
```lisp
;; Center a dialog on a background
(composite dialog background :x-position +center+ :y-position +middle+)

;; Convenience functions
(overlay-centered foreground background)
(overlay-at foreground background 10 5)  ; absolute position
```

**Example:**
```bash
sbcl --load examples/overlay.lisp
```

### Windows Console Support (Experimental)

Tuition now includes experimental Windows support through the Windows Console API.

**What Works:**
- Terminal raw mode via SetConsoleMode
- ANSI escape sequences for styling and layout
- Terminal size detection via GetConsoleScreenBufferInfo
- Keyboard and mouse input handling
- All core TUI features (styling, borders, layout, components)

**Requirements:**
- SBCL on Windows
- Windows 10 version 1511 or later
- Windows Terminal recommended for best experience

See `docs/WINDOWS.md` for detailed documentation.

## Bug Fixes and Improvements

### Layout Improvements
- `join-horizontal` and `join-vertical` now handle empty block lists gracefully
- `place-vertical` handles text larger than available height by showing a window into the content
- Position values are now clamped to valid range (0.0-1.0)

### Style Fixes
- Outer background colors are now re-applied after inner ANSI resets, fixing styled content within styled containers
- `split-string-by-newline` now handles Windows CR/LF line endings
- Text wrapping now preserves intentional spacing in content

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
