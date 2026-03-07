# tuition 2.0.0 Release Notes

I'm pleased to announce tuition 2.0.0, a major release of the Common Lisp library for building terminal user interfaces.

## What's New

### Cell-Based Renderer

Tuition now uses a cell-based rendering engine that diffs screen buffers and only redraws changed cells, similar to ncurses. This is automatic — existing programs benefit without code changes.

- Each terminal cell is represented as a `cell` struct with character, width, foreground/background colors, and attributes
- `parse-styled-string` converts ANSI-styled strings into a screen buffer
- `render-diff` emits minimal ANSI sequences between two buffers for flicker-free updates

### Declarative View

Terminal modes are now controlled declaratively through the `view-state` returned by `view`, rather than at program creation time:

```lisp
(defmethod tui:view ((model my-model))
  (tui:make-view
   (format nil "Count: ~D" (counter model))
   :alt-screen t
   :mouse-mode :cell-motion
   :window-title "My App"))
```

This means terminal modes can change dynamically based on application state. Available options include `:alt-screen`, `:mouse-mode`, `:report-focus`, `:window-title`, `:cursor`, `:foreground-color`, `:background-color`, `:keyboard-enhancements`, and more.

### Compositing System

New layer-based compositing for building complex multi-panel UIs with z-ordering and hit testing:

```lisp
(let* ((header (tui:make-layer header-content))
       (body   (tui:make-layer body-content))
       (comp   (tui:make-compositor header body)))
  (tui:compositor-render comp))
```

Layers support positioning, z-ordering, and mouse hit testing via `compositor-hit`.

### Higher-Fidelity Input (Kitty Keyboard Protocol)

Terminals that support the Kitty keyboard protocol (kitty, foot, WezTerm) now report:

- Separate key press, release, and repeat events (`key-press-msg`, `key-release-msg`)
- Full Unicode codepoints
- Disambiguated modifier keys via a unified bitmask (`+mod-shift+`, `+mod-alt+`, `+mod-ctrl+`, `+mod-meta+`, `+mod-hyper+`, `+mod-super+`)
- Human-readable key descriptions via `keystroke`

### Clipboard Support (OSC 52)

Read and write system clipboard and primary selection:

```lisp
(tui:set-clipboard "text to copy")
(tui:read-clipboard)  ; terminal responds with clipboard-msg
```

### Style Enhancements

- **Extended underlines**: `:curly`, `:double`, `:dotted`, `:dashed`
- **Colored underlines**: `:underline-color`
- **Hyperlinks**: `:hyperlink "https://..."`
- **Border gradients**: `:fg-colors` list for rainbow borders
- **Custom fill characters**: `:padding-char`, `:margin-char`
- **Color utilities**: `darken-color`, `lighten-color`, `complementary-color`, `blend-colors`
- **Light/dark helper**: `light-dark` chooses values based on terminal background luminance

### Terminal Utilities

- Cursor shape control: `set-cursor-shape` (`:block`, `:underline`, `:bar`)
- Cursor color: `set-cursor-color`
- Color queries: `request-background-color-cmd`, `request-foreground-color-cmd`
- Terminal version query: `request-terminal-version`
- Unicode mode (DEC 2027): `enable-unicode-mode`
- Bracketed paste control: `:disable-bracketed-paste`
- Raw escape sequences: `raw-cmd`

### Comprehensive Test Suite

Added a FiveAM-based test suite with 200+ checks covering style utilities, borders, layout, list/table/tree rendering, input parsing, and text reflow. Uses golden file regression testing with ground-truth output from the charmbracelet/lipgloss Go project for byte-identical rendering verification.

## Breaking Changes

This is a major version bump with API changes. A v1 compatibility layer is provided (`tuition-v1` / `tui-v1` package) that maps old symbol names to v2 equivalents.

Key changes:

| v1 | v2 |
|----|-----|
| `key-msg` | `key-press-msg` |
| `key-msg-key` | `key-event-code` |
| `key-msg-ctrl` | `(mod-contains (key-event-mod msg) +mod-ctrl+)` |
| `mouse-press-event` | `mouse-click-msg` |
| `mouse-release-event` | `mouse-release-msg` |
| `mouse-drag-event` / `mouse-move-event` | `mouse-motion-msg` |
| `mouse-scroll-event` | `mouse-wheel-msg` |
| `focus-in-msg` / `focus-out-msg` | `focus-msg` / `blur-msg` |
| `make-program :alt-screen t` | `make-view content :alt-screen t` |

See `doc/V2-MIGRATION.md` for the full migration guide.

## New Dependencies

- `alexandria` — Common utilities
- `serapeum` — Extended utilities
- `cl-base64` — Base64 encoding for clipboard support

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
