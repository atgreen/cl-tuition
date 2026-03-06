# Tuition v2 Migration Guide

This document covers migrating from Tuition v1 to v2. Tuition v2 is a clean break
with no backward-compatible aliases.

## Key Changes Overview

1. **Cell-based rendering** — Only changed cells are redrawn (like ncurses)
2. **Advanced compositing** — Layer/Compositor/Canvas system for z-ordered compositing
3. **Higher-fidelity input** — Kitty keyboard protocol, key press/release/repeat, unified modifier bitmask
4. **Declarative view** — `view` returns a `view-state` struct that controls terminal modes

## Key Message Changes

### v1
```lisp
(defmethod tui:update-message ((model my-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((tui:key-msg-ctrl msg)
       ;; Ctrl+C
       (when (char= key #\c) ...))
      ((char= key #\q) ...)
      ((eq key :up) ...))))
```

### v2
```lisp
(defmethod tui:update-message ((model my-model) (msg tui:key-press-msg))
  (let ((key (tui:key-event-code msg))
        (mod (tui:key-event-mod msg)))
    (cond
      ((tui:mod-contains mod tui:+mod-ctrl+)
       ;; Ctrl+C
       (when (and (characterp key) (char= key #\c)) ...))
      ((and (characterp key) (char= key #\q)) ...)
      ((eq key :up) ...))))
```

### Symbol mapping

| v1 | v2 |
|----|-----|
| `key-msg` | `key-press-msg` |
| `key-msg-p` | `key-press-msg-p` |
| `make-key-msg` | `make-key-press-msg` |
| `key-msg-key` | `key-event-code` |
| `key-msg-ctrl` | `(mod-contains (key-event-mod msg) +mod-ctrl+)` |
| `key-msg-alt` | `(mod-contains (key-event-mod msg) +mod-alt+)` |
| — | `key-release-msg` (new) |
| — | `key-event-mod` (bitmask) |
| — | `key-event-text` (new) |
| — | `key-event-shifted-code` (new) |
| — | `key-event-base-code` (new) |
| — | `key-event-repeat-p` (new) |

### Modifier bitmask constants

```lisp
tui:+mod-shift+     tui:+mod-alt+      tui:+mod-ctrl+
tui:+mod-meta+      tui:+mod-hyper+    tui:+mod-super+
tui:+mod-caps-lock+ tui:+mod-num-lock+
```

Construct with `(tui:make-mod :shift t :ctrl t)`, test with `(tui:mod-contains mod tui:+mod-ctrl+)`.

## Mouse Event Changes

### v1
```lisp
(defmethod tui:update-message ((model my-model) (msg tui:mouse-press-event))
  (let ((x (tui:mouse-event-x msg))
        (y (tui:mouse-event-y msg))
        (ctrl (tui:mouse-event-ctrl msg)))
    ...))
```

### v2
```lisp
(defmethod tui:update-message ((model my-model) (msg tui:mouse-click-msg))
  (let ((x (tui:mouse-event-x msg))
        (y (tui:mouse-event-y msg))
        (ctrl (tui:mod-contains (tui:mouse-event-mod msg) tui:+mod-ctrl+)))
    ...))
```

### Symbol mapping

| v1 | v2 |
|----|-----|
| `mouse-press-event` | `mouse-click-msg` |
| `mouse-release-event` | `mouse-release-msg` |
| `mouse-drag-event` | `mouse-motion-msg` |
| `mouse-move-event` | `mouse-motion-msg` |
| `mouse-scroll-event` | `mouse-wheel-msg` |
| `mouse-scroll-event-p` | `mouse-wheel-msg-p` |
| `mouse-scroll-direction` | `mouse-wheel-direction` |
| `mouse-event-shift` | `(mod-contains (mouse-event-mod msg) +mod-shift+)` |
| `mouse-event-alt` | `(mod-contains (mouse-event-mod msg) +mod-alt+)` |
| `mouse-event-ctrl` | `(mod-contains (mouse-event-mod msg) +mod-ctrl+)` |

Note: `mouse-event-x`, `mouse-event-y`, and `mouse-event-button` are unchanged.

## Focus Event Changes

| v1 | v2 |
|----|-----|
| `focus-in-msg` | `focus-msg` |
| `focus-out-msg` | `blur-msg` |

## Declarative View

### v1
Terminal modes were set at program creation time:
```lisp
(tui:make-program model :alt-screen t :mouse :cell-motion)
```

The `view` method returned a plain string:
```lisp
(defmethod tui:view ((model my-model))
  (format nil "Count: ~D~%Press q to quit." (counter model)))
```

### v2
`make-program` no longer accepts `:alt-screen` or `:mouse`. Terminal modes are
controlled declaratively through the `view-state` returned by `view`:

```lisp
(tui:make-program model)
```

```lisp
(defmethod tui:view ((model my-model))
  (tui:make-view
   (format nil "Count: ~D~%Press q to quit." (counter model))
   :alt-screen t
   :mouse-mode :cell-motion))
```

### view-state options

| Keyword | Type | Description |
|---------|------|-------------|
| `:alt-screen` | boolean | Use alternate screen buffer |
| `:mouse-mode` | `:cell-motion` / `:all-motion` / nil | Mouse tracking mode |
| `:report-focus` | boolean | Report focus/blur events |
| `:window-title` | string or nil | Terminal window title |
| `:cursor` | cursor or nil | Cursor position/shape (nil = hidden) |
| `:foreground-color` | color or nil | Terminal foreground color |
| `:background-color` | color or nil | Terminal background color |
| `:keyboard-enhancements` | plist | Kitty keyboard protocol settings |
| `:on-mouse` | function or nil | Mouse event interceptor |

## Cell-Based Rendering

v2 uses a cell-based rendering engine that diffs screen buffers and only redraws
changed cells. This is automatic — existing code benefits without changes.

Key types:
- `cell` — A single terminal cell (char, width, fg, bg, attrs, link)
- `screen-buffer` — A 2D grid of cells
- `parse-styled-string` — Parse ANSI string into a screen-buffer
- `render-diff` — Emit minimal ANSI sequences between two buffers

Attribute bitmask constants:
```lisp
tui:+attr-bold+  tui:+attr-italic+  tui:+attr-underline+
tui:+attr-blink+ tui:+attr-reverse+ tui:+attr-strikethrough+
tui:+attr-faint+
```

## Compositing System

New layer-based compositing for complex UIs:

```lisp
;; Create layers
(let* ((header (tui:make-layer (tui:render-styled "Header" header-style)))
       (body   (tui:make-layer body-content))
       (footer (tui:make-layer "Press q to quit")))
  ;; Position them
  (tui:layer-set-y body 2)
  (tui:layer-set-y footer 20)
  ;; Create compositor and render
  (let ((comp (tui:make-compositor header body footer)))
    (tui:compositor-render comp)))
```

Hit testing for mouse events:
```lisp
(multiple-value-bind (id layer)
    (tui:compositor-hit comp mouse-x mouse-y)
  (when id
    (format t "Clicked on: ~A" id)))
```

## Style Enhancements

### Extended underlines
```lisp
(tui:make-style :underline :curly)   ; curly underline
(tui:make-style :underline :double)  ; double underline
(tui:make-style :underline :dotted)  ; dotted underline
(tui:make-style :underline :dashed)  ; dashed underline
(tui:make-style :underline-color "#ff0000")  ; colored underline
```

### Hyperlinks
```lisp
(tui:make-style :hyperlink "https://example.com")
```

### Color utilities
```lisp
(tui:darken-color "#ff0000" 0.3)        ; darken by 30%
(tui:lighten-color "#ff0000" 0.3)       ; lighten by 30%
(tui:complementary-color "#ff0000")     ; 180° hue rotation
(tui:blend-colors "#ff0000" "#0000ff" 0.5) ; 50% blend
```

### Border gradients
```lisp
(tui:render-border content tui:*border-rounded*
                   :fg-colors '("#ff0000" "#00ff00" "#0000ff"))
```

### Custom fill characters
```lisp
(tui:make-style :padding-char #\. :margin-char #\~)
```

## Kitty Keyboard Protocol

v2 supports the Kitty keyboard protocol for higher-fidelity input. Terminals that
support it (kitty, foot, WezTerm, etc.) will report:
- Key press, release, and repeat events separately
- Full Unicode codepoints
- Disambiguated modifier keys

Enable via view-state:
```lisp
(tui:make-view content :keyboard-enhancements '(:disambiguate t))
```

## Terminal Utilities

New cursor control:
```lisp
(tui:set-cursor-shape :bar)    ; :block, :underline, :bar
(tui:set-cursor-color "#ff0000")
```

Color queries:
```lisp
;; These return commands that produce messages:
(tui:request-background-color-cmd)
(tui:request-foreground-color-cmd)
(tui:request-cursor-color-cmd)
```

## Clipboard Support (OSC 52)

v2 adds clipboard support via the OSC 52 protocol:

```lisp
;; Set system clipboard
(tui:set-clipboard "text to copy")

;; Read clipboard (terminal responds with clipboard-msg)
(tui:read-clipboard)

;; Set primary selection (X11)
(tui:set-primary-clipboard "text")

;; Command versions for use in update:
(tui:set-clipboard-cmd "text")
(tui:read-clipboard-cmd)
```

Handle clipboard responses:
```lisp
(defmethod tui:update-message ((model my-model) (msg tui:clipboard-msg))
  (let ((content (tui:clipboard-msg-content msg)))
    ;; process clipboard content
    (values model nil)))
```

## Bracketed Paste Control

v2 adds `disable-bracketed-paste` to `view-state`:
```lisp
(tui:make-view content :disable-bracketed-paste t)
```

New paste boundary messages:
| Symbol | Description |
|--------|-------------|
| `paste-start-msg` | Bracketed paste sequence started |
| `paste-end-msg` | Bracketed paste sequence ended |

## Unicode Mode (DEC Private Mode 2027)

Enable Unicode text segmentation for correct character width measurement:
```lisp
(tui:make-view content :unicode-mode t)
```

Direct control:
```lisp
(tui:enable-unicode-mode)
(tui:disable-unicode-mode)
```

## Terminal Queries

```lisp
;; Query terminal version (XTVERSION)
(tui:request-terminal-version)
;; Response: terminal-version-msg with (terminal-version-msg-version msg)

;; Query cursor color
(tui:request-cursor-color)
;; Response: cursor-color-msg with (cursor-color-msg-color msg)

;; DEC Private Mode Report (DECRPM)
(tui:request-mode-report 2027)  ; query Unicode mode
;; Response: mode-report-msg with mode and setting slots
;; setting: 0=not recognized, 1=set, 2=reset, 3=permanently set, 4=permanently reset
```

## Raw Escape Command

Send arbitrary escape sequences:
```lisp
(tui:raw-cmd "\e[?1049h")  ; enter alt screen directly
```

## Key Event Utilities

`keystroke` returns a human-readable string for a key event:
```lisp
(tui:keystroke (tui:make-key-press-msg :code #\c :mod tui:+mod-ctrl+))
;; => "ctrl+c"

(tui:keystroke (tui:make-key-press-msg :code :up :mod (tui:make-mod :shift t :alt t)))
;; => "alt+shift+up"
```

## Light/Dark Helper

Choose values based on terminal background luminance:
```lisp
(tui:light-dark light-value dark-value)           ; auto-detect
(tui:light-dark light-value dark-value "#1a1a1a") ; explicit bg color
```
