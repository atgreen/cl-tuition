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

### External Program Execution

`exec-cmd` runs external programs (editors, pagers, etc.) with full TUI suspension and proper terminal restoration.

**Features:**
- Pauses input reading to prevent keystrokes leaking to/from the subprocess
- Suspends signal handlers during execution
- Properly suspends/resumes terminal state (alt-screen, raw mode, mouse reporting)
- Optional callback to process results after the program exits
- Sends `window-size-msg` after resume to trigger redraw

**Usage:**
```lisp
;; Open a file in the user's editor
(exec-cmd "vim" '("myfile.txt")
          :callback (lambda (exit-code)
                      (make-instance 'file-saved-msg)))

;; Access from within update methods
*current-program*          ; bound during event loop
(program-input-paused p)   ; pause/resume input reading
```

### Batched Message Processing

The event loop now processes input more efficiently by reading all available events atomically and rendering once per batch instead of once per message. This significantly improves performance during rapid input (fast typing, mouse drags, scroll bursts).

### Scroll Event Coalescing

Consecutive scroll events in the same direction are now combined into a single event with a `scroll-count` field, preventing erratic jumping during fast scrolling.

```lisp
(defmethod tui:update-message ((model my-app) (msg tui:mouse-scroll-event))
  (let ((direction (tui:mouse-scroll-direction msg))
        (count (tui:mouse-scroll-count msg)))  ; number of coalesced events
    ...))
```

### Extended Key Support

New key symbols for additional keyboard input:
- `:page-up`, `:page-down`, `:insert`
- Modified arrow keys with Shift/Alt/Ctrl combinations
- `:shift-up`, `:shift-down`, `:shift-left`, `:shift-right`, `:shift-home`, `:shift-end`
- Alt and Ctrl modifiers set via `:alt` and `:ctrl` flags in `key-msg`

### Terminal Title

`set-terminal-title` sets the terminal window/tab title using the standard OSC escape sequence. Works in xterm, gnome-terminal, iTerm2, Windows Terminal, and most modern emulators.

```lisp
(tui:set-terminal-title "My Application")
```

## Bug Fixes

- Fixed parsing of modified arrow key sequences (Shift/Ctrl/Alt+arrows) which were previously unrecognized

## Documentation

- New `doc/BORDERS.md` covers borders, title bars, opaque shadows, and transparent shadow compositing
- README updated with exec-cmd, batched processing, scroll coalescing, shadow, and title bar sections

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
