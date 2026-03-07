# tuition 1.0.1 Release Notes

I'm pleased to announce tuition 1.0.1, a patch release of the Common Lisp library for building terminal user interfaces.

## What's New

### Initial Release

Tuition brings The Elm Architecture (TEA) to Common Lisp terminal applications with a clean, functional approach to building interactive TUIs. This release includes:

**Core Architecture:**
- TEA-style Model-View-Update pattern using CLOS
- Message-based event system with generic method dispatch
- Concurrent command system for non-blocking I/O and timers
- Pure rendering approach with composable utilities

**Terminal Control:**
- Raw terminal mode with proper cleanup and error handling
- Alternate screen buffer support
- Cursor control and screen clearing
- Cross-platform terminal handling

**Input Handling:**
- Comprehensive keyboard input decoding with modifiers (Ctrl, Alt)
- Full mouse support with hierarchical event system:
  - Mouse press, release, drag, move, and scroll events
  - Cell-based and all-motion tracking modes
  - Button and modifier detection
- Special key support (arrows, function keys, etc.)

**Text Styling:**
- ANSI color support (8 standard + bright variants)
- Text attributes (bold, italic, underline)
- Adaptive color system for theme-aware UIs
- Composable style objects

**Layout System:**
- Horizontal and vertical layout composition with alignment
- Flexible placement and positioning helpers
- Width/height constraints
- Terminal-aware sizing

**Borders:**
- Multiple border styles (rounded, thick, double, ASCII, markdown, block)
- Configurable titles and padding
- Border rendering with proper corner handling

**Text Reflow:**
- Word wrapping with indentation support
- Text truncation with ellipsis
- Multi-line indentation
- ANSI-aware width calculation

**Components:**
Reusable UI components with encapsulated state:
- Spinner with multiple animation styles
- Progress bar with percentage and gradient fills
- List view with selection and filtering
- Table with headers and row formatting
- Text input with cursor, placeholder, and validation
- Viewport for scrollable content
- Text area for multi-line editing
- Paginator for page navigation
- Timer and stopwatch with formatting options
- Help component for keybinding display

**Zones (Mouse Areas):**
- Named region tracking for click/hover detection
- Zone manager for reliable pointer-to-identifier mapping
- Support for overlapping and nested zones

**Examples:**
Comprehensive example programs demonstrating:
- Hello world and counter basics
- File manager with navigation
- Spinner animations
- Progress bar demos
- And many more in the `examples/` directory

**Testing:**
- Basic test suite
- Automated example verification via GitHub Actions
- Test scripts for development workflow

## Installation

### Via ocicl
```bash
ocicl install tuition
```

### Via Quicklisp (when available)
```lisp
(ql:quickload :tuition)
```

### From Source
```bash
git clone https://github.com/atgreen/tuition.git
cd tuition
# Load in your Lisp environment
```

## Quick Start

```lisp
(defpackage #:hello-world
  (:use #:cl #:tuition))

(in-package #:hello-world)

(defclass hello-model () ())

(defmethod tui:init ((model hello-model))
  nil)

(defmethod tui:update-message ((model hello-model) (msg tui:key-msg))
  (declare (ignore msg))
  (values model (tui:quit-cmd)))

(defmethod tui:view ((model hello-model))
  (tui:bold "Hello, World! Press any key to exit."))

(defun main ()
  (tui:run (tui:make-program (make-instance 'hello-model))))
```

## Dependencies

- bordeaux-threads — cross-platform threading
- trivial-channels — thread-safe message passing
- version-string — version comparison utilities
- alexandria — utility library
- serapeum — utility library

## Breaking Changes

None. This is the initial release.

## Known Limitations

- Terminal detection is basic; assumes modern terminal emulator
- Some terminal emulators may have incomplete mouse support
- Windows support is best-effort through bordeaux-threads

## Future Roadmap

Planned features for upcoming releases:
- Additional components (menus, dialogs, tabs)
- Enhanced markdown rendering
- Animation utilities using spring physics
- Performance optimizations for large renders
- More comprehensive terminal capability detection

## Contributors

Tuition was created by Anthony Green, with inspiration from the Charmbracelet ecosystem (Bubble Tea, Lip Gloss, Bubbles, Harmonica).

---

For more information, visit the [tuition repository](https://github.com/atgreen/tuition) or read the [README](https://github.com/atgreen/tuition/blob/master/README.md).
