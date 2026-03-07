# tuition 1.1.0 Release Notes

I'm pleased to announce tuition 1.1.0, a feature release of the Common Lisp library for building terminal user interfaces.

## What's New

### Date Picker Component

A new interactive calendar component for date selection, inspired by [ethanefung/bubble-datepicker](https://github.com/ethanefung/bubble-datepicker).

**Features:**
- Interactive calendar grid with month/year header
- Keyboard navigation with arrow keys and vim-style h/j/k/l
- Month navigation with `[` and `]` keys
- Year navigation with `{` and `}` keys
- Date selection with Enter/Space, clear with Escape
- Jump to today with Home key
- Customizable styles via `make-datepicker-styles`
- DST-safe date arithmetic (no off-by-one errors across daylight saving boundaries)

**Usage:**
```lisp
(use-package :tui.datepicker)

;; Create a date picker
(make-datepicker)

;; With custom styles
(make-datepicker
  :styles (make-datepicker-styles
           :today (tui:make-style :foreground tui:*fg-green* :bold t)
           :selected (tui:make-style :reverse t)))
```

**Example:**
```bash
sbcl --load examples/datepicker.lisp
```

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
