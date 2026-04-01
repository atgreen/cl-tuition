# tuition 2.1.0 Release Notes

I'm pleased to announce tuition 2.1.0, a feature and bugfix release of the Common Lisp library for building terminal user interfaces.

## What's New

### RGB-to-Palette Conversion and Color Auto-Downmapping

Tuition now automatically maps truecolor values to the best available palette entry when the terminal doesn't support 24-bit color:

- `rgb-to-ansi256` / `rgb-to-ansi16` find the nearest palette color using weighted Euclidean (redmean) color distance
- `hex-to-ansi256` / `hex-to-ansi16` / `ansi256-to-hex` convenience converters
- `complete-color` objects now auto-populate their `ansi256` and `ansi` slots from a truecolor hex value, so a single color definition works across all terminal capabilities
- `resolve-color-foreground` / `resolve-color-background` return ready-to-use escape sequences

### Modernized Markdown Renderer

The built-in markdown renderer now uses `complete-color` objects instead of 16-color keywords, enabling richer theme support on modern terminals while remaining backwards-compatible on older ones. Use `make-markdown-style-from-colors` to build theme-driven markdown styling from your own palette.

## Bug Fixes

### `batch` and `cmd-sequence` no longer silently drop commands

A misuse of `serapeum:keep` caused `batch` and `cmd-sequence` to always return nil, silently dropping every init command, tick timer, and update batch. This has been fixed. (#17)

### 256-color escape codes now generated correctly

`as-foreground-code` / `as-background-code` previously only handled truecolor and 16-color values. They now correctly emit `38;5;N` / `48;5;N` sequences for 256-color palette entries.

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
