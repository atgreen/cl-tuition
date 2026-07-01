# tuition 2.2.0 Release Notes

I'm pleased to announce tuition 2.2.0, a feature release of the Common Lisp library for building terminal user interfaces. This release ports a batch of recent `charmbracelet/bubbles` and `lipgloss` features to tuition, and fixes a long-standing color-resolution bug.

## What's New

### Textarea overhaul

The textarea component gained a large set of editing and rendering features, ported from recent bubbles textarea work:

- **Scrolling and cursor visibility** — a vertical scroll offset keeps the cursor on screen once content exceeds the viewport (previously it walked off the bottom). Added `page-up` / `page-down` and `scroll-position` / `scroll-percent` accessors.
- **Editing commands** — document-level `move-to-begin` / `move-to-end`, word navigation (`Alt-b` / `Alt-f`), word deletion (`Ctrl-w`, `Alt-Backspace`, `Alt-d`), `transpose-chars` (`Ctrl-T`), word-case (`Alt-c` / `Alt-l` / `Alt-u`), and a `word` accessor for the word under the cursor.
- **Bracketed paste** support.
- **`char-limit` and `max-lines` are now enforced** on insert (the slots existed but were previously ignored).
- **Opt-in soft-wrapping** (`:soft-wrap t`) — long lines wrap to `width` display columns, with cursor and scroll tracking moved to visual lines.
- **Opt-in dynamic height** (`:dynamic-height t`) — the viewport grows and shrinks to fit the content, clamped between `:min-height` and `:max-height`.

Soft-wrap and dynamic-height default off, so existing behavior is unchanged.

### Progress bar gradients (#838)

`make-progress` now accepts `:colors` (a list of hex color stops) and `:empty-color`:

- omitted / `nil` — plain fill (the default)
- a single hex string — solid fill
- two or more hex strings — a gradient blend across the filled portion

The blend is distributed evenly between stops, ported from bubbles progress #838.

### Tree indenter styling (#446)

`make-tree` accepts `:indenter-style` to style the continuation/indentation guides (the `│` characters) separately from the branch connectors, and `:indent` now applies a left margin to the whole tree.

### Table cell wrapping (#620)

`tuition.render.table:make-table` accepts `:widths`. When set narrower than a cell's content, cells soft-wrap and a row's height grows to its tallest wrapped cell (with shorter cells blank-padded). Default width calculation is unchanged.

## Bug Fixes

### `resolve-color` now resolves hex color strings

Passing a bare hex color such as `"#FF0000"` to `make-style :foreground` (or any color slot) previously produced an invalid escape sequence (`ESC[#FF0000m`). Hex strings are now resolved to a real SGR code — truecolor, 256-color, or 16-color depending on terminal capability — matching how `complete-color` objects are handled. Already-resolved SGR codes (`"31"`, `"38;2;1;2;3"`) still pass through unchanged.

### Textarea backspace no longer crashes after `set-value`

`textarea-set-value` built a plain vector, but the delete-merge paths require an adjustable fill-pointer vector, so backspacing across a line break after `set-value` always errored. It now builds the expected vector type.

### Tree nested-line indent accounting

The nested-line drop calculation used `length` on a prefix that can carry ANSI styling, miscounting columns. It now uses `visible-length`.

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
