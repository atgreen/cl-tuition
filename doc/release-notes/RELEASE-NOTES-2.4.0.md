# tuition 2.4.0 Release Notes

I'm pleased to announce tuition 2.4.0, a feature release of the Common Lisp library for building terminal user interfaces. This release ports another batch of recent `charmbracelet/bubbles` and `lipgloss` work — headlined by an interactive tree component and textarea selection — and fixes a family of OSC 8 hyperlink layout bugs.

## What's New

### Interactive tree component (bubbles #893)

A new `tuition.components.tree` package (nickname `tui.tree`) provides a navigable tree, complementing the static renderer in `tuition.render.tree`:

- **Nodes** — `make-node` builds a tree of values and children (plain values become leaves), with per-node open/closed state (`node-open` / `node-close` / `node-toggle`) and hidden subtrees (`node-set-hidden`).
- **Navigation** — `↓/j/Ctrl-n` and `↑/k/Ctrl-p` move the selection, `PgDn/Space/f` and `PgUp/b` page, `d`/`u` half-page, `g`/`Home` and `G`/`End` jump, `Enter` toggles, `→/l` opens, and `←/h` closes the selected node.
- **Rendering** — branch guides, `▼`/`▶` open/closed indicators, a `→` cursor column, multi-line node values, and viewport windowing with a configurable scroll-off margin. Styles for the selected node, parents, leaves, root, guides, and cursor are all configurable.

### Textarea selection (bubbles #1029)

The textarea now supports selecting text:

- **Keyboard** — `Shift`-modified movement extends a selection (`Shift-←/→/↑/↓`, and `Ctrl-Shift-←/→` by word), `Ctrl-g` selects all, and `Ctrl-Shift-c` copies the selection to the system clipboard via OSC 52.
- **Pointer** — `textarea-begin-selection` / `textarea-extend-selection` / `textarea-end-selection` track a mouse drag, with `textarea-position-at` mapping view coordinates (wide-character aware, past the prompt/line-number gutter) to buffer positions.
- **Editing semantics** — typing, pasting, `Enter`, and the delete commands replace the active selection; plain movement clears it.
- **Reading it back** — `textarea-selection`, `textarea-selected-text`, `textarea-has-selection-p`, and `textarea-delete-selection`. The highlight style defaults to reverse video and is configurable via `:selection-style`.

### Table fit-content mode (lipgloss #697)

`tuition.render.table:make-table` accepts `:fit-content`. Combined with `:width`, the width acts as a maximum: the table renders at its natural content width unless that exceeds the maximum, in which case columns are shrunk to fit as before.

### Textarea word motion crosses lines (bubbles #1036)

`textarea-cursor-word-backward` / `-forward` now cross line boundaries like upstream — at the start of a line, word-backward jumps to the last word of the previous line — and stop cleanly at the very start and end of the input. Word deletion remains line-local, matching upstream.

### Clipboard and terminal-query commands are now wired

`set-clipboard-cmd`, `set-primary-clipboard-cmd`, `read-clipboard-cmd`, `request-foreground-color-cmd`, `request-background-color-cmd`, `request-cursor-color-cmd`, and `raw-cmd` were previously inert stubs. They now emit their OSC 52 / OSC 10/11/12 sequences through a new internal `write-escape-msg`, which the program loop writes on the render thread so raw escapes never interleave with a frame.

## Bug Fixes

### OSC 8 hyperlinks no longer break layout

Escape-sequence scanning is now OSC-aware everywhere, via a shared helper: `visible-length`, text wrapping/truncation tokenization, the overlay compositor, the cells parser, and `zone-scan` all skip OSC payloads instead of counting them as visible columns. Hyperlinked text now sizes boxes, composites modals, and registers mouse zones correctly. In addition, the cells parser now records OSC 8 URIs into `cell-link` (honoring both ST and BEL terminators), so hyperlinks survive a round-trip through the cell-based compositor.

### keybindings-help honors :separator

`keybindings-help` ignored its `:separator` argument — the format string consumed list entries as separators, jamming entries together. Entries are now joined with the requested separator.

### truncate-text no longer truncates text that fits

`(truncate-text "hello" 5)` returned `"hell…"`, reserving room for the ellipsis even when nothing needed truncating. Text that already fits within the width is now returned unchanged.

### Windows console support is actually loaded

`src/windows-console.lisp` was never referenced by `tuition.asd`; it is now loaded on Windows via `:if-feature :win32`.

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
