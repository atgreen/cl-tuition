# tuition 2.5.0 Release Notes

I'm pleased to announce tuition 2.5.0, a feature release of the Common Lisp library for building terminal user interfaces. This release adds a file picker, completing tuition's coverage of the `charmbracelet/bubbles` component set.

## What's New

### Filepicker component

A new `tuition.components.filepicker` package (nickname `tui.filepicker`) ports the bubbles filepicker: navigate the filesystem and pick a file.

- **Async directory reads** — `filepicker-init` (and navigating into a directory) returns a command that lists the directory off the UI thread and delivers the result as a message; stale listings from other picker instances are ignored.
- **Listing** — entries are sorted directories-first then by name, with `ls`-style permission strings and humanized sizes (via `sb-posix` on SBCL/unix, gracefully omitted elsewhere), symlink targets shown as `name → target`, and dotfiles hidden unless `:show-hidden`.
- **Navigation** — `j`/`k`/arrows/`Ctrl-n`/`Ctrl-p` move, `J`/`K`/`PgDn`/`PgUp` page, `g`/`G` jump, `Enter`/`→`/`l` opens a directory or selects a file, and `Backspace`/`←`/`h`/`Esc` returns to the parent, restoring the previous cursor and scroll position.
- **Selection rules** — `:allowed-types` restricts selectable file suffixes (excluded files render dimmed and refuse selection), and `:file-allowed` / `:dir-allowed` control what `Enter` may pick. Query results with `filepicker-did-select-file`, `filepicker-did-select-disabled-file`, and `filepicker-highlighted-path`.
- **Viewport** — an explicit `:height`, or `:auto-height` (the default) tracking window-size messages. When no height has been set, one entry is shown rather than a blank view (bubbles #1026).

A runnable demo ships as `examples/filepicker.lisp`:

```bash
sbcl --load examples/filepicker.lisp
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
