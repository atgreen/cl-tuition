# tuition 2.6.0 Release Notes

I'm pleased to announce tuition 2.6.0, a feature release of the Common Lisp library for building terminal user interfaces. This release adds an optional single-threaded runtime, broader Windows support, and a print-above-the-view facility, alongside a batch of rendering and styling improvements adapted from upstream `charmbracelet`.

## What's New

### Optional single-threaded runtime

`bordeaux-threads` and `trivial-channels` are now **optional** dependencies. Build with the `:tuition-single-threaded` feature and tuition runs on a cooperative, single-threaded event loop — input polling, message processing, and command execution in one thread, with no threading library required. The default threaded runtime is unchanged. This makes tuition usable on Lisp implementations and deployments where threads are unavailable or unwanted.

### Windows terminal support

tuition now runs on Windows beyond SBCL and delivers resize events there:

- **CFFI console bindings** — `windows-console.lisp` was ported from `sb-alien` to CFFI, so tuition loads on non-SBCL Windows implementations (e.g. dotcl). `cffi` is a dependency on Windows only.
- **Resize events** — when stdin is a console, the input loop reads it with `ReadConsoleInputW`, so a `WINDOW_BUFFER_SIZE_EVENT` arrives as a `window-size-msg`. With `ENABLE_VIRTUAL_TERMINAL_INPUT`, keys, arrows, mouse and bracketed paste come through the existing escape-sequence parser; surrogate pairs are rejoined, and input that arrives split across reads is reassembled.
- **Initial window size** — the first `window-size-msg` is now sent on Windows and on non-SBCL implementations, not only on SBCL/Unix, so a program learns the terminal size before the first resize.

### Print above the live view

A new print-above-view facility lets a program emit permanent lines above its live, redrawing view — they scroll up into the terminal's scrollback while the view stays put. This mirrors bubbletea's `tea.Println` / `tea.Printf`.

- `println` and `printf` commands, and the `program-println` / `program-printf` helpers, queue a line to print above the view.
- Inline (non-alt-screen) views now render at their **content height** rather than filling the terminal, so the rows below the view are left untouched and printed lines flow into scrollback. Alt-screen rendering is unchanged.
- A runnable demo ships as `examples/print-above.lisp`:

  ```bash
  cd examples && sbcl --load print-above.lisp
  ```

### Styling and color

- **`is-dark-color`** — classifies a color as dark using the CIELAB L\* lightness (threshold 50) instead of HSL, so perceptually dark hues like pure blue are correctly treated as dark.
- **Multi-rune borders** — border top and bottom edges now tile multi-character, wide-glyph, and zero-width border segments across the exact display width, instead of repeating only the first character.
- **Alphabetic list enumerator** — `alphabet-enumerator` uses bijective base-26 (`A.` … `Z.`, `AA.`, `AB.` …), so lists longer than 26 items are numbered correctly.

## Fixes

- **Inline style mode** now joins wrapped lines with a space rather than dropping every line after the first, matching lipgloss `Inline` semantics.
- **`wrap-text`** preserves intentional runs of spaces when `:normalize-spaces` is nil, so width-constrained content keeps its padding — for example a right-bordered column rendered through a width'd style now keeps its border aligned.
- **`get-terminal-size`** is hardened: it clamps implausible dimensions, falls back to the environment and then to 80×24, and always returns a `(cols . rows)` cons.

## Thanks

This release would not have happened without the community. My thanks to:

- **Robert Burnett** ([@VisenDev](https://github.com/VisenDev)) — the optional single-threaded runtime, which makes `bordeaux-threads` optional and adds the cooperative event loop.
- **SANO, Masatoshi** ([@snmsts](https://github.com/snmsts)) — Windows terminal support: the CFFI console bindings, console input records, and resize events.

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
