# Tuition Project Review

This document provides a focused review of the Tuition project from a Common Lisp perspective, covering architecture, style, alignment with Lisp ideals, usability, completeness, organization, and maintainability.

## Architecture

- Clear TEA organization: protocol (messages, init/update/view), terminal/runtime, input decoding, renderer, styling/layout, and components live in sensible files. Good modularity for a TUI toolkit.
- CLOS-first event handling is idiomatic: `update-message` specialized by message class keeps application code clean and extensible.
- Concurrency model (`bordeaux-threads` + `trivial-channels`) fits CL well; “commands” returning messages asynchronously is a nice match to TEA.
- Terminal lifecycle via `with-raw-terminal`, SBCL signal handling (SIGWINCH/SIGTSTP/SIGCONT), alt-screen/mouse/paste toggling: solid and thoughtful.
- Rendering is intentionally simple: full-screen re-render when output changes. It trades performance for correctness and simplicity, which is fine for v0.1.

## Lisp Ideals

- Embraces CLOS and conditions. The use of restarts in `with-raw-terminal` is a great Lisp touch.
- Packages and exports are explicit, readable, and documented in `package.lisp`.
- Internal/private helpers are conventionally named (leading `%`) and scoped well.
- Macros are used sparingly and purposefully (`defprogram`).
- Good docstrings and SPDX headers throughout.

## Style and Code Quality

- Consistent naming, structure, and inline documentation. Functions compose small, clear string operations without cleverness.
- ANSI handling: `visible-length` aims to be robust (ESC stripping, combining marks, East Asian width). That’s above-average attention to correctness for TUIs.
- Layout helpers read cleanly and take alignment constants consistently; `style.lisp` collects text shaping logically.

## Notable Issues and Risks

1. Name collision: `resolve-color` is defined in `src/style.lisp` and redefined in `src/markdown.lisp` (same package). The markdown redefinition will overwrite the more capable version for the entire package, which can cause incorrect color resolution elsewhere.
   - High-priority fix: rename the markdown helper (e.g., `md-resolve-fg`/`md-resolve-bg`) or make it local; do not redefine `tui:resolve-color`.

2. Duplicate reflow implementation: reflow logic exists both in `src/style.lisp` and `src/reflow.lisp`.
   - Only one should exist. Either remove `src/reflow.lisp` (if `style.lisp` is authoritative) or split “reflow” to its own file and export from there, updating the ASDF system accordingly. Duplication will drift and confuse users.

3. Renderer stream consistency: `renderer:render` writes `view-string` to its `output-stream`, but `clear-screen`/`move-cursor-home` use the default stream via `format t`.
   - That breaks output redirection/capture. Pass the renderer’s output stream to those helpers or have terminal ops accept a stream.

4. Input debug logging is enabled by default and writes to `/tmp` on every key path.
   - That will degrade performance and produce files unexpectedly. Default to disabled; enable via a documented toggle.

5. ASDF test-op points to a missing system: `tuition.asd` declares `:in-order-to ((test-op (test-op "tuition/tests")))` but no `tuition/tests` exists.
   - Running `test-op` will fail. Provide a trivial test system or remove the clause for now.

6. Examples and docs drift:
   - `examples/keybindings.lisp` uses `(colored …)` with positional args instead of `:fg`/`:bg` keywords. That call will error. Fix to `(colored "text" :fg …)`.
   - `src/components/README.md` refers to `tea.*` package nicknames and `tea:` symbols, but actual nicknames are `tui.*` and the exported nickname for the root is `tui`. Update the doc to match code (or add `tea.*` nicknames consistently).

7. Repository hygiene:
   - Compiled artifacts and caches are checked in (`.cache/`, `lisp/sbcl-…/`, `examples/*.fasl`). Add a `.gitignore` and purge committed FASLs and caches. They severely hurt maintainability and diff signal.
   - `ocicl/` vendor directories are present. If this repo is the “source” library, don’t vendor downloaded dependencies; document ocicl usage (already done) and keep the repo clean.

8. Portability: terminal raw mode is SBCL-only; `get-terminal-size` shells out to `stty` (Unix assumption).
   - README recommends SBCL; that’s acceptable, but you can detect `TIOCGWINSZ` in SBCL to avoid spawning `stty`, and add clear errors/guards on other lisps.

9. Zones: good design, but require explicit `zone-scan` wrapping at the top-level view.
   - That’s fine but be explicit in README: “Call `zone-scan` once on the final composed view.”

10. Minor logic issues:
    - `renderer`: `clear-screen` already moves cursor home; calling `move-cursor-home` immediately after is redundant.
    - `layout`: `join-horizontal` calculates empty fill width using `block-width` (first line) which may not reflect max width of multi-line blocks. If you rely on “first line width” as fill width it will misalign blocks with multi-line content. Consider using per-block max line width.

## Usability and API

- README is comprehensive and approachable; quick starts are helpful.
- Exports are thoughtfully curated; keywords and plists keep APIs flexible.
- Components use their own packages and nicknames (`tui.spinner`, `tui.progress`, etc.) which is the right choice for clarity and isolation.
- Keybinding layer is pragmatic and readable; help text generation is handy.
- Markdown renderer is a nice bonus; its internal wrapping differs from the core `wrap-text` API (and reimplements splitting). It works, but consolidating reflow primitives would reduce redundancy.

## Completeness

- Feature coverage is impressive for an 0.1 release: TEA loop, input (keys/mouse/paste), raw terminal lifecycle, styling, layout, borders, reflow, zones, markdown, table/list/tree helpers, and several components (spinner, progress, textinput, textarea, paginator, viewport, stopwatch, timer, help).
- Enhancement roadmap is realistic and well organized.
- Tests are minimal (manual test files) and there’s no automated test system. Even a tiny regression suite for `visible-length`, `wrap-text`, input parsing, and borders would pay dividends.

## Organization

- Files are logically grouped under `src/` with small, single-responsibility units.
- Components live under `src/components` with clear per-component packages.
- Examples are plentiful and runnable — great for adoption.
- The `ocicl/` and cache folders in the repo are out of place; fix via `.gitignore` and repo cleanup.

## Maintainability

- Code is easy to read and reason about; functions have narrow purposes and docstrings.
- Biggest risks are name conflicts across files within the same package (as seen in markdown) and duplicated logic (reflow).
- Splitting `style.lisp` into clearer submodules (colors, style, measurement, reflow) could help long-term but isn’t urgent. Do it after resolving duplication and naming.

## High-Impact Fixes (suggested order)

1. Fix `resolve-color` naming conflict in `markdown.lisp` (rename, make local).
2. Remove one of the reflow implementations; keep a single source and update ASDF.
3. Default input logging to off; provide enable/disable functions (already present).
4. Make terminal ops in renderer write to renderer’s `output-stream`.
5. Add a `.gitignore`; remove FASLs/.cache/lisp-sbcl artifacts from the repo.
6. Provide a minimal `tuition/tests` ASDF system (even just loads and sanity asserts).
7. Fix examples (`colored` call syntax) and update `components/README.md` package names to `tui.*`.

## Nice-to-Haves

- Use ioctl (`TIOCGWINSZ`) for window size on SBCL/Unix instead of `stty`.
- Add a simple FPS cap in the renderer to avoid excessive redraws with fast message traffic.
- Consider integrating `zone-scan` into renderer via an option to avoid user footguns.
- Expand input sequences coverage (function keys, modifiers) and unit tests for parsing.

## Summary

Tuition is thoughtfully designed, idiomatic Common Lisp, and already quite capable. It balances clarity and practicality: CLOS-driven TEA, solid terminal lifecycle, and a useful set of components and helpers. The main things to address are a few correctness bugs (name collision in markdown, example API mismatches), repository hygiene, and removing duplicated code. With those fixed, this is a strong foundation for a CL TUI toolkit.

