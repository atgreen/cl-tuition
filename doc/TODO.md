Tuition TODO — Make It Feel Idiomatic to Common Lisp

Goals
- Favor CLOS, conditions/restarts, and keyword arguments over Go-style patterns.
- Make the API REPL-friendly, extensible, and composable.
- Tighten packaging, examples, docs, and tests to match Lisp ecosystems.

API and Naming
- DONE: Replace option-sentinel functions with keyword arguments (helpers removed).
  - Example: `(make-program model :alt-screen t :mouse :cell-motion)`.
- Keep `tea` nickname; ensure exported symbols are hyphenated, descriptive, and consistent.
- DONE: Provide a thin `defprogram` macro to define model class + `init`/`update`/`view` methods succinctly for common cases.

Messages and Dispatch
- DONE: Switch to CLOS message classes with `defmessage` macro.
  - Built-ins: `key-msg`, `mouse-msg`, `quit-msg`, `window-size-msg`.
  - Keep constructor funcs and `*-p` predicates for convenience.
- DONE: Add `update-message` generic for message-dispatch; `update` forwards to it by default.

Commands and Concurrency
- Keep `(values model cmd)` return — Lisp-idiomatic multiple values.
- TODO: Add a `defer`/`after` utility using `trivial-timeout` instead of `(sleep)` in threads for timer-based commands.
- TODO: Provide a scheduler hook so users can plug custom dispatch (e.g., coalescing, priority queues).
- TODO: Add a non-blocking run mode and a “step/pump” API for embedding into existing event loops.

Terminal and I/O
- DONE: Wrap raw-mode lifecycle in a macro: `with-raw-terminal` used by `run` and available to users.
- DONE: Add restarts for terminal setup (use-no-raw, retry, abort); signal `terminal-operation-error` on failures.
- TODO: Avoid global `*terminal-io*` dependence where possible; pass streams via program/init options; keep dynamic vars as defaults.
- TODO: Improve Windows and non-SBCL portability; add feature-conditional backends or fallbacks (SBCL, CCL, ECL, ABCL).
- TODO: Optionally integrate with `cl-charms` as a backend (make it optional in ASDF via secondary system, not a hard dependency).

Rendering
- TODO: Avoid full-screen clear on unchanged regions; add a diff renderer (later optimization) with configurable strategy: full, line-diff, smart-diff.
- TODO: Add a pluggable renderer protocol (generic function `render-model` over renderer subclass), keep current default simple renderer.

Errors, Conditions, and Restarts
- DONE: Define library condition types (`tuition-error`, `terminal-error`, `terminal-operation-error`, `input-error`).
- DONE: Use `restart-case` in `with-raw-terminal` for terminal setup (continue without raw, retry, abort).
- DONE: Provide `*error-handler*` hook and route loop errors through it.

Styling, Layout, and Zones
- Offer a small DSL for composition in views (optional macros) to reduce string munging when building TUI trees.
- Ensure layout/border/style all accept keyword args consistently and return plain strings; avoid side effects.
- Make Zones API more discoverable: add a simple high-level helper for common hover/click use-cases.

REPL Ergonomics
- DONE: `run` uses `with-raw-terminal` for robust cleanup; restarts available.
- DONE: Add `(stop program)` and cooperative `join` (uses `bt:join-thread` internally), no `bt:destroy-thread`.
- TODO: Add a debug flag to echo raw input and render timing; route logs to `*trace-output*` or user-supplied function.

Packaging and Systems
- TODO: Split ASDF systems into optional modules to reduce heavy deps for core usage.
  - `tuition/core` (no cl-charms, minimal deps) — terminal via ANSI, input via POSIX.
  - `tuition/charm-backend` (optional cl-charms).
  - `tuition/components` (spinner, textinput, list, table) — only if files exist or add them.
- DONE: Align `README.md` and `tuition.asd` with actual files (remove nonexistent modules; minimal deps).
- TODO: Add Quicklisp install instructions alongside ocicl.

Documentation and Examples
- Ensure examples in README exist; add minimal, self-contained examples:
  - `examples/simple.lisp` (countdown) — UPDATED to use `defprogram`.
  - `examples/counter.lisp` (interactive) — UPDATED to use `update-message`.
  - Mouse + zones example using the exported API (pending keyword-options update is done).
- TODO: Add a short “From Bubble Tea to Lisp” migration guide for mental model translation.
- TODO: Expand STYLING.md, LAYOUT, and ZONES docs with small end-to-end snippets.

Testing
- TODO: Add FiveAM tests for:
  - `update`/`update-message` protocol contracts (values and dispatch).
  - Input parsing (keys, modifiers, mouse sequences).
  - Renderer behavior (no redraw on equal string, clears on change).
  - Terminal lifecycle with restarts (simulate `enter-raw-mode` failure, exercise `USE-NO-RAW`).
- TODO: Provide a smoke test that runs a headless “program pump” to verify basic event loop behavior w/o a real terminal.

Performance and Correctness
- TODO: Ensure Unicode input handling with `flexi-streams` when necessary; document recommended stream setup.
- TODO: Consider bounded mailboxes or back-pressure strategy for `msg-channel` to avoid unbounded growth.
- DONE: Cooperative shutdown of input thread; avoid `bt:destroy-thread`.

Backwards Compatibility and Stability
- We intentionally removed compatibility shims (no external users yet). Favor clarity over legacy.
- TODO: Version and changelog updates when changing public exports.

Nice-to-Haves (Later)
- A small component set (spinner, progress bar, text input, list, table) implemented idiomatically with CLOS and pure view functions.
- A layout combinator library with constraints or grid primitives that produce strings.
- A template/rendering bridge (e.g., mustache) for simple static views.

Immediate Action Items
- DONE: Align ASDF to present files; pare down deps.
- DONE: Replace option-sentinel functions with keyword args (helpers removed).
- DONE: Add `with-raw-terminal` and use it in `run` with restarts.
- DONE: Switch input thread to cooperative shutdown and join.
- DONE: Introduce condition types and route errors via `*error-handler*`.
- TODO: Convert remaining examples to `update-message`/`defprogram` pattern as desired.
- TODO: Add a minimal FiveAM test suite and CI entry point.
