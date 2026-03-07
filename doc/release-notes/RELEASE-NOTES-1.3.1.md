# tuition 1.3.1 Release Notes

I'm pleased to announce tuition 1.3.1, a patch release of the Common Lisp library for building terminal user interfaces.

## What's New

### TUI Rendering Flicker Fix

- **In-place overwrite rendering**: Replaced `clear-screen` with in-place overwrite to eliminate visible flicker during TUI redraws.
- **Direct /dev/tty I/O**: Opened `/dev/tty` directly for all TUI I/O operations, bypassing `*terminal-io*` indirection for more reliable terminal access.
- **Robust stream unwrapping**: Fixed `stream-fd` to recursively unwrap nested streams, ensuring the correct file descriptor is always resolved.

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
