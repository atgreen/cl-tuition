# tuition 1.0.3 Release Notes

I'm pleased to announce tuition 1.0.3, a patch release of the Common Lisp library for building terminal user interfaces.

## What's New

### Windows Support

- **Multi-platform CI**: Added GitHub Actions workflows for Windows alongside Linux and macOS.
- **Path handling fixes**: Converted backslashes to forward slashes in paths for cross-platform compatibility.
- **CI improvements**: Fixed ASDF loading and source registry configuration for reliable builds across all platforms.

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
