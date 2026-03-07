# tuition 1.0.2 Release Notes

I'm pleased to announce tuition 1.0.2, a patch release of the Common Lisp library for building terminal user interfaces.

## What's New

### Markdown Rendering Improvements

- **Fixed ANSI escape sequence corruption**: The `:bold` and `:italic` style handlers now correctly return `nil` instead of `t` in `resolve-markdown-color`, preventing malformed escape sequences that caused 'm' prefix artifacts on styled words.
- **Added table rendering**: Markdown tables are now supported with Unicode box-drawing characters for proper borders and alignment.
- **Inline styles in table cells**: Bold, italic, and code styles are now processed within table cells.
- **Proper column alignment**: Uses `visible-length` for correct column width calculation when ANSI codes are present.
- **Smart quote handling**: Support for backtick-like Unicode characters for inline code (handles smart quotes from AI CLI tools).
- **Additional markdown fixes**: Various improvements to markdown rendering reliability.

### Terminal Handling

- **Disabled input processing**: Improved terminal input handling with disabled input processing for better raw mode support.

### Build System

- **CI switched to ocicl**: Continuous integration now uses ocicl instead of Quicklisp for faster and more reliable builds.

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

## Contributors

Thanks to erjanmx for fixing a typo in the README.

---

For more information, visit the [tuition repository](https://github.com/atgreen/tuition) or read the [README](https://github.com/atgreen/tuition/blob/master/README.md).
