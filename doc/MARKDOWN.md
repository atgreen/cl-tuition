# Markdown Rendering

Tuition includes a markdown renderer inspired by [Glamour](https://github.com/charmbracelet/glamour), allowing you to render beautiful markdown documents in your terminal applications.

## Features

- **Headers** (H1, H2, H3)
- **Bold** and *italic* text
- `Inline code` with styling
- Code blocks with background
- Unordered and ordered lists
- Block quotes
- Links (rendered with color/underline)
- Horizontal rules
- Multiple built-in themes

## Quick Start

```lisp
(tui:render-markdown "# Hello World

This is **bold** and this is *italic*.

- Item 1
- Item 2
- Item 3" :style :dark)
```

## Basic Usage

The simplest way to render markdown is with the `render-markdown` function:

```lisp
(tui:render-markdown text &key (style :dark) (width 80))
```

### Parameters

- `text` - Markdown source string
- `style` - Theme to use (`:dark`, `:light`, `:pink`, `:ascii`, or a custom `markdown-style` struct)
- `width` - Maximum line width for wrapping (default: 80)

### Example

```lisp
(let ((md "## Welcome

This is a **markdown** document with `code` and *emphasis*.

> This is a quote

And here's a list:

1. First item
2. Second item
3. Third item"))
  (format t "~A~%" (tui:render-markdown md :style :dark :width 70)))
```

## Built-in Styles

Tuition comes with four built-in themes:

### Dark Theme (`:dark`)

The default theme, optimized for dark terminal backgrounds:
- Bright cyan headers
- Yellow inline code
- Blue links
- High contrast colors

```lisp
(tui:render-markdown text :style :dark)
```

### Light Theme (`:light`)

Optimized for light terminal backgrounds:
- Darker colors for headers
- Magenta code blocks
- Subdued quote styling

```lisp
(tui:render-markdown text :style :light)
```

### Pink Theme (`:pink`)

A colorful, vibrant theme:
- Magenta headers
- Cyan code
- Pink links

```lisp
(tui:render-markdown text :style :pink)
```

### ASCII Theme (`:ascii`)

No colors, plain ASCII characters only:
- Great for non-color terminals
- Uses `*` instead of `•` for bullets
- Uses `-` for horizontal rules

```lisp
(tui:render-markdown text :style :ascii)
```

## Custom Styles

You can create custom styles using `make-markdown-style`:

```lisp
(let ((custom-style (tui:make-markdown-style
                     :h1-color :bright-red
                     :h2-color :red
                     :code-color :green
                     :link-color :yellow
                     :list-bullet "→ ")))
  (tui:render-markdown text :style custom-style))
```

### Style Options

Headers:
- `:h1-prefix`, `:h1-color`, `:h1-bold`
- `:h2-prefix`, `:h2-color`, `:h2-bold`
- `:h3-prefix`, `:h3-color`, `:h3-bold`

Text:
- `:bold-color` - Color for bold text
- `:italic-color` - Color for italic text
- `:code-color`, `:code-bg` - Inline code styling
- `:emph-color` - Emphasis/italic styling
- `:strong-color` - Strong/bold styling

Code Blocks:
- `:code-block-color` - Foreground color
- `:code-block-bg` - Background color
- `:code-block-margin` - Left margin in spaces

Lists:
- `:list-bullet` - Bullet character for unordered lists
- `:list-indent` - Indentation in spaces
- `:ordered-bullet-format` - Format string for ordered lists

Quotes:
- `:quote-prefix` - Prefix character(s)
- `:quote-color` - Color for quotes
- `:quote-indent` - Indentation

Links:
- `:link-color` - Color for links
- `:link-underline` - Whether to underline links

Other:
- `:hr-char` - Character for horizontal rules
- `:document-margin` - Document margins

## Supported Markdown Features

### Headers

```markdown
# H1 Header
## H2 Header
### H3 Header
```

### Emphasis

```markdown
This is **bold** text.
This is *italic* text.
```

### Code

Inline code:
```markdown
Use the `format` function.
```

Code blocks:
````markdown
```lisp
(defun hello ()
  (format t "Hello!"))
```
````

### Lists

Unordered:
```markdown
- Item 1
- Item 2
- Item 3
```

Ordered:
```markdown
1. First
2. Second
3. Third
```

### Quotes

```markdown
> This is a quote
> spanning multiple lines
```

### Links

```markdown
Check out [Common Lisp](https://common-lisp.net)!
```

### Horizontal Rules

```markdown
---
```

or

```markdown
***
```

## Advanced Usage

### Using the Renderer Object

For more control, create a renderer instance:

```lisp
(let ((renderer (tui:make-markdown-renderer :style :dark :width 60)))
  ;; Renderer can be reused
  (tui:render-markdown text1 :style (tui:renderer-style renderer))
  (tui:render-markdown text2 :style (tui:renderer-style renderer)))
```

### Creating Style Functions

You can create helper functions for your custom styles:

```lisp
(defun my-markdown-style ()
  (tui:make-markdown-style
   :h1-color :bright-green
   :code-color :cyan
   :link-color :bright-blue))

(defun render-my-markdown (text)
  (tui:render-markdown text :style (my-markdown-style)))
```

## Integration with Components

Markdown rendering works great with other Tuition components:

### With Viewport

```lisp
(let ((viewport (tui:make-viewport :width 80 :height 24))
      (markdown-content (tui:render-markdown my-long-document)))
  (tui:viewport-set-content viewport markdown-content)
  ;; Now you can scroll through the rendered markdown
  (tui:viewport-view viewport))
```

### With Borders

```lisp
(let ((content (tui:render-markdown "# Title\n\nContent here.")))
  (tui:render-border content
                     :title "Documentation"
                     :border tui:*border-rounded*))
```

## Example Application

See `examples/markdown.lisp` for a complete interactive markdown viewer that demonstrates:
- Rendering markdown documents
- Switching between themes dynamically
- Interactive key handling

Run it with:
```lisp
(asdf:load-system :tuition)
(load "examples/markdown.lisp")
(tuition-example-markdown:main)
```

## Convenience Function

For quick rendering, use the shorthand `markdown` function:

```lisp
(tui:markdown "# Quick Test")
;; Equivalent to:
(tui:render-markdown "# Quick Test" :style :dark :width 80)
```

## Performance Notes

- Inline parsing happens on every render
- For static content, render once and cache the result
- Large documents benefit from using a viewport component
- Code blocks are rendered with minimal overhead

## Limitations

Current limitations (may be addressed in future versions):

- No syntax highlighting in code blocks
- No table support
- No task lists
- No HTML passthrough
- No automatic link detection (must use markdown link syntax)
- Headers only support H1-H3

## Future Enhancements

Planned features:

- Syntax highlighting for code blocks (using colorize library)
- Table rendering
- More header levels (H4-H6)
- Custom element renderers
- Markdown parsing to AST for custom rendering
- GFM (GitHub Flavored Markdown) extensions

## Comparison with Glamour

Feature parity with Glamour:

| Feature | Glamour | Tuition |
|---------|---------|---------|
| Headers | ✓ | ✓ (H1-H3) |
| Bold/Italic | ✓ | ✓ |
| Code | ✓ | ✓ |
| Code blocks | ✓ | ✓ (no highlighting) |
| Lists | ✓ | ✓ |
| Quotes | ✓ | ✓ |
| Links | ✓ | ✓ |
| Tables | ✓ | ✗ |
| Syntax highlighting | ✓ | ✗ |
| Custom styles | ✓ | ✓ |
| Multiple themes | ✓ | ✓ |

## Contributing

If you'd like to add features to the markdown renderer:

1. Extend the parser in `markdown.lisp`
2. Add style options to `markdown-style` struct
3. Update built-in styles
4. Add tests and examples
5. Update this documentation
