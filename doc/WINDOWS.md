# Windows Support

Tuition now includes experimental Windows support through the Windows Console API.

## Status

**Current support level: Experimental**

### What Works

- Terminal raw mode (via SetConsoleMode)
- ANSI escape sequences for styling and layout
- Terminal size detection (via GetConsoleScreenBufferInfo)
- Keyboard input handling
- Mouse input (when enabled in program options)
- All core TUI features (styling, borders, layout, components)

### What Doesn't Work Yet

- **Terminal resize detection**: Windows doesn't send SIGWINCH signals. Programs won't automatically respond to terminal resizes. Workaround: restart the program after resizing.
- **Suspend/Resume (Ctrl+Z)**: Windows doesn't have SIGTSTP/SIGCONT signals. These features are Unix-only.

## Requirements

- **Common Lisp Implementation**: SBCL (tested on Windows)
- **Terminal**: Windows Terminal, PowerShell, or any terminal with ANSI support
- **OS**: Windows 10 version 1511 or later (for Virtual Terminal Sequences support)

## Installation

### Via ocicl on Windows

1. Install SBCL for Windows from https://www.sbcl.org/platform-table.html
2. Install ocicl from https://github.com/ocicl/ocicl
3. Install Tuition:

```cmd
ocicl install tuition
```

### From Source

```cmd
git clone https://github.com/atgreen/tuition.git
cd tuition
```

Then load in your Lisp environment.

## Running Examples

All examples should work on Windows. Try the hello-world example:

```cmd
sbcl --eval "(asdf:load-system :tuition)" --eval "(load \"examples/hello-world.lisp\")" --eval "(hello-world::main)"
```

Or in the REPL:

```lisp
(asdf:load-system :tuition)
(load "examples/hello-world.lisp")
(hello-world::main)
```

## Terminal Recommendations

**Best Experience**: Use **Windows Terminal** (available from Microsoft Store or GitHub)
- Full ANSI escape sequence support
- Unicode support
- Good performance
- Configurable colors and fonts

**Also Works**:
- PowerShell 5.1+ / PowerShell Core 7+
- Command Prompt (Windows 10+)

**Not Recommended**:
- cmd.exe on Windows 7/8 (lacks virtual terminal support)
- Very old PowerShell versions

## Known Issues

1. **Console Resize**: Programs don't detect terminal resize events. If you resize the terminal window, you'll need to restart the application.

2. **Focus Events**: May not work reliably on all Windows terminals.

3. **Some Special Keys**: Certain key combinations might be intercepted by Windows or the terminal emulator.

## Implementation Details

Windows support is implemented using FFI bindings to the Windows Console API:

- `GetStdHandle` - Get console handles
- `GetConsoleMode` / `SetConsoleMode` - Control console modes
- `GetConsoleScreenBufferInfo` - Query terminal size

Key files:
- `src/windows-console.lisp` - Windows Console API bindings (lines 11-175)
- `src/terminal.lisp` - Platform-agnostic terminal control (with `#+win32` conditionals)
- `src/program.lisp` - Event loop (signal handlers are Unix-only at lines 77-156)

## Future Improvements

Planned enhancements for Windows support:

- [ ] Console resize event detection via `ReadConsoleInput` with `WINDOW_BUFFER_SIZE_EVENT`
- [ ] Better input handling for Windows-specific key codes
- [ ] Testing on more Windows terminal emulators
- [ ] CI/CD testing on Windows
- [ ] Performance optimizations

## Reporting Issues

If you encounter issues on Windows, please report them at:
https://github.com/atgreen/tuition/issues

Include:
- Windows version
- Terminal emulator (Windows Terminal, PowerShell, cmd.exe, etc.)
- SBCL version
- Error messages or unexpected behavior
- Minimal reproduction case

## Contributing

Windows support contributions are welcome! Areas that need work:

1. Resize event detection
2. More robust input handling
3. Testing on different Windows versions and terminals
4. Documentation improvements
5. Example programs demonstrating Windows-specific considerations

## License

Windows support code is licensed under the same MIT license as the rest of Tuition.
