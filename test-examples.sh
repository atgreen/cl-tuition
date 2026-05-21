#!/bin/bash
# Test that all examples load without immediate errors

set -e

EXAMPLES_DIR="examples"
FAILED=()
PASSED=()

echo "Testing all examples load correctly..."
echo "======================================"
echo

for example in "$EXAMPLES_DIR"/*.lisp; do
    filename=$(basename "$example")
    echo -n "Testing $filename... "

    # Run with timeout in a PTY using script. The -e flag makes script
    # propagate the child's exit code (without it, script always exits 0
    # and every example looks like it passed). --disable-debugger makes SBCL
    # exit non-zero on unhandled errors instead of entering the debugger.
    run_output=$(mktemp)
    exit_code=0
    # The outer { ... } 2>/dev/null swallows the "Killed" job-status line bash
    # emits when timeout's --kill-after fires; sbcl's own stderr is already
    # captured via 2>&1 above.
    { timeout --kill-after=2 4 script -e -q -c "sbcl --disable-debugger --load \"$example\"" /dev/null > "$run_output" 2>&1 || exit_code=$?; } 2>/dev/null
    if [ $exit_code -eq 0 ]; then
        # Loaded and exited normally (non-interactive examples)
        echo "✓ PASSED"
        PASSED+=("$filename")
        rm -f "$run_output"
    elif [ $exit_code -eq 124 ] || [ $exit_code -eq 137 ]; then
        # Killed by timeout - expected for TUI examples that run indefinitely
        echo "✓ PASSED (running)"
        PASSED+=("$filename")
        rm -f "$run_output"
    else
        echo "✗ FAILED (exit $exit_code)"
        FAILED+=("$filename")
        echo "  Error output:"
        grep -B 1 -A 3 -iE "unhandled|debugger invoked|fatal" "$run_output" | head -15 | sed 's/^/    /'
        echo
        rm -f "$run_output"
    fi
done

echo
echo "======================================"
echo "Results:"
echo "  Passed: ${#PASSED[@]}"
echo "  Failed: ${#FAILED[@]}"

if [ ${#FAILED[@]} -gt 0 ]; then
    echo
    echo "Failed examples:"
    for f in "${FAILED[@]}"; do
        echo "  - $f"
    done
    exit 1
fi

echo
echo "All examples loaded successfully!"
