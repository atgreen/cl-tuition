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

    # Run with timeout in a PTY (using script), capture output
    # Timeout after 2 seconds - we just want to see if it loads/starts
    if timeout 2 script -q -c "sbcl --load \"$example\"" /dev/null >/dev/null 2>&1; then
        # Timeout means it ran (normal for TUI apps)
        echo "✓ PASSED (loaded and started)"
        PASSED+=("$filename")
    else
        exit_code=$?
        if [ $exit_code -eq 124 ]; then
            # Exit code 124 is timeout - this is expected for running TUIs
            echo "✓ PASSED (loaded and started)"
            PASSED+=("$filename")
        else
            # Real failure (compilation error, runtime error before entering TUI, etc.)
            echo "✗ FAILED (exit code: $exit_code)"
            FAILED+=("$filename")

            # Show error details
            echo "  Error output:"
            timeout 2 script -q -c "sbcl --load \"$example\"" /dev/null 2>&1 | grep -A 5 -i "error\|warning\|undefined" | head -20 | sed 's/^/    /'
            echo
        fi
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
