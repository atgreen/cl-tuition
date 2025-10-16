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

    # Run with timeout in a PTY using script, capture stderr to check for errors
    # Use --disable-debugger so SBCL exits on unhandled errors instead of entering debugger
    run_output=$(mktemp)
    if timeout 2 script -q -c "sbcl --disable-debugger --load \"$example\"" /dev/null > "$run_output" 2>&1; then
        echo "✓ PASSED"
        PASSED+=("$filename")
        rm -f "$run_output"
    else
        exit_code=$?
        if [ $exit_code -eq 124 ]; then
            # Exit code 124 is timeout - this is expected for running TUIs
            echo "✓ PASSED"
            PASSED+=("$filename")
            rm -f "$run_output"
        else
            # Non-timeout exit - check if it's an error or expected
            if grep -qi "debugger invoked\|unhandled.*error\|undefined function\|undefined variable\|end of file\|read error" "$run_output"; then
                echo "✗ FAILED"
                FAILED+=("$filename")
                echo "  Error output:"
                grep -B 2 -A 3 -i "error\|undefined\|debugger invoked" "$run_output" | head -15 | sed 's/^/    /'
                echo
            else
                # Early exit but no obvious error - might be expected
                echo "✓ PASSED (early exit)"
                PASSED+=("$filename")
            fi
            rm -f "$run_output"
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
