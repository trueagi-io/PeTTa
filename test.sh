#!/bin/sh
# Run all MeTTa examples through PeTTa integration tests.
#
# The full suite runs each example via the release CLI with per-file progress.
# Failures in one example don't affect others.
#
# Usage: sh test.sh
# Or:    cargo test --test examples -- --ignored --nocapture

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)

echo "Building release binary..."
cd "$SCRIPT_DIR"
RUSTFLAGS="-C target-cpu=native" cargo build --release 2>&1 || exit 1

echo ""
echo "Running MeTTa example tests (full suite)..."
echo "============================================="

# Run the full ignored test suite
RUSTFLAGS="-C target-cpu=native" cargo test --test examples -- --ignored --nocapture 2>&1
status=$?

if [ $status -eq 0 ]; then
    echo ""
    echo "All example tests passed."
else
    echo ""
    echo "Some examples failed (see above)."
fi

exit $status
