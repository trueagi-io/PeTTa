#!/bin/sh
set -e

# Check if SWI-Prolog is installed
if ! command -v swipl >/dev/null 2>&1; then
    echo "Error: swipl (SWI-Prolog) is not installed."
    echo "Install it: https://www.swi-prolog.org/Download.html"
    exit 1
fi

echo "Building petta (Rust)..."
cargo build --release

echo "Build successful."
