#!/bin/sh
set -e

# Check if SWI-Prolog is installed
if ! command -v swipl >/dev/null 2>&1; then
    echo "Error: swipl (SWI-Prolog) is not installed."
    echo "Install it: https://www.swi-prolog.org/Download.html"
    exit 1
fi

# Clone mork_ffi if not present (required for MORK backend)
if [ ! -d "mork_ffi" ]; then
    echo "Cloning mork_ffi (MORK FFI bindings)..."
    git clone --depth 1 https://github.com/patham9/mork_ffi.git
fi

echo "Building petta (Rust)..."
cargo build --release

echo "Build successful."
