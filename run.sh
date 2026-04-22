#!/bin/sh

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)

# If already built, run the release binary directly
if [ -f "$SCRIPT_DIR/target/release/petta" ]; then
    exec "$SCRIPT_DIR/target/release/petta" "$@"
fi

# Otherwise build and run
exec cargo run --quiet -- "$@"
