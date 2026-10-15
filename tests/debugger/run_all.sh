#!/bin/sh
#
# Run the full debugger regression suite (the top-level debug_test.sh smoke test
# plus every assertion harness under tests/debugger/). Exits non-zero if any fail.

set -eu

ROOT_DIR=$(cd -- "$(dirname -- "$0")/../.." && pwd)
HERE=$(cd -- "$(dirname -- "$0")" && pwd)

status=0

run() {
    name="$1"
    if sh "$name" >/tmp/petta_debug_suite.out 2>&1; then
        printf 'PASS %s\n' "$name"
    else
        printf 'FAIL %s\n' "$name"
        cat /tmp/petta_debug_suite.out
        status=1
    fi
}

run "$ROOT_DIR/debug_test.sh"
for t in "$HERE"/*.sh; do
    [ "$t" = "$HERE/run_all.sh" ] && continue
    run "$t"
done

if [ "$status" -eq 0 ]; then
    printf 'All debugger suites passed.\n'
fi
exit "$status"
