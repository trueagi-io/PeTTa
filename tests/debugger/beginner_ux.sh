#!/bin/sh
#
# Beginner-usability regression: the on-ramp features added for newcomers.
#  1. `sh debug.sh watch <file> <fn>` produces a compact, depth-indented trace
#     (via --debug-indent) with no per-event header or stack dump.
#  2. A mistyped --debug category warns and suggests the closest valid one
#     (instead of silently doing nothing).
#  3. An unrecognized --debug-* flag warns.
#  4. A missing file reports a friendly message and exits non-zero (no raw
#     Prolog stack error).
#  5. --debug flags with no file reports "no .metta file given".

set -eu

ROOT_DIR=$(cd -- "$(dirname -- "$0")/../.." && pwd)
MAIN="$ROOT_DIR/src/main.pl"

strip_ansi() {
    sed 's/\x1b\[[0-9;]*m//g'
}

run_metta() {
    swipl -q -s "$MAIN" -- "$@" 2>&1 | strip_ansi
}

# Like run_metta but preserves swipl's exit code (a pipeline would report sed's).
# Writes captured output to $RUN_OUT and returns swipl's status.
run_metta_rc() {
    swipl -q -s "$MAIN" -- "$@" >/tmp/petta_ux.out 2>&1
    rc=$?
    RUN_OUT=$(strip_ansi </tmp/petta_ux.out)
    return $rc
}

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    exit 1
}

# 1. watch shorthand: compact indented trace.
watch=$(sh "$ROOT_DIR/debug.sh" watch "$ROOT_DIR/tests/debugger/fixtures/fib_buggy.metta" fib 2>&1 | strip_ansi)

printf '%s\n' "$watch" | grep -E '^ENTER  \(fib 10\)' >/dev/null \
    || fail "watch did not show the top-level ENTER (fib 10) at column 0"
printf '%s\n' "$watch" | grep -E '^  ENTER  \(fib 9\)' >/dev/null \
    || fail "watch did not indent the nested (fib 9) call"
printf '%s\n' "$watch" | grep -F '[DEBUG' >/dev/null \
    && fail "watch (compact) should not print [DEBUG ...] headers"
printf '%s\n' "$watch" | grep -F 'stack:' >/dev/null \
    && fail "watch (compact) should not print per-event 'stack:' lines"

# 2. mistyped category warns + suggests.
cat=$(run_metta "$ROOT_DIR/examples/fib.metta" --debug=runtim)
printf '%s\n' "$cat" | grep -F "unknown debug category 'runtim'" >/dev/null \
    || fail "mistyped --debug category did not warn"
printf '%s\n' "$cat" | grep -F "Did you mean 'runtime'" >/dev/null \
    || fail "mistyped --debug category did not suggest 'runtime'"

# 3. unrecognized --debug-* flag warns.
flag=$(run_metta "$ROOT_DIR/examples/fib.metta" --debug-brk=fib)
printf '%s\n' "$flag" | grep -F "unknown debug option '--debug-brk=fib'" >/dev/null \
    || fail "unrecognized --debug-* flag did not warn"

# 4. missing file: friendly message + non-zero exit, no raw Prolog error.
rc=0
run_metta_rc "$ROOT_DIR/examples/does_not_exist.metta" --debug=runtime || rc=$?
[ "$rc" -ne 0 ] || fail "missing file should exit non-zero"
printf '%s\n' "$RUN_OUT" | grep -F "file not found:" >/dev/null \
    || fail "missing file did not give a friendly message"
printf '%s\n' "$RUN_OUT" | grep -F "source_sink" >/dev/null \
    && fail "missing file still leaked the raw Prolog source_sink error"

# 5. debug flags but no file.
rc=0
run_metta_rc --debug=runtime || rc=$?
[ "$rc" -ne 0 ] || fail "debug flags with no file should exit non-zero"
printf '%s\n' "$RUN_OUT" | grep -F "no .metta file given" >/dev/null \
    || fail "debug flags with no file did not report a missing file"

printf 'Debugger beginner-usability tests passed.\n'
