#!/bin/sh
#
# M1 regression: compiled runtime/breakpoint events must report the real source
# form Index and Line of the originating MeTTa form, not the old hard-coded
# "#compiled line 0". The form Index/Line are threaded from process_form
# (src/filereader.pl) through wrap_runtime_call (src/translator.pl) into
# trace_compiled_goal (src/metta.pl) via the current_compile_form/2 fact.

set -eu

ROOT_DIR=$(cd -- "$(dirname -- "$0")/../.." && pwd)
MAIN="$ROOT_DIR/src/main.pl"

strip_ansi() {
    sed 's/\x1b\[[0-9;]*m//g'
}

run_metta() {
    file="$1"
    shift
    path="$ROOT_DIR/examples/$file"
    # deliberately-broken debugger fixtures live outside examples/ so the
    # example suites do not try to run them
    [ -f "$path" ] || path="$ROOT_DIR/tests/debugger/fixtures/$file"
    swipl -q -s "$MAIN" -- "$path" "$@" 2>&1 | strip_ansi
}

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    exit 1
}

# tests/debugger/fixtures/fib_buggy.metta: the function clause is form #1 on line 1; the
# !(test (fib 10) ...) runnable is form #2 on line 7. Recursive fib calls come
# from the clause (line 1); the top-level call comes from the runnable (line 7).
# (fib_buggy is small/fast and exercises the same compiled mapping as fib.metta.)
trace=$(run_metta fib_buggy.metta --debug=runtime --debug-goal=fib --debug-max-events=40 --silent)

printf '%s\n' "$trace" | grep -F "line 0" >/dev/null \
    && fail "compiled events still report 'line 0' (M1 source mapping not applied)"

printf '%s\n' "$trace" | grep -F "#2 line 7 compiled" >/dev/null \
    || fail "top-level (fib 30) call not mapped to form #2 line 7"

printf '%s\n' "$trace" | grep -F "#1 line 1 compiled" >/dev/null \
    || fail "recursive fib calls not mapped to clause form #1 line 1"

printf 'Debugger source-mapping (M1) tests passed.\n'
