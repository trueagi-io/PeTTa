#!/bin/sh
#
# M3 regression: breakpoints must surface MeTTa $variable bindings by name, not
# just positional args/result. The parse-time variable environment is threaded
# through translation (current_compile_vars, b_setval to keep variable identity)
# and baked into each compiled-call wrapper as Name-Var pairs, then printed as
# "var $Name = Value" at breakpoints. See source_expr_bindings (src/translator.pl)
# and print_compiled_vars (src/debugger.pl).

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

# fib_buggy recurses with (fib (- $N 3)); breaking when the argument goes
# negative happens with $N = 2, so the breakpoint must report "var $N = 2".
out=$(run_metta fib_buggy.metta --debug-break-if='fib:arg1<0' --debug-break-once --debug-goal=fib --silent)

printf '%s\n' "$out" | grep -F "source expr: (fib (- 2 3))" >/dev/null \
    || fail "expected source expr (fib (- 2 3)) at the breakpoint"

printf '%s\n' "$out" | grep -F "var \$N = 2" >/dev/null \
    || fail "breakpoint did not report MeTTa variable binding 'var \$N = 2'"

printf 'Debugger variable-inspection (M3) tests passed.\n'
