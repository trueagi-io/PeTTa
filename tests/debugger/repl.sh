#!/bin/sh
#
# M5 regression: the interactive breakpoint REPL primitives.
#   - debug_repl_eval/1 evaluates a MeTTa expression in the current context,
#     including referencing the breakpoint's $variables by name, with tracing
#     suspended so it does not recurse into the debugger.
#   - debug_repl_add_break/1, debug_repl_clear_breaks/0, debug_repl_show_breaks/0
#     manage breakpoints live.
# The prompt itself is TTY-only, so we drive the underlying predicates directly.

set -eu

ROOT_DIR=$(cd -- "$(dirname -- "$0")/../.." && pwd)
PLFILE=$(mktemp /tmp/petta_repl_test_XXXXXX.pl)
trap 'rm -f "$PLFILE"' EXIT

# Single-quoted heredoc: keep $x / $N literal (no shell expansion).
cat > "$PLFILE" <<'EOF'
:- ensure_loaded('src/metta').
:- initialization(main).
main :-
    process_metta_string("(= (double $x) (* $x 2))", _),
    debug_repl_eval("(+ 1 2)"),
    debug_repl_eval("(double 21)"),
    % Simulate a breakpoint context binding $N = 5, then evaluate using $N.
    assertz(debug_break_context(enter,
        meta(1, 1, compiled([fib, 5], ['N'-5])), compiled, fib(5, _))),
    debug_repl_eval("(+ $N 100)"),
    % Suspension: a runtime break is armed, but evaluating must not trace/break.
    assertz(debug_break_target(double)),
    debug_repl_eval("(double 4)"),
    % Live breakpoint management.
    debug_repl_clear_breaks,
    debug_repl_add_break("fib"),
    debug_repl_add_break("fib:arg1<0"),
    debug_repl_show_breaks,
    debug_repl_clear_breaks,
    debug_repl_show_breaks,
    halt.
EOF

OUT=$(cd "$ROOT_DIR" && swipl -q "$PLFILE" 2>&1 | sed 's/\x1b\[[0-9;]*m//g')

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    printf '%s\n' "$OUT" >&2
    exit 1
}

printf '%s\n' "$OUT" | grep -F "= 3"   >/dev/null || fail "eval (+ 1 2) should be 3"
printf '%s\n' "$OUT" | grep -F "= 42"  >/dev/null || fail "eval (double 21) should be 42"
printf '%s\n' "$OUT" | grep -F "= 105" >/dev/null || fail "eval (+ \$N 100) with \$N=5 should be 105"
printf '%s\n' "$OUT" | grep -F "= 8"   >/dev/null || fail "eval (double 4) should be 8"

# Suspension: evaluating under an armed 'double' breakpoint must NOT produce any
# trace/breakpoint output, since tracing is suspended during REPL evaluation.
printf '%s\n' "$OUT" | grep -qF "[BREAKPOINT" \
    && fail "REPL eval leaked breakpoint output (suspension failed)"
printf '%s\n' "$OUT" | grep -qE "^ *ENTER " \
    && fail "REPL eval leaked trace output (suspension failed)"

# Breakpoint management.
printf '%s\n' "$OUT" | grep -F "added breakpoint on goal head fib" >/dev/null || fail ":break head failed"
printf '%s\n' "$OUT" | grep -F "added conditional breakpoint fib:arg1<0" >/dev/null || fail ":break condition failed"
printf '%s\n' "$OUT" | grep -F "head: fib" >/dev/null || fail ":info should list head fib"
printf '%s\n' "$OUT" | grep -F "condition: fib:arg1<0" >/dev/null || fail ":info should list condition"
printf '%s\n' "$OUT" | grep -F "(none)" >/dev/null || fail ":clear/:info should report none after clearing"

printf 'Debugger REPL (M5) tests passed.\n'
