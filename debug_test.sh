#!/bin/sh

set -eu

ROOT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
TRACE_FILE="$ROOT_DIR/debug_trace_test.log"
rm -f "$TRACE_FILE"

output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/examples/chain.metta" --debug=runtime --debug-goal=+ 2>&1)
# Arithmetic builtins are compiled to native is/2 (arith_inline, src/translator.pl)
# and constant-folded when both operands are literals, so there is no call left to
# trace. Trace a user-defined function instead.
trace_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/tests/debugger/fixtures/add_buggy.metta" --debug=runtime --debug-goal=add-two 2>&1 || true)
prolog_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/tests/debugger/fixtures/add_buggy.metta" --debug=runtime-prolog --debug-goal=add-two --silent 2>&1 || true)
help_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- --debug-help 2>&1)
file_output_console=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/tests/debugger/fixtures/fib_buggy.metta" --debug=runtime --debug-goal=fib --debug-max-events=8 --debug-output="$TRACE_FILE" --silent 2>&1 || true)
spaces_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/examples/spaces.metta" 2>&1)
space_debug_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/examples/spaces.metta" --debug=space --silent 2>&1)
add_buggy_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/tests/debugger/fixtures/add_buggy.metta" --debug=runtime --debug-goal=add-two --silent 2>&1 || true)
add_buggy_op_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/tests/debugger/fixtures/add_buggy.metta" --debug=runtime --debug-goal=add-two --silent 2>&1 || true)
depth_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/tests/debugger/fixtures/fib_buggy.metta" --debug=runtime --debug-goal=fib --debug-depth=2 --silent 2>&1 || true)
limit_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/tests/debugger/fixtures/fib_buggy.metta" --debug=runtime --debug-goal=fib --debug-max-events=6 --silent 2>&1 || true)
break_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/tests/debugger/fixtures/fib_buggy.metta" --debug-break=fib --debug-goal=fib --debug-depth=2 --silent 2>&1 || true)
cond_break_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/tests/debugger/fixtures/fib_buggy.metta" --debug-break-if='fib:arg1<0' --debug-goal=fib --silent 2>&1 || true)
break_once_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/tests/debugger/fixtures/fib_buggy.metta" --debug-break-if='fib:arg1<0' --debug-break-once --debug-goal=fib --silent 2>&1 || true)
result_break_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/tests/debugger/fixtures/fib_buggy.metta" --debug-break-if='fib:result=0' --debug-break-once --debug-goal=fib --silent 2>&1 || true)
and_break_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/tests/debugger/fixtures/fib_buggy.metta" --debug-break-if='fib:arg1=2&result=0' --debug-break-once --debug-goal=fib --silent 2>&1 || true)
or_break_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/tests/debugger/fixtures/fib_buggy.metta" --debug-break-if='fib:arg1<0|result=0' --debug-break-once --debug-goal=fib --silent 2>&1 || true)
skip_break_output=$(swipl -q -s "$ROOT_DIR/src/main.pl" -- "$ROOT_DIR/tests/debugger/fixtures/fib_buggy.metta" --debug-break-if='fib:arg1<0|result=0' --debug-break-skip=1 --debug-break-once --debug-goal=fib --silent 2>&1 || true)

strip_ansi() {
    sed 's/\x1b\[[0-9;]*m//g'
}

clean_output=$(printf '%s\n' "$output" | strip_ansi)
clean_trace_output=$(printf '%s\n' "$trace_output" | strip_ansi)
clean_prolog_output=$(printf '%s\n' "$prolog_output" | strip_ansi)
clean_help_output=$(printf '%s\n' "$help_output" | strip_ansi)
clean_file_output_console=$(printf '%s\n' "$file_output_console" | strip_ansi)
clean_trace_file=$(strip_ansi < "$TRACE_FILE")
clean_spaces_output=$(printf '%s\n' "$spaces_output" | strip_ansi)
clean_space_debug_output=$(printf '%s\n' "$space_debug_output" | strip_ansi)
clean_add_buggy_output=$(printf '%s\n' "$add_buggy_output" | strip_ansi)
clean_add_buggy_op_output=$(printf '%s\n' "$add_buggy_op_output" | strip_ansi)
clean_depth_output=$(printf '%s\n' "$depth_output" | strip_ansi)
clean_limit_output=$(printf '%s\n' "$limit_output" | strip_ansi)
clean_break_output=$(printf '%s\n' "$break_output" | strip_ansi)
clean_cond_break_output=$(printf '%s\n' "$cond_break_output" | strip_ansi)
clean_break_once_output=$(printf '%s\n' "$break_once_output" | strip_ansi)
clean_result_break_output=$(printf '%s\n' "$result_break_output" | strip_ansi)
clean_and_break_output=$(printf '%s\n' "$and_break_output" | strip_ansi)
clean_or_break_output=$(printf '%s\n' "$or_break_output" | strip_ansi)
clean_skip_break_output=$(printf '%s\n' "$skip_break_output" | strip_ansi)

printf '%s\n' "$clean_trace_output" | grep -F "stack: (add-two 7 3)" >/dev/null
printf '%s\n' "$clean_trace_output" | grep -F "ENTER  (add-two 7 3)" >/dev/null
printf '%s\n' "$clean_trace_output" | grep -F "OK     (add-two 7 3) => 4" >/dev/null
printf '%s\n' "$clean_trace_output" | grep -F "#2 line 4 compiled" >/dev/null
printf '%s\n' "$clean_output" | grep -F "is 18, should 18. ✅" >/dev/null
printf '%s\n' "$clean_output" | grep -F "is 12, should 12. ✅" >/dev/null
printf '%s\n' "$clean_prolog_output" | grep -F "ENTER  (add-two 7 3)" >/dev/null
printf '%s\n' "$clean_prolog_output" | grep -F ":- 'add-two'(7, 3, _)." >/dev/null
printf '%s\n' "$clean_help_output" | grep -F "PeTTa Debugger" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F "runtime-leaf" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F "runtime-prolog" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F "sh debug.sh examples/fib.metta --debug=runtime" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F -- "--debug-break=<heads>" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F -- "--debug-break-if=<spec>" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F -- "--debug-break-once" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F -- "--debug-break-space=<spaces>" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F -- "--debug-break-match-fail" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F -- "--debug-break-error" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F -- "--debug-format=json" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F -- "--debug-break-skip=<n>" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F -- "--debug-output=<file>" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F "arg1=2&result=0" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F "arg1<0|result=0" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F "debug-output=trace.log" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F "TTY breakpoint commands:" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F -- "--debug-depth=<n>" >/dev/null
printf '%s\n' "$clean_help_output" | grep -F -- "--debug-max-events=<n>" >/dev/null
printf '%s\n' "$clean_file_output_console" | grep -F "ENTER  (fib 10)" >/dev/null
printf '%s\n' "$clean_trace_file" | grep -F "ENTER  (fib 10)" >/dev/null
printf '%s\n' "$clean_trace_file" | grep -F "[DEBUG limit] event limit reached; suppressing further debug output" >/dev/null
printf '%s\n' "$clean_spaces_output" | grep -F "is ((bar a) (bar b)), should ((bar a) (bar b)). ✅" >/dev/null
printf '%s\n' "$clean_space_debug_output" | grep -F "result: (bar a)" >/dev/null
printf '%s\n' "$clean_space_debug_output" | grep -F "result: (bar b)" >/dev/null
printf '%s\n' "$clean_add_buggy_output" | grep -F "ENTER  (add-two 7 3)" >/dev/null
printf '%s\n' "$clean_add_buggy_output" | grep -F "OK     (add-two 7 3) => 4" >/dev/null
printf '%s\n' "$clean_add_buggy_op_output" | grep -F "ENTER  (add-two 7 3)" >/dev/null
printf '%s\n' "$clean_add_buggy_op_output" | grep -F "OK     (add-two 7 3) => 4" >/dev/null
printf '%s\n' "$clean_depth_output" | grep -F "ENTER  (fib 10)" >/dev/null
printf '%s\n' "$clean_depth_output" | grep -F "ENTER  (fib 9)" >/dev/null
if printf '%s\n' "$clean_depth_output" | grep -F "stack: (fib 10) → (fib 9) →" >/dev/null; then exit 1; fi
printf '%s\n' "$clean_limit_output" | grep -F "[DEBUG limit] event limit reached; suppressing further debug output" >/dev/null
# The top-level !(test (fib 10) ...) call is form #2 on line 7; recursive fib
# calls come from the function clause defined as form #1 on line 1 (M1 source
# mapping replaced the old hard-coded "#compiled line 0").
printf '%s\n' "$clean_break_output" | grep -F "[BREAKPOINT #2 line 7 compiled] (fib 10)" >/dev/null
printf '%s\n' "$clean_break_output" | grep -F "[BREAKPOINT #1 line 1 compiled] (fib 9)" >/dev/null
if printf '%s\n' "$clean_break_output" | grep -F "stack: (fib 10) → (fib 9) →" >/dev/null; then exit 1; fi
printf '%s\n' "$clean_cond_break_output" | grep -F "[BREAKPOINT #1 line 1 compiled] (fib -1)" >/dev/null
printf '%s\n' "$clean_cond_break_output" | grep -F "source expr: (fib (- 2 3))" >/dev/null
printf '%s\n' "$clean_cond_break_output" | grep -F "var \$N = 2" >/dev/null
break_once_count=$(printf '%s\n' "$clean_break_once_output" | grep -F "[BREAKPOINT #1 line 1 compiled]" | wc -l | tr -d ' ')
[ "$break_once_count" = "1" ]
printf '%s\n' "$clean_result_break_output" | grep -F "[BREAKPOINT #1 line 1 compiled] (fib 2) => 0" >/dev/null
printf '%s\n' "$clean_and_break_output" | grep -F "[BREAKPOINT #1 line 1 compiled] (fib 2) => 0" >/dev/null
and_break_count=$(printf '%s\n' "$clean_and_break_output" | grep -F "[BREAKPOINT #1 line 1 compiled]" | wc -l | tr -d ' ')
[ "$and_break_count" = "1" ]
printf '%s\n' "$clean_or_break_output" | grep -F "reason: matched condition fib:arg1<0|result=0" >/dev/null
printf '%s\n' "$clean_or_break_output" | grep -F "match: arg1 = -1" >/dev/null
printf '%s\n' "$clean_skip_break_output" | grep -F "hit: 2" >/dev/null
printf '%s\n' "$clean_skip_break_output" | grep -F "reason: matched condition fib:arg1<0|result=0" >/dev/null

printf 'Debugger smoke test passed.\n'
rm -f "$TRACE_FILE"
