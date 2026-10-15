#!/bin/sh
#
# Regression tests for debugger instrumentation that must NOT perturb the
# semantics of normal MeTTa evaluation.
#
# Guards two bugs introduced by the initial debugger work and fixed in M0:
#   1. Nondeterminism collapse: trace_goal_execution pushed a call-stack frame
#      on entry but popped it after the goal, so backtracking re-popped and
#      corrupted the global stack, reducing a nondeterministic user goal to its
#      first solution. Fired even with NO debug flags.
#   2. Duplicate solutions: a leftover `call_goals([]).` clause made empty goal
#      lists succeed twice, multiplying solution counts (e.g. nilbc).
#
# It also checks that nondeterminism is preserved WITH tracing active, which the
# top-level test.sh does not exercise.

set -eu

ROOT_DIR=$(cd -- "$(dirname -- "$0")/../.." && pwd)
MAIN="$ROOT_DIR/src/main.pl"

strip_ansi() {
    sed 's/\x1b\[[0-9;]*m//g'
}

run_metta() {
    # usage: run_metta <file> [extra args...]
    file="$1"
    shift
    swipl -q -s "$MAIN" -- "$ROOT_DIR/examples/$file" "$@" 2>&1 | strip_ansi
}

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    exit 1
}

# --- 1. Nondeterminism preserved on normal runs (no debug flags) ---

mettaset_out=$(run_metta mettaset.metta)
printf '%s\n' "$mettaset_out" \
    | grep -F "is ((set 1 a) (set 1 b) (set 1 c) (set 2 d) (set 2 e) (set 2 f) (set 3 a) (set 3 b)), should ((set 1 a) (set 1 b) (set 1 c) (set 2 d) (set 2 e) (set 2 f) (set 3 a) (set 3 b)). ✅" >/dev/null \
    || fail "mettaset collapsed nondeterminism (got: $(printf '%s\n' "$mettaset_out" | grep -F 'should' | head -1))"

for f in matchnested.metta matchnested2.metta; do
    out=$(run_metta "$f")
    printf '%s\n' "$out" \
        | grep -F "is ((transitive sim som sam) (transitive tim tom tam)), should ((transitive sim som sam) (transitive tim tom tam)). ✅" >/dev/null \
        || fail "$f collapsed nondeterminism"
done

# --- 2. No duplicate solutions (call_goals([]) must yield exactly one) ---

# nilbc's first test expects exactly ONE solution: (: (a1 a2 a2) (= $t $t)).
# Before the fix it produced four copies. Count occurrences in the result side.
nilbc_first=$(run_metta nilbc.metta | grep -F 'is ' | grep -F 'should' | head -1)
dup_count=$(printf '%s\n' "$nilbc_first" | sed 's/should.*//' | grep -oF '(a1 a2 a2)' | wc -l | tr -d ' ')
[ "$dup_count" = "1" ] || fail "nilbc produced $dup_count copies of (a1 a2 a2) (expected 1)"

nilbc_fail=$(run_metta nilbc.metta | grep -cF '❌' || true)
[ "$nilbc_fail" = "0" ] || fail "nilbc has $nilbc_fail failing assertions"

# --- 3. Nondeterminism preserved WITH tracing active ---

traced=$(run_metta mettaset.metta --debug=runtime --silent)
printf '%s\n' "$traced" \
    | grep -F "is ((set 1 a) (set 1 b) (set 1 c) (set 2 d) (set 2 e) (set 2 f) (set 3 a) (set 3 b))" >/dev/null \
    || fail "mettaset collapsed nondeterminism under --debug=runtime"

printf 'Debugger nondeterminism regression tests passed.\n'
