#!/bin/sh
#
# M2 regression: the runtime port model must surface a `redo` port when a
# nondeterministic goal is re-satisfied on backtracking, so multi-solution
# MeTTa goals (match, superpose, case, ...) render as
#   ENTER -> OK -> REDO -> OK -> REDO -> OK ...
# instead of collapsing to a single OK. See traced_goal_port (src/metta.pl).

set -eu

ROOT_DIR=$(cd -- "$(dirname -- "$0")/../.." && pwd)
MAIN="$ROOT_DIR/src/main.pl"

strip_ansi() {
    sed 's/\x1b\[[0-9;]*m//g'
}

run_metta() {
    file="$1"
    shift
    swipl -q -s "$MAIN" -- "$ROOT_DIR/examples/$file" "$@" 2>&1 | strip_ansi
}

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    exit 1
}

# mettaset.metta matches (set $x $y) against 8 atoms in &self. The first
# solution is an OK; each of the remaining 7 must be preceded by a REDO.
trace=$(run_metta mettaset.metta --debug=runtime --debug-goal=match --silent)

ok_count=$(printf '%s\n' "$trace" | grep -cF "OK     (match")
redo_count=$(printf '%s\n' "$trace" | grep -cF "REDO   (match")

[ "$ok_count" -ge 8 ] \
    || fail "expected >= 8 OK match events, got $ok_count (nondeterminism collapsed?)"

# With N solutions there are N-1 redo ports. 8 solutions -> at least 7 redos.
[ "$redo_count" -ge 7 ] \
    || fail "expected >= 7 REDO match events, got $redo_count (redo port missing?)"

# Sanity: a redo must be followed by another OK (re-satisfaction yields a result).
printf '%s\n' "$trace" | grep -F "REDO   (match" >/dev/null \
    || fail "no REDO port emitted at all"

printf 'Debugger redo-port (M2) tests passed.\n'
