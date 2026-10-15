#!/bin/sh
#
# M4 regression: richer breakpoints and watch conditions.
#   1. Substring/structural condition operator `~` (e.g. match:arg3~set).
#   2. --debug-break-space=<space>: break on add/remove space mutation.
#   3. --debug-break-match-fail: break when a space match fails.
#   4. --debug-break-error: break when a goal returns an Error term.
# See debugger.pl (compare_break_value/~, space_breakpoint_*, breakpoint_triggered
# for errors) and spaces.pl event emission.

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

# 1. Substring/structural condition: arg3 of match is (set $x $y); ~set matches.
contains_out=$(run_metta mettaset.metta --debug-break-if='match:arg3~set' --debug-break-once --debug-goal=match --silent)
printf '%s\n' "$contains_out" | grep -F "reason: matched condition match:arg3~set" >/dev/null \
    || fail "substring operator ~ did not match arg3~set"

# 2. Break on space mutation (add to &self).
space_out=$(run_metta spaces.metta --debug-break-space='&self' --debug-break-once --silent)
printf '%s\n' "$space_out" | grep -F "[BREAKPOINT space]" >/dev/null \
    || fail "no space-mutation breakpoint fired for --debug-break-space=&self"
printf '%s\n' "$space_out" | grep -F "reason: space mutation: add to &self" >/dev/null \
    || fail "space-mutation breakpoint reason missing"

# 3. Break on match failure.
matchfail_out=$(run_metta matchsingle.metta --debug-break-match-fail --debug-break-once --silent)
printf '%s\n' "$matchfail_out" | grep -F "[BREAKPOINT space]" >/dev/null \
    || fail "no match-fail breakpoint fired for --debug-break-match-fail"
printf '%s\n' "$matchfail_out" | grep -F "reason: match failed in" >/dev/null \
    || fail "match-fail breakpoint reason missing"

# 4. Break on a goal returning an Error term.
error_out=$(run_metta he_error.metta --debug-break-error --debug-break-once --silent)
printf '%s\n' "$error_out" | grep -F "reason: returned error" >/dev/null \
    || fail "no error breakpoint fired for --debug-break-error"

printf 'Debugger rich-breakpoints (M4) tests passed.\n'
