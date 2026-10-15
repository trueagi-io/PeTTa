#!/bin/sh
#
# M7 regression: drive a full DAP session against src/debugger/dap_server.pl over stdio
# and check the framed JSON responses/events. Exercises initialize, function
# breakpoints, launch+configurationDone, the stopped event, stackTrace, scopes,
# variables (MeTTa $variable inspection), evaluate (REPL in the paused frame),
# continue, and clean termination on disconnect.

set -eu

ROOT_DIR=$(cd -- "$(dirname -- "$0")/../.." && pwd)
DAP="$ROOT_DIR/src/debugger/dap_server.pl"

INPUT=$(mktemp /tmp/petta_dap_in_XXXXXX)
OUTPUT=$(mktemp /tmp/petta_dap_out_XXXXXX)
trap 'rm -f "$INPUT" "$OUTPUT"' EXIT

# Append one Content-Length framed DAP message (the JSON given as $1) to $INPUT.
frame() {
    json="$1"
    len=$(printf '%s' "$json" | wc -c)
    printf 'Content-Length: %d\r\n\r\n%s' "$len" "$json" >> "$INPUT"
}

run_session() {
    # stderr carries program/debug output; the DAP protocol is on stdout.
    swipl --stack_limit=8g -q -g dap_server "$DAP" < "$INPUT" > "$OUTPUT" 2>/dev/null || true
}

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    printf '%s\n' '--- DAP output ---' >&2
    cat "$OUTPUT" >&2
    exit 1
}

# --- Scenario A: breakpoint, inspect, evaluate, disconnect ---------------
: > "$INPUT"
frame '{"seq":1,"type":"request","command":"initialize","arguments":{"adapterID":"petta"}}'
frame '{"seq":2,"type":"request","command":"setFunctionBreakpoints","arguments":{"breakpoints":[{"name":"fib"}]}}'
frame "{\"seq\":3,\"type\":\"request\",\"command\":\"launch\",\"arguments\":{\"program\":\"$ROOT_DIR/examples/fib.metta\"}}"
frame '{"seq":4,"type":"request","command":"configurationDone","arguments":{}}'
# First stop is (fib 30) from the runnable (no $vars); continue to a recursive call.
frame '{"seq":5,"type":"request","command":"continue","arguments":{"threadId":1}}'
# Second stop is (fib 29) with the caller frame binding $N = 30.
frame '{"seq":6,"type":"request","command":"stackTrace","arguments":{"threadId":1}}'
frame '{"seq":7,"type":"request","command":"scopes","arguments":{"frameId":1}}'
frame '{"seq":8,"type":"request","command":"variables","arguments":{"variablesReference":1}}'
frame '{"seq":9,"type":"request","command":"evaluate","arguments":{"expression":"(+ $N 5)","context":"repl"}}'
frame '{"seq":10,"type":"request","command":"disconnect","arguments":{}}'
run_session

grep -qF '"event":"initialized"'  "$OUTPUT" || fail "no initialized event"
grep -qF '"command":"initialize"' "$OUTPUT" || fail "no initialize response"
grep -qF '"event":"stopped"'      "$OUTPUT" || fail "no stopped event at breakpoint"
grep -qF '"command":"stackTrace"' "$OUTPUT" || fail "no stackTrace response"
grep -qF '(fib'                   "$OUTPUT" || fail "stackTrace has no fib frame"
grep -qF '"command":"variables"'  "$OUTPUT" || fail "no variables response"
grep -qF '$N'                     "$OUTPUT" || fail "variables did not expose \$N"
grep -qF '"command":"evaluate"'   "$OUTPUT" || fail "no evaluate response"
grep -qF '"result":"35"'          "$OUTPUT" || fail "evaluate (+ \$N 5) with \$N=30 should be 35"
grep -qF '"command":"disconnect"' "$OUTPUT" || fail "no disconnect response"

# --- Scenario B: run to natural completion -> terminated/exited ----------
: > "$INPUT"
frame '{"seq":1,"type":"request","command":"initialize","arguments":{}}'
frame "{\"seq\":2,\"type\":\"request\",\"command\":\"launch\",\"arguments\":{\"program\":\"$ROOT_DIR/examples/chain.metta\"}}"
frame '{"seq":3,"type":"request","command":"configurationDone","arguments":{}}'
frame '{"seq":4,"type":"request","command":"disconnect","arguments":{}}'
run_session

grep -qF '"event":"terminated"' "$OUTPUT" || fail "no terminated event on natural completion"
grep -qF '"event":"exited"'     "$OUTPUT" || fail "no exited event on natural completion"

printf 'Debugger DAP (M7) tests passed.\n'
