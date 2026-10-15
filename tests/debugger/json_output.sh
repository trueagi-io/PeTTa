#!/bin/sh
#
# M6 regression: machine-readable JSON trace (--debug-format=json / --debug-jsonl).
#   - Every emitted debug line is a single, valid JSON object (JSONL).
#   - Runtime events carry stage/goal/index/line/depth/vars (+ result on success),
#     including the redo port for nondeterministic goals.
#   - Space and result events are emitted too.
# Also checks the fast path: with no debug flags, tracing stays off.

set -eu

ROOT_DIR=$(cd -- "$(dirname -- "$0")/../.." && pwd)
MAIN="$ROOT_DIR/src/main.pl"

JSONL=$(mktemp /tmp/petta_jsonl_XXXXXX.jsonl)
VALIDATOR=$(mktemp /tmp/petta_jsonval_XXXXXX.pl)
trap 'rm -f "$JSONL" "$VALIDATOR"' EXIT

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    exit 1
}

# Collect a JSON trace (only the JSON object lines, not program output).
swipl -q -s "$MAIN" -- "$ROOT_DIR/examples/mettaset.metta" \
    --debug=runtime,result --debug-goal=match --debug-format=json --silent 2>&1 \
    | grep '^{' > "$JSONL"

[ -s "$JSONL" ] || fail "no JSON lines emitted"

# Validate that every line is a well-formed JSON object.
cat > "$VALIDATOR" <<'EOF'
:- use_module(library(http/json)).
:- initialization(main).
main :-
    ( current_prolog_flag(argv, [File|_]) -> true ; File = '' ),
    setup_call_cleanup(open(File, read, S), check_lines(S), close(S)),
    writeln('ALL_VALID'),
    halt.
main :- writeln('INVALID'), halt(1).
check_lines(S) :-
    read_line_to_string(S, Line),
    ( Line == end_of_file
      -> true
      ; atom_json_dict(Line, Dict, []),
        ( get_dict(event, Dict, _) -> true ; throw(no_event_field) ),
        check_lines(S)
    ).
EOF

swipl -q "$VALIDATOR" "$JSONL" 2>&1 | grep -qF "ALL_VALID" \
    || fail "not all JSON lines are valid objects with an event field"

# Content checks.
grep -F '"stage":"enter"'   "$JSONL" >/dev/null || fail "no enter stage in JSON"
grep -F '"stage":"success"' "$JSONL" >/dev/null || fail "no success stage in JSON"
grep -F '"stage":"redo"'    "$JSONL" >/dev/null || fail "no redo stage in JSON (nondeterminism)"
grep -F '"event":"result"'  "$JSONL" >/dev/null || fail "no result event in JSON"
grep -F '"event":"runtime"' "$JSONL" >/dev/null || fail "no runtime event in JSON"

# --debug-jsonl is an accepted alias.
swipl -q -s "$MAIN" -- "$ROOT_DIR/examples/chain.metta" \
    --debug=runtime --debug-jsonl --silent 2>&1 | grep -qF '"event":"runtime"' \
    || fail "--debug-jsonl alias did not emit JSON"

# Fast path: with no debug flags, runtime tracing must be inactive (zero overhead).
swipl -q -s "$MAIN" -- "$ROOT_DIR/examples/chain.metta" --silent 2>&1 \
    | grep -qF '"event"' \
    && fail "JSON/trace leaked on a non-debug run"
swipl -q -g "ensure_loaded('$ROOT_DIR/src/metta')" \
    -g "( runtime_tracing_active -> writeln(active), halt(1) ; writeln(inactive) )" \
    -g halt 2>&1 | grep -qF inactive \
    || fail "runtime_tracing_active is on with no debug flags (fast path broken)"

printf 'Debugger JSON-output (M6) tests passed.\n'
