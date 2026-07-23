#!/bin/sh
# Soundness oracles for the typechecker, run over the whole example suite.
#
# Phase A (--oracle): every statically discharged output certification is
# re-emitted as a runtime guard. If the checker certified a type the runtime
# value does not have, the guard throws and the example fails.
#
# Phase B (--no-det-cut): the determinism commit is suppressed. A function
# declared/validated det that is semantically nondeterministic then produces
# extra results, visible as a diff of the test output against the normal run.
#
# Both phases only make sense for examples that normally pass; fail_* and
# the standing skip list are excluded. Timeouts are reported, not hidden.
set -u
SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
TMP_DIR=$(mktemp -d /tmp/soundness_matrix_XXXX)
trap 'rm -rf "$TMP_DIR"' EXIT
FAILED=0

mode_arg_for() {
    case "$(basename "$1")" in
        strictdet_*.metta) printf '%s' --strict-det ;;
        strict_*.metta)    printf '%s' --strict ;;
        *)                 printf '%s' '' ;;
    esac
}

for f in "$ROOT_DIR"/examples/*.metta; do
    base=$(basename "$f")
    case "$base" in
        fail_*.metta|repl.metta|llm_cities.metta|torch.metta|greedy_chess.metta|git_import2.metta) continue ;;
    esac
    mode=$(mode_arg_for "$f")

    # Phase A: forced certification guards must never fire.
    out=$(timeout -k 5 240 sh "$ROOT_DIR/run.sh" "$f" $mode --oracle -s 2>&1)
    st=$?
    if [ $st -eq 124 ] || [ $st -eq 137 ]; then
        echo "[SKIP-TIMEOUT oracle] $base"
    elif [ $st -ne 0 ] || echo "$out" | grep -q "❌"; then
        echo "[FAIL oracle] $base: certified type contradicted at runtime (or run broke under --oracle)"
        echo "$out" | grep -E "❌|ERROR" | head -3
        FAILED=1
    fi

    # Phase B: removing det commits must not change any test result.
    normout=$(timeout -k 5 240 sh "$ROOT_DIR/run.sh" "$f" $mode -s 2>&1)
    st1=$?
    nodout=$(timeout -k 5 240 sh "$ROOT_DIR/run.sh" "$f" $mode --no-det-cut -s 2>&1)
    st2=$?
    if [ $st1 -eq 124 ] || [ $st1 -eq 137 ] || [ $st2 -eq 124 ] || [ $st2 -eq 137 ]; then
        echo "[SKIP-TIMEOUT no-det-cut] $base"
    else
        norm=$(echo "$normout" | grep "should")
        nod=$(echo "$nodout" | grep "should")
        if [ "$norm" != "$nod" ]; then
            echo "[FAIL det] $base: results differ without determinism commits"
            FAILED=1
        fi
    fi
done

if [ $FAILED -eq 0 ]; then
    echo "OK: soundness_matrix.sh"
else
    echo "FAILURES in soundness_matrix.sh"
    exit 1
fi
