#!/bin/sh
# Soundness oracles for the typechecker, run over the whole example suite.
#
# Phase A (--oracle): every statically discharged certification - clause
# OUTPUTS and call-site ARGUMENTS alike - is re-emitted as a runtime check. If
# the checker certified a type the runtime value does not have, the check
# throws and the example fails. Argument certifications are the half that used
# to go unaudited, and call sites are where the interesting holes live.
#
# Phase B (--no-det-cut): the determinism commit is suppressed, exposing
# CLAUSE-SELECTION alternatives. A narrow instrument: the ! sits at clause
# entry and overlapping heads are already a hard static error, so in practice
# it almost never differs. Kept because it is cheap and it is the only phase
# that tests the commit itself.
#
# Phase C (--oracle-det): every call believed det/semidet — including a
# -[$e]-> call instantiated by its closure arguments — counts its solutions
# and throws on zero (det) or on two or more (either). This is what catches
# BODY-level determinism violations, which Phase B is blind to.
#
# Phase D (counterexample cases): MULTI-FILE programs that would violate a
# certification, each pinned to the exact finding that must reject it. Unlike
# phases A-C these are expected to fail, and the test is that they fail for the
# stated reason. Load ORDER is the point of every case here: a constructor or a
# determinism declaration arriving in a later file than the code it constrains
# cannot be expressed in a single file at all, because the per-file declaration
# prepass makes a file's own declarations visible to all of it.
#
# Each case names the flags it needs, and all of them currently need NONE: the
# checker rejects these at compile time, so no oracle has to run to catch them.
# That is the goal state - a case that only fails under an oracle flag is a
# hole in the static checker that has been instrumented rather than closed.
# The oracles are still exercised, over the whole suite, by phases A-C.
#
# Phases A-C only make sense for examples that normally pass; fail_* and the
# standing skip list are excluded. Timeouts are reported, not hidden.
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

#SWI's printed names for otherwise identical free variables depend on clause
#allocation details (including whether commit cuts were emitted). Canonicalize
#them per result line while preserving repeated-variable identity, so Phase B
#compares answers rather than allocator-generated suffixes.
normalize_test_results() {
    grep "should" |
        perl -pe 'my (%v, $n); s{\$_[0-9]+}{$v{$&} //= "\$_V" . ++$n}ge'
}

# Phases A-C for one example. Failure is signalled through $TMP_DIR/failed
# (this runs inside a pooled background worker, where a shell variable would
# be lost - the same convention Phase D already uses):
run_oracle_phases() {
    f=$1
    base=$(basename "$f")
    mode=$(mode_arg_for "$f")

    # Phase A: forced certification guards must never fire.
    out=$(timeout -k 5 240 sh "$ROOT_DIR/run.sh" "$f" $mode --oracle -s 2>&1)
    st=$?
    if [ $st -eq 124 ] || [ $st -eq 137 ]; then
        echo "[SKIP-TIMEOUT oracle] $base"
    elif [ $st -ne 0 ] || echo "$out" | grep -q "❌"; then
        echo "[FAIL oracle] $base: certified type contradicted at runtime (or run broke under --oracle)"
        echo "$out" | grep -E "❌|ERROR" | head -3
        : > "$TMP_DIR/failed"
    fi

    # Phase B: removing det commits must not change any test result.
    normout=$(timeout -k 5 240 sh "$ROOT_DIR/run.sh" "$f" $mode -s 2>&1)
    st1=$?
    nodout=$(timeout -k 5 240 sh "$ROOT_DIR/run.sh" "$f" $mode --no-det-cut -s 2>&1)
    st2=$?
    if [ $st1 -eq 124 ] || [ $st1 -eq 137 ] || [ $st2 -eq 124 ] || [ $st2 -eq 137 ]; then
        echo "[SKIP-TIMEOUT no-det-cut] $base"
    else
        norm=$(echo "$normout" | normalize_test_results)
        nod=$(echo "$nodout" | normalize_test_results)
        if [ "$norm" != "$nod" ]; then
            echo "[FAIL det] $base: results differ without determinism commits"
            : > "$TMP_DIR/failed"
        fi
    fi

    # Phase C: every committed call must really have the cardinality it claims.
    out=$(timeout -k 5 600 sh "$ROOT_DIR/run.sh" "$f" $mode --oracle-det -s 2>&1)
    st=$?
    if [ $st -eq 124 ] || [ $st -eq 137 ]; then
        echo "[SKIP-TIMEOUT oracle-det] $base"
    elif [ $st -ne 0 ] || echo "$out" | grep -q "❌"; then
        echo "[FAIL oracle-det] $base: declared determinism contradicted at runtime"
        echo "$out" | grep -E "❌|ERROR" | head -3
        : > "$TMP_DIR/failed"
    fi
}

# The per-file phases run in a BOUNDED pool of PETTA_TEST_JOBS workers
# (default 8), each taking a chunk of the file list sequentially - this loop
# is ~3 swipl runs per example and used to be the bulk of the suite's wall
# clock. The stack cap keeps pool x worst-case memory bounded.
NJOBS=${PETTA_TEST_JOBS:-8}
export PETTA_STACK_LIMIT=${PETTA_STACK_LIMIT:-3g}
i=0
for f in "$ROOT_DIR"/examples/*.metta; do
    base=$(basename "$f")
    case "$base" in
        fail_*.metta|repl.metta|llm_cities.metta|torch.metta|greedy_chess.metta|git_import2.metta) continue ;;
    esac
    echo "$f" >> "$TMP_DIR/chunk$((i % NJOBS))"
    i=$((i+1))
done
for c in "$TMP_DIR"/chunk*; do
    (
        while IFS= read -r f; do
            run_oracle_phases "$f"
        done < "$c"
        exit 0
    ) > "$c.log" 2>&1 &
done
wait
cat "$TMP_DIR"/chunk*.log

# Phase D. One case per line:
#
#     <extra flags>|<substring the finding must contain>|<file> [<file> ...]
#
# Paths are relative to examples/ and are loaded IN THE ORDER GIVEN - run.sh
# passes several .metta files straight through to the loader, and load order is
# precisely what these cases are about. The case must fail, and it must fail
# with the named finding: a case that dies of something else (the raw Prolog
# error the hole would otherwise produce, or a dynamic oracle finding where a
# static rejection is expected) means the check stopped catching it, which is a
# regression, not a pass.
#
# The single-file counterexamples that used to live here are now ordinary
# examples/fail_*.metta cases, checked by test.sh like every other one:
# fail_det_once_semidet, fail_det_two_arg_if, fail_det_case_no_catchall,
# fail_newtype_wildcard_leak.
counterexample_cases() {
    cat <<'CASES'
|Deterministic function pick has overlapping clauses|soundness/late_det_decl_1_defs.metta soundness/late_det_decl_2_decl.metta
|Type mismatch: got "oops" but expected 'Number'|soundness/ctor_snapshot_1_goal.metta soundness/ctor_snapshot_2_gpu.metta
|Deterministic function rank is not exhaustive|soundness/late_ctor_exhaustive_1_ranks.metta soundness/late_ctor_exhaustive_2_blue.metta
|Argument of f is unbound|soundness/late_det_boundness_1_defs.metta soundness/late_det_boundness_2_decl.metta
CASES
}

counterexample_cases | while IFS='|' read -r flags want files; do
    [ -n "$files" ] || continue
    paths=""
    for rel in $files; do paths="$paths $ROOT_DIR/examples/$rel"; done
    # shellcheck disable=SC2086
    out=$(timeout -k 5 240 sh "$ROOT_DIR/run.sh" $paths $flags -s 2>&1)
    st=$?
    if [ $st -eq 0 ]; then
        echo "[FAIL counterexample] $files: expected it to be rejected, it ran clean"
        : > "$TMP_DIR/failed"
    elif ! echo "$out" | grep -qF "$want"; then
        echo "[FAIL counterexample] $files: failed, but not with the expected finding"
        echo "  wanted: $want"
        echo "$out" | grep -E "ERROR" | head -2
        : > "$TMP_DIR/failed"
    fi
done
if [ -f "$TMP_DIR/failed" ]; then FAILED=1; fi

if [ $FAILED -eq 0 ]; then
    echo "OK: soundness_matrix.sh"
else
    echo "FAILURES in soundness_matrix.sh"
    exit 1
fi
