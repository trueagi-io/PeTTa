#!/bin/sh
# Randomized execution testing, judged by the oracles that already exist.
#
# fuzz_generator.pl emits small random programs whose embedded !(test ...)
# forms encode the cardinality their DECLARED effects promise - including
# programs whose promise the runtime cannot honor. The checker must reject
# those; conservative rejection is never a failure here. What fails the
# matrix is only ACCEPTED-THEN-MISBEHAVES:
#
#   strict-det cases: loads cleanly but a test yields the wrong count, or
#     --oracle / --oracle-det re-runs throw on a certification the checker
#     discharged statically.
#   default cases:   loads cleanly but a test fails, or the "should" results
#     differ with --no-det-cut (the commit changed observable answers).
#   mutation cases:  an incremental add/remove/late-declaration sequence
#     ends in a state observably different from a fresh load of the
#     equivalent final program - the executable statement of "removal is
#     the inverse of addition".
#
# A clean typecheck/determinism ERROR is enforcement, not failure: a det
# boundary proviso throwing on an unbound argument is the design working.
# Failures are shrunk by form-line deletion and saved with the seed.
set -u
SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
SEED=${FUZZ_SEED:-20260729}
COUNT=${FUZZ_COUNT:-45}
TMP_DIR=$(mktemp -d /tmp/fuzz_matrix_XXXX)
KEEP_DIR=${FUZZ_KEEP_DIR:-/tmp/fuzz_matrix_failures}
trap 'rm -rf "$TMP_DIR"' EXIT
FAILED=0
ACCEPTED=0
REJECTED=0
BUGS=0

swipl -q -s "$SCRIPT_DIR/fuzz_generator.pl" -- "$TMP_DIR/cases" "$SEED" "$COUNT" || {
    echo "[FAIL fuzz] generator failed (seed $SEED)"
    exit 1
}

run_petta() {
    # $1 file, rest: flags. Echoes exit code; output in $TMP_DIR/last.out
    timeout -k 5 60 sh "$ROOT_DIR/run.sh" "$@" -s > "$TMP_DIR/last.out" 2>&1
    echo $?
}

normalize_should() {
    grep "should" "$1" |
        perl -pe 'my (%v, $n); s{\$_[0-9]+}{$v{$&} //= "\$_V" . ++$n}ge'
}

# A test-form mismatch prints ❌; a clean rejection prints a type or
# determinism error and exits nonzero.
has_test_failure() { grep -q "❌" "$TMP_DIR/last.out"; }

# check_case FILE CLASS -> sets CASE_BUG to reason or empty
check_case() {
    f=$1; class=$2
    CASE_BUG=""
    case "$class" in
    strict_det)
        st=$(run_petta "$f" --strict-det)
        # A ❌ means the program LOADED and ran a failing test - a load
        # rejection never reaches test execution - so it outranks exit codes.
        if has_test_failure; then CASE_BUG="accepted but a test failed under --strict-det"; return 0; fi
        if [ "$st" -ne 0 ]; then REJECTED=$((REJECTED + 1)); return 0; fi
        ACCEPTED=$((ACCEPTED + 1))
        st=$(run_petta "$f" --strict-det --oracle)
        if [ "$st" -ne 0 ] || has_test_failure; then CASE_BUG="--oracle contradicted a discharged certification"; return 0; fi
        st=$(run_petta "$f" --strict-det --oracle-det)
        if [ "$st" -ne 0 ] || has_test_failure; then CASE_BUG="--oracle-det found a cardinality violation"; return 0; fi
        ;;
    default)
        st=$(run_petta "$f")
        if has_test_failure; then CASE_BUG="accepted but a test failed in default mode"; return 0; fi
        if [ "$st" -ne 0 ]; then REJECTED=$((REJECTED + 1)); return 0; fi
        ACCEPTED=$((ACCEPTED + 1))
        cp "$TMP_DIR/last.out" "$TMP_DIR/plain.out"
        st=$(run_petta "$f" --no-det-cut)
        if [ "$st" -ne 0 ]; then CASE_BUG="--no-det-cut broke an accepted program"; return 0; fi
        normalize_should "$TMP_DIR/plain.out" > "$TMP_DIR/norm_a"
        normalize_should "$TMP_DIR/last.out" > "$TMP_DIR/norm_b"
        if ! cmp -s "$TMP_DIR/norm_a" "$TMP_DIR/norm_b"; then
            CASE_BUG="results differ without determinism commits"; return 0
        fi
        ;;
    esac
    return 0
}

# check_mutation INC FRESH -> sets CASE_BUG
check_mutation() {
    inc=$1; fresh=$2
    CASE_BUG=""
    sti=$(run_petta "$inc")
    normalize_should "$TMP_DIR/last.out" > "$TMP_DIR/inc_norm"
    incfail=0; has_test_failure && incfail=1
    stf=$(run_petta "$fresh")
    normalize_should "$TMP_DIR/last.out" > "$TMP_DIR/fresh_norm"
    freshfail=0; has_test_failure && freshfail=1
    # Mutation programs are designed-correct, not adversarial: their fresh
    # form's expectations must hold outright.
    if [ "$freshfail" -ne 0 ]; then
        CASE_BUG="designed mutation expectation failed even on fresh load"
        return 0
    fi
    if [ "$sti" -ne "$stf" ] || [ "$incfail" -ne "$freshfail" ]; then
        CASE_BUG="incremental and fresh load disagree on acceptance (inc=$sti/$incfail fresh=$stf/$freshfail)"
        return 0
    fi
    if [ "$sti" -eq 0 ] && ! cmp -s "$TMP_DIR/inc_norm" "$TMP_DIR/fresh_norm"; then
        CASE_BUG="incremental results diverge from fresh load"
        return 0
    fi
    if [ "$sti" -eq 0 ]; then ACCEPTED=$((ACCEPTED + 1)); else REJECTED=$((REJECTED + 1)); fi
    return 0
}

# shrink FILE CLASS [FRESH] - remove one line at a time while the bug persists
shrink_case() {
    f=$1; class=$2; fresh=${3:-}
    want="$CASE_BUG"
    pass=0
    while [ $pass -lt 2 ]; do
        pass=$((pass + 1)); changed=0
        n=$(grep -c "" "$f")
        i=1
        while [ "$i" -le "$n" ]; do
            sed "${i}d" "$f" > "$TMP_DIR/shrunk.metta"
            cp "$f" "$TMP_DIR/backup.metta"
            cp "$TMP_DIR/shrunk.metta" "$f"
            if [ "$class" = mutation ]; then check_mutation "$f" "$fresh"; else check_case "$f" "$class"; fi
            if [ -n "$CASE_BUG" ]; then
                changed=1; n=$((n - 1))
            else
                cp "$TMP_DIR/backup.metta" "$f"; i=$((i + 1))
            fi
        done
        CASE_BUG="$want"
        [ $changed -eq 0 ] && break
    done
}

while IFS="$(printf '\t')" read -r id class path extra; do
    if [ "$class" = mutation ]; then
        check_mutation "$path" "$extra"
    else
        check_case "$path" "$class"
    fi
    if [ -n "$CASE_BUG" ]; then
        BUGS=$((BUGS + 1)); FAILED=1
        reason="$CASE_BUG"
        shrink_case "$path" "$class" "$extra"
        mkdir -p "$KEEP_DIR"
        keep="$KEEP_DIR/seed${SEED}_case${id}.metta"
        cp "$path" "$keep"
        [ "$class" = mutation ] && cp "$extra" "$KEEP_DIR/seed${SEED}_case${id}_fresh.metta"
        echo "[FAIL fuzz] case $id ($class): $reason"
        echo "  seed $SEED, shrunk program saved to $keep:"
        sed 's/^/    /' "$path"
    fi
done < "$TMP_DIR/cases/manifest.tsv"

echo "fuzz summary: seed=$SEED count=$COUNT accepted=$ACCEPTED rejected=$REJECTED bugs=$BUGS"
if [ "$FAILED" -ne 0 ]; then exit 1; fi
echo "[PASS] randomized execution testing found no accepted-then-misbehaving program"
exit 0
