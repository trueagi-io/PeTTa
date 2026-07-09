#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
PATH="$ROOT/../../local/swipl-9.3.36/bin:$PATH"
export PATH
TMPDIR=${TMPDIR:-/tmp}

run_ok() {
    name="$1"
    file="$2"
    log="$TMPDIR/petta-${name}-$$.log"
    timeout 15s sh "$ROOT/run.sh" "$ROOT/$file" > "$log" 2>&1
    printf '%s\n' "$log"
}

# Repeated failed specialization should be memoized: only the first call tries
# wrap and pass.  Later calls should compile to direct fallback calls.
log=$(run_ok repro1 tests/regression/repro1_failed_specialization_memo.metta)
count=$(grep -c 'Not specialized' "$log" || true)
[ "$count" -eq 2 ] || { echo "repro1 expected 2 failed-specialization attempts, got $count"; cat "$log"; exit 1; }

# Binary branching failed-specialization cascade should be linear in chain depth,
# not exponential.  For f1..f12 with f12 terminating, f1..f11 each fail once.
log=$(run_ok repro2 tests/regression/repro2_exponential_failed_specialization.metta)
count=$(grep -c 'Not specialized' "$log" || true)
[ "$count" -eq 11 ] || { echo "repro2 expected 11 failed-specialization attempts, got $count"; cat "$log"; exit 1; }

# Failed specializations must not leave observable type atoms in &self.
log=$(run_ok repro3 tests/regression/repro3_failed_specialization_self_leak.metta)
if grep -q ' (wrap_Spec_' "$log"; then
    echo "repro3 leaked wrap_Spec_ into &self output"
    cat "$log"
    exit 1
fi

grep -q 'partial wrap' "$log" || { echo "repro3 missing parent wrap type output"; cat "$log"; exit 1; }

# Variant-normalized names should not embed fresh Prolog variable ids inside
# compound partial/2 keys.  This repro still has an unbound arithmetic variable
# and may fail at runtime; the regression assertion is the stable specialization
# key generated before the arithmetic error.
log="$TMPDIR/petta-repro4-$$.log"
set +e
timeout 15s sh "$ROOT/run.sh" "$ROOT/tests/regression/repro4_variant_normalization.metta" > "$log" 2>&1
status=$?
set -e
[ "$status" -eq 2 ] || { echo "repro4 expected current arithmetic instantiation error status 2, got $status"; cat "$log"; exit 1; }
if grep -Eq 'app_Spec_\[partial\(lambda_1,\[_[0-9]+\]\)\]' "$log"; then
    echo "repro4 specialization key still contains fresh variable id"
    cat "$log"
    exit 1
fi
grep -Fq 'app_Spec_[partial(lambda_1,[_])]' "$log" || { echo "repro4 missing normalized specialization key"; cat "$log"; exit 1; }

printf 'specializer regression checks passed\n'
