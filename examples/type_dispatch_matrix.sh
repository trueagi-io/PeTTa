#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
TMP_DIR=$(mktemp -d /tmp/type_dispatch_matrix_XXXX)
trap 'rm -rf "$TMP_DIR"' EXIT

run_capture() {
    name=$1
    shift
    out="$TMP_DIR/$name.out"
    if "$@" >"$out" 2>&1; then
        status=0
    else
        status=$?
    fi
    printf '%s\n' "$status" >"$TMP_DIR/$name.status"
}

status_of() {
    cat "$TMP_DIR/$1.status"
}

assert_status() {
    name=$1
    expected=$2
    actual=$(status_of "$name")
    if [ "$actual" != "$expected" ]; then
        echo "[FAIL] $name: expected exit $expected, got $actual"
        cat "$TMP_DIR/$name.out"
        exit 1
    fi
}

assert_contains() {
    name=$1
    pattern=$2
    if ! grep -F "$pattern" "$TMP_DIR/$name.out" >/dev/null; then
        echo "[FAIL] $name: missing pattern: $pattern"
        cat "$TMP_DIR/$name.out"
        exit 1
    fi
}

assert_not_contains() {
    name=$1
    pattern=$2
    if grep -F "$pattern" "$TMP_DIR/$name.out" >/dev/null; then
        echo "[FAIL] $name: unexpected pattern: $pattern"
        cat "$TMP_DIR/$name.out"
        exit 1
    fi
}

assert_tail_not_contains() {
    name=$1
    pattern=$2
    if tail -n 20 "$TMP_DIR/$name.out" | grep -F "$pattern" >/dev/null; then
        echo "[FAIL] $name: unexpected tail pattern: $pattern"
        tail -n 40 "$TMP_DIR/$name.out"
        exit 1
    fi
}

assert_tail_contains() {
    name=$1
    pattern=$2
    if ! tail -n 20 "$TMP_DIR/$name.out" | grep -F "$pattern" >/dev/null; then
        echo "[FAIL] $name: missing tail pattern: $pattern"
        tail -n 40 "$TMP_DIR/$name.out"
        exit 1
    fi
}

run_capture single_monomorphic sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/type_single_monomorphic_resolved.metta"
assert_status single_monomorphic 0
assert_not_contains single_monomorphic "typecheck_match"
assert_not_contains single_monomorphic "typecheck_or_error"
assert_not_contains single_monomorphic "no_matching_overload"

run_capture single_poly_resolved sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/type_single_poly_resolved.metta"
assert_status single_poly_resolved 0
assert_tail_contains single_poly_resolved "'map-flat_Spec_[partial(+,[1])]'(partial(+, [1]),"
assert_tail_not_contains single_poly_resolved "typecheck_match"
assert_tail_not_contains single_poly_resolved "typecheck_or_error"
assert_tail_not_contains single_poly_resolved "no_matching_overload"

run_capture single_poly_compile_fail sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/fail_single_poly_compile_mismatch.metta"
assert_status single_poly_compile_fail 2
assert_contains single_poly_compile_fail "Type mismatch: got [\"s\",2] but expected ['List','Number']"
assert_not_contains single_poly_compile_fail ":- findall(_,"
assert_not_contains single_poly_compile_fail "(   fail"

run_capture overload_resolved sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/type_overload_resolved.metta"
assert_status overload_resolved 0
assert_tail_contains overload_resolved "f(1,"

run_capture overload_unresolved sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/type_overload_runtime_error.metta" -s
assert_status overload_unresolved 0
assert_contains overload_unresolved "(no_matching_overload f)"

run_capture higher_order_partial sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/type_list_parametric.metta" -s
assert_status higher_order_partial 0
assert_contains higher_order_partial "is (2 3 4), should (2 3 4)."

run_capture list_mismatch sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/fail_list_type_mismatch.metta" -s
assert_status list_mismatch 2
assert_contains list_mismatch "Type mismatch: got [1,\"oops\",3] but expected ['List','Number']"

run_capture strict_pass sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/strict_pass_declared.metta" --strict -s
assert_status strict_pass 0
assert_contains strict_pass "3"

run_capture strict_runtime_guard sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/fail_strict_runtime_guard.metta" --strict -s
assert_status strict_runtime_guard 2
assert_contains strict_runtime_guard "Strict mode rejected residual runtime type goal"

run_capture strict_missing_type sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/fail_strict_missing_type.metta" --strict -s
assert_status strict_missing_type 2
assert_contains strict_missing_type "Strict mode requires a declared or inferable type for idish/1"

# Inference + codegen: an untyped fib infers (-> Number Number) and compiles to
# guard-free native arithmetic with a fused comparison.
run_capture inferred_fib sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/fib.metta"
assert_status inferred_fib 0
assert_not_contains inferred_fib "typecheck_or_error"
assert_not_contains inferred_fib "typecheck_match"
assert_contains inferred_fib "A<2"
assert_contains inferred_fib "B is D+F"

# Inference semantics: knowledge only (never rejects), taint on conflicting
# use, and the closure fast path from head-use arrow inference.
run_capture inference sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/type_inference.metta"
assert_status inference 0
assert_contains inference "is A+1"
assert_contains inference "apply_fn1"

# Unknown destructured fields need residual guards; plain inferred parameters,
# declared list fields and constructor fields still compile without them.
run_capture destructured_unknown sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/type_destructured_arg_guard.metta" --warn-runtime-checks
assert_status destructured_unknown 0
assert_contains destructured_unknown "h([A], B) :-"
assert_contains destructured_unknown "typecheck_or_error(A, 'A')"
assert_contains destructured_unknown "Warning: runtime type check in only-a against 'A'"
assert_not_contains destructured_unknown "❌"
if [ "$(grep -c "Warning: runtime type check in only-a" "$TMP_DIR/destructured_unknown.out")" -ne 3 ]; then
    echo "[FAIL] expected guards for h, nested and the consuming branch of maybe-h"
    exit 1
fi

run_capture destructured_known sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/type_destructured_known.metta" --warn-runtime-checks
assert_status destructured_known 0
assert_not_contains destructured_known "Warning: runtime type check"
assert_not_contains destructured_known "typecheck_or_error"
assert_not_contains destructured_known "❌"

run_capture destructured_partial sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/type_destructured_partial.metta" --warn-runtime-checks
assert_status destructured_partial 0
assert_not_contains destructured_partial "Warning: runtime type check"
assert_not_contains destructured_partial "typecheck_or_error"
assert_not_contains destructured_partial "❌"

run_capture destructured_call sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/type_destructured_call_guard.metta" --warn-runtime-checks
assert_status destructured_call 0
assert_contains destructured_call "typecheck_or_error([stv, x, 3],"
assert_contains destructured_call "C is A*B"
assert_not_contains destructured_call "❌"
assert_not_contains destructured_call "type_error evaluable"

run_capture destructured_strict sh "$ROOT_DIR/run.sh" "$ROOT_DIR/examples/strict_destructured_param.metta" --strict --warn-runtime-checks
assert_status destructured_strict 0
assert_not_contains destructured_strict "Warning: runtime type check"
assert_not_contains destructured_strict "typecheck_or_error"

printf '%s\n' "[PASS] single monomorphic resolved"
printf '%s\n' "[PASS] single polymorphic resolved"
printf '%s\n' "[PASS] single polymorphic compile-time mismatch"
printf '%s\n' "[PASS] multi-overload resolved"
printf '%s\n' "[PASS] multi-overload unresolved"
printf '%s\n' "[PASS] higher-order partial"
printf '%s\n' "[PASS] list element mismatch"
printf '%s\n' "[PASS] strict mode declared pass"
printf '%s\n' "[PASS] strict mode runtime-guard rejection"
printf '%s\n' "[PASS] strict mode missing-type rejection"
printf '%s\n' "[PASS] inferred fib compiles guard-free to native arithmetic"
printf '%s\n' "[PASS] inference: knowledge-only, taint, closure fast path"
printf '%s\n' "[PASS] destructured fields: residual checks only without static evidence"
printf '%s\n' "[PASS] complete and partial tagged shapes: caller checks, guard-free callees"
