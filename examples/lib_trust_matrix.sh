#!/bin/sh
set -u

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
TMP_DIR=$(mktemp -d /tmp/lib_trust_matrix_XXXX)
trap 'rm -rf "$TMP_DIR"' EXIT
FAILED=0

run_compile() {
    file=$1
    out=$2
    shift 2
    sh "$ROOT_DIR/run.sh" "$file" "$@" >"$out" 2>&1
}

trusted="$TMP_DIR/trusted.out"
if run_compile "$SCRIPT_DIR/lib_trust_library_call.metta" "$trusted"; then
    if grep -q "typecheck_or_error" "$trusted" || grep -q "constrain_var_type" "$trusted"; then
        echo "[FAIL] trusted library call emitted a residual guard"
        FAILED=1
    else
        echo "[PASS] trusted library call is guard-free in default mode"
    fi
else
    echo "[FAIL] trusted library fixture did not compile"
    FAILED=1
fi

datastructures="$TMP_DIR/datastructures.out"
if run_compile "$SCRIPT_DIR/lib_trust_datastructures_call.metta" "$datastructures"; then
    if grep -q "typecheck_or_error" "$datastructures" || grep -q "constrain_var_type" "$datastructures"; then
        echo "[FAIL] typed lib_datastructures call emitted a residual guard"
        FAILED=1
    else
        echo "[PASS] typed lib_datastructures call is guard-free in default mode"
    fi
else
    echo "[FAIL] typed lib_datastructures fixture did not compile"
    FAILED=1
fi

declared="$TMP_DIR/declared.out"
if run_compile "$SCRIPT_DIR/lib_trust_declared_caller.metta" "$declared"; then
    if grep -q "typecheck_or_error" "$declared"; then
        echo "[PASS] declared caller retains the library boundary guard"
    else
        echo "[FAIL] declared caller incorrectly suppressed its library guard"
        FAILED=1
    fi
else
    echo "[FAIL] declared-caller library fixture did not compile"
    FAILED=1
fi

user="$TMP_DIR/user.out"
if run_compile "$SCRIPT_DIR/lib_trust_user_redeclaration.metta" "$user"; then
    if grep -q "typecheck_or_error" "$user"; then
        echo "[PASS] user redeclaration restores the residual guard"
    else
        echo "[FAIL] user redeclaration did not restore the residual guard"
        FAILED=1
    fi
else
    echo "[FAIL] user redeclaration fixture did not compile"
    FAILED=1
fi

origin="$TMP_DIR/origin.out"
if run_compile "$SCRIPT_DIR/lib_trust_origin_recompile.metta" "$origin"; then
    if grep -q "should Error. ✅" "$origin"; then
        echo "[PASS] origin flip recompiles prior guard-free callers"
    else
        echo "[FAIL] origin flip left a compiled guard-free caller stale"
        FAILED=1
    fi
else
    echo "[FAIL] origin-recompile fixture did not compile"
    FAILED=1
fi

strict="$TMP_DIR/strict.out"
if run_compile "$SCRIPT_DIR/fail_strict_library_trusted_residual.metta" "$strict" --strict; then
    echo "[FAIL] strict mode accepted a trusted-library residual"
    FAILED=1
elif grep -q "Strict mode rejected residual runtime type goal" "$strict"; then
    echo "[PASS] strict mode still rejects trusted-library residuals"
else
    echo "[FAIL] strict fixture failed for the wrong reason"
    tail -5 "$strict"
    FAILED=1
fi

exit "$FAILED"
