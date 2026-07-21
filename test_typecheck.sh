#!/bin/sh
# Spec suite for the compile-time typechecking feature (see AGENTS.md).
# These tests are EXPECTED TO FAIL until the corresponding phase is implemented.
# Salvaged from the abandoned compile-time-typechecking branch (a0e0746 + fixes).
#
# Usage: sh test_typecheck.sh            run the whole spec suite
#        sh test_typecheck.sh <pattern>  run only files matching the glob pattern
# The regular regression suite (test.sh) is unaffected by these tests.

cd "$(dirname "$0")"

mode_arg_for_test() {
    base=$(basename "$1")
    case "$base" in
        strict_*.metta|fail_strict_*.metta)
            printf '%s\n' --strict
            ;;
        *)
            printf '%s\n' ''
            ;;
    esac
}

run_test() {
    f="$1"
    mode_arg=$(mode_arg_for_test "$f")
    echo "Running $f"
    if [ -n "$mode_arg" ]; then
        raw_output=$(sh run.sh "$f" "$mode_arg")
    else
        raw_output=$(sh run.sh "$f")
    fi
    output=$(printf '%s\n' "$raw_output" | grep "is " | grep " should ")
    echo "$output" | grep -q "❌"
    fail=$?
    echo "$output" | grep -q "✅"
    pass=$?
    if [ $fail -eq 0 ] || [ $pass -ne 0 ]; then
        echo "FAILURE in $f:"
        echo "$output"
        return 1
    else
        echo "OK: $f"
        return 0
    fi
}

run_expected_fail_test() {
    f="$1"
    mode_arg=$(mode_arg_for_test "$f")
    echo "Running (expected fail) $f"
    if [ -n "$mode_arg" ]; then
        output=$(sh run.sh "$f" "$mode_arg" 2>&1)
    else
        output=$(sh run.sh "$f" 2>&1)
    fi
    status=$?
    if [ $status -eq 0 ]; then
        echo "FAILURE in $f:"
        echo "Expected non-zero exit status, got success"
        return 1
    fi
    echo "$output" | grep -E -q "Type mismatch|Type conflict|Determinism check failed|Conflicting determinism declarations|Deterministic function .* overlapping clauses|Strict mode rejected residual runtime type goal|Strict mode requires a declared or inferable type|No matching typed overload"
    if [ $? -ne 0 ]; then
        echo "FAILURE in $f:"
        echo "Expected type error output"
        echo "$output"
        return 1
    fi
    echo "OK (failed as expected): $f"
    return 0
}

pattern="${1:-*}"
failures=0
total=0

for f in ./examples_typecheck/$pattern.metta; do
    [ -e "$f" ] || continue
    total=$((total + 1))
    base=$(basename "$f")
    case "$base" in fail_*.metta)
        run_expected_fail_test "$f" || failures=$((failures + 1))
        ;;
    *)
        run_test "$f" || failures=$((failures + 1))
        ;;
    esac
done

if [ "$pattern" = "*" ]; then
    echo ""
    echo "Running type_dispatch_matrix.sh"
    if sh examples_typecheck/type_dispatch_matrix.sh; then
        echo "OK: type_dispatch_matrix.sh"
    else
        failures=$((failures + 1))
        echo "FAILURE in type_dispatch_matrix.sh"
    fi
    total=$((total + 1))
fi

echo ""
echo "=== typecheck spec: $((total - failures))/$total passing ==="
[ "$failures" -eq 0 ]
