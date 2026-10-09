#!/bin/sh

swipl -q -s tests/test_parser_swrite.pl -g run_tests -t halt || exit 1

# strict_* and fail_strict_* examples run with the --strict typechecking flag:
mode_arg_for_test() {
    base=$(basename "$1")
    case "$base" in
        strictdet_*.metta|fail_strictdet_*.metta)
            printf '%s\n' --strict-det
            ;;
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
    # $mode_arg is --strict or empty; unquoted so an empty value adds no argument
    output=$(sh run.sh "$f" $mode_arg)
    error=$?
    output=$(echo "$output"  | grep "is " | grep " should ")
    echo "$output" | grep -q "❌"
    fail=$?
    echo "$output" | grep -q "✅"
    pass=$?
    if [ $error -ne 0 ] || [ $fail -eq 0 ] || [ $pass -ne 0 ]; then
        echo "FAILURE in $f:"
        echo "$output"
        return 1
    else
        echo "OK: $f"
        echo "$output"
        return 0
    fi
}

# fail_* examples must be rejected at compile time with a type/determinism error:
run_expected_fail_test() {
    f="$1"
    mode_arg=$(mode_arg_for_test "$f")
    echo "Running (expected fail) $f"
    output=$(sh run.sh "$f" $mode_arg 2>&1)
    status=$?
    if [ $status -eq 0 ]; then
        echo "FAILURE in $f:"
        echo "Expected non-zero exit status, got success"
        return 1
    fi
    echo "$output" | grep -E -q "Type mismatch|Type conflict|Determinism check failed|Conflicting determinism declarations|Deterministic function .* overlapping clauses|Deterministic function .* is not exhaustive|Strict mode rejected residual runtime type goal|Strict mode cannot express the unresolved nominal intersection|SpaceOf declaration .* rejects existing row|Strict mode requires a declared or inferable type|No matching typed overload|Declared output type variable|Declared parametric parameter|Arrows are prefix|--strict-det requires|Effect-polymorphic declaration|Effect variable|car-atom expects a non-empty expression|Syntax error|is unbound: a -\[.*\]-> function requires bound arguments|is not a proper list: its -\[.*\]-> proof requires a bound proper list"
    if [ $? -ne 0 ]; then
        echo "FAILURE in $f:"
        echo "Expected type error output"
        echo "$output"
        return 1
    fi
    echo "OK (failed as expected): $f"
    return 0
}

# The examples run in a BOUNDED pool: PETTA_TEST_JOBS chunks (default 8), each
# a background worker running its share sequentially. Unbounded fan-out - one
# swipl per example, ~290 at once - could exhaust RAM and take the machine
# down; the pool keeps peak memory at jobs x stack cap by construction, which
# is also why the harness lowers run.sh's stack limit here (override either
# knob in the environment). Failures are collected and reported at the end
# instead of killing the run: verdicts are unchanged, only scheduling is.
run_one_example() {
    case "$(basename "$1")" in
        fail_*.metta) run_expected_fail_test "$1" ;;
        *)            run_test "$1" ;;
    esac
}

NJOBS=${PETTA_TEST_JOBS:-8}
# 3g: the smallest cap the heaviest example (matespacefast) fits in, and
# 8 x 3g bounds the pool's worst case below the machine:
export PETTA_STACK_LIMIT=${PETTA_STACK_LIMIT:-3g}
TMPD=$(mktemp -d /tmp/petta_test_XXXX)
i=0
for f in ./examples/*.metta; do
    base=$(basename "$f")
    case "$base" in repl.metta|llm_cities.metta|torch.metta|greedy_chess.metta|git_import2.metta)
        continue ;;
    esac
    echo "$f" >> "$TMPD/chunk$((i % NJOBS))"
    i=$((i+1))
done

for c in "$TMPD"/chunk*; do
    (
        while IFS= read -r f; do
            if ! run_one_example "$f"; then
                echo "$f" >> "$TMPD/failed"
            fi
        done < "$c"
        exit 0
    ) > "$c.log" 2>&1 &
done
wait

cat "$TMPD"/chunk*.log
status=0
if [ -f "$TMPD/failed" ]; then
    echo ""
    echo "==============================="
    echo "Failed tests:"
    sed 's/^/❌ /' "$TMPD/failed"
    echo "==============================="
    status=1
fi
rm -rf "$TMPD"

if [ $status -eq 0 ]; then
    echo ""
    echo "Running examples/type_dispatch_matrix.sh"
    if sh examples/type_dispatch_matrix.sh; then
        echo "OK: type_dispatch_matrix.sh"
    else
        echo "FAILURE in type_dispatch_matrix.sh"
        status=1
    fi
fi

if [ $status -eq 0 ]; then
    echo ""
    echo "Running examples/lib_trust_matrix.sh"
    if sh examples/lib_trust_matrix.sh; then
        echo "OK: lib_trust_matrix.sh"
    else
        echo "FAILURE in lib_trust_matrix.sh"
        status=1
    fi
fi

if [ $status -eq 0 ]; then
    echo ""
    echo "Running examples/builtin_registry_matrix.sh"
    if sh examples/builtin_registry_matrix.sh; then
        echo "OK: builtin_registry_matrix.sh"
    else
        echo "FAILURE in builtin_registry_matrix.sh"
        status=1
    fi
fi

if [ $status -eq 0 ]; then
    echo ""
    echo "Running examples/typecheck_boundary_matrix.sh"
    if sh examples/typecheck_boundary_matrix.sh; then
        echo "OK: typecheck_boundary_matrix.sh"
    else
        echo "FAILURE in typecheck_boundary_matrix.sh"
        status=1
    fi
fi

if [ $status -eq 0 ]; then
    echo ""
    echo "Running examples/unified_checker_matrix.sh"
    if sh examples/unified_checker_matrix.sh; then
        echo "OK: unified_checker_matrix.sh"
    else
        echo "FAILURE in unified_checker_matrix.sh"
        status=1
    fi
fi

if [ $status -eq 0 ]; then
    echo ""
    echo "Running examples/fuzz_matrix.sh"
    if sh examples/fuzz_matrix.sh; then
        echo "OK: fuzz_matrix.sh"
    else
        echo "FAILURE in fuzz_matrix.sh"
        status=1
    fi
fi

# Soundness oracles (2 extra runs per example; skip with SKIP_SOUNDNESS=1):
if [ $status -eq 0 ] && [ "${SKIP_SOUNDNESS:-0}" != "1" ]; then
    echo ""
    echo "Running examples/soundness_matrix.sh"
    if sh examples/soundness_matrix.sh; then
        echo "OK: soundness_matrix.sh"
    else
        echo "FAILURE in soundness_matrix.sh"
        status=1
    fi
fi

exit $status
