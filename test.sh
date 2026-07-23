#!/bin/sh

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
    echo "$output" | grep -E -q "Type mismatch|Type conflict|Determinism check failed|Conflicting determinism declarations|Deterministic function .* overlapping clauses|Strict mode rejected residual runtime type goal|Strict mode requires a declared or inferable type|No matching typed overload|Declared output type variable"
    if [ $? -ne 0 ]; then
        echo "FAILURE in $f:"
        echo "Expected type error output"
        echo "$output"
        return 1
    fi
    echo "OK (failed as expected): $f"
    return 0
}

pids=""
pidfile="/tmp/metta_pid_map.$$"
: > "$pidfile"

for f in ./examples/*.metta; do
    base=$(basename "$f")
    case "$base" in repl.metta|llm_cities.metta|torch.metta|greedy_chess.metta|git_import2.metta)
        continue ;;
    esac
    case "$base" in fail_*.metta)
        run_expected_fail_test "$f" &
        ;;
    *)
        run_test "$f" &
        ;;
    esac
    pid=$!
    pids="$pids $pid"
    echo "$pid $f" >> "$pidfile"
done

status=0
for pid in $pids; do
    if ! wait "$pid"; then
        failed_file=$(grep "^$pid " "$pidfile" | cut -d' ' -f2-)
        echo ""
        echo "==============================="
        echo "Stopping tests due to failure:"
        echo "❌ Failed test: $failed_file"
        echo "==============================="
        kill $pids 2>/dev/null
        status=1
        break
    fi
done

rm -f "$pidfile"

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

exit $status
