#!/bin/sh

run_test() {
    f="$1"
    echo "Running $f"
    output=$(sh run.sh "$f" | grep "is " | grep " should ")
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
        echo "$output"
        return 0
    fi
}

pids=""
pidfile="/tmp/metta_pid_map.$$"
: > "$pidfile"

for f in ./examples/*.metta; do
    base=$(basename "$f")
    case "$base" in computebudget_limit.metta|repl.metta|llm_cities.metta|torch.metta|greedy_chess.metta|git_import2.metta)
        continue ;;
    esac
    run_test "$f" &
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
exit $status
