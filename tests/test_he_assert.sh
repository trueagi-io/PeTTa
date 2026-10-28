#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT HUP INT TERM

# Successful scalar, multiple, duplicate, empty and literal-expression results.
timeout 20s sh "$project_dir/run.sh" "$project_dir/examples/he_assert.metta" --silent

# A mismatch must stop execution, rather than returning False and continuing.
# Variables use strict equality, not unification or alpha-equivalence.
for expression in \
    '(assertEqualToResult (+ 1 2) (99))' \
    '(assertEqualToResult (superpose (1 2)) (1 2 3))' \
    '(assertEqualToResult (superpose (1 1)) (1))' \
    '(assertEqualToResult (superpose (1)) (1 1))' \
    '(assertEqualToResult (empty) (1))' \
    '(assertEqualToResult (empty) invalid)' \
    '(assertEqualToResult (empty) $expected)' \
    '(assertEqualToResult (quote ($x $x)) (($y $y)))' \
    '(assertEqualToResult (quote ($x $x)) ((1 1)))'
do
    printf '!(import! &self (library lib_he))\n!%s\n!(println! "AFTER_ASSERTION")\n' \
        "$expression" > "$fixture/failure.metta"
    status=0
    timeout 20s sh "$project_dir/run.sh" "$fixture/failure.metta" --silent \
        > "$fixture/failure.log" 2>&1 || status=$?
    if [ "$status" -ne 1 ] || \
       ! grep -q 'Assertion failed:' "$fixture/failure.log" || \
       grep -q AFTER_ASSERTION "$fixture/failure.log"; then
        echo "Expected assertion failure: $expression" >&2
        cat "$fixture/failure.log" >&2
        exit 1
    fi
done

echo "Hyperon result assertion checks passed"
