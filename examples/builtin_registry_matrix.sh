#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
OUT=$(mktemp /tmp/builtin_registry_matrix_XXXX)
trap 'rm -f "$OUT"' EXIT

GOAL="validate_builtin_registry_schema,
      format('[PASS] registry schema and unique keys~n', []),
      validate_builtin_registry_hooks,
      format('[PASS] named procedural hooks exist~n', []),
      validate_builtin_registry_signatures,
      format('[PASS] builtin signatures agree in both directions~n', []),
      builtin_signature(implies, 2, unspecified, ['Bool','Bool'], 'Bool'),
      format('[PASS] implies has a registered Boolean signature~n', []),
      validate_builtin_registration_coverage,
      validate_builtin_implementation_coverage,
      format('[PASS] registered and implemented builtins are covered~n', []),
      setup_call_cleanup(
          assertz(fun('__registry_matrix_unregistered')),
          catch(validate_builtin_registration_coverage,
                error(unregistered_builtin_spec('__registry_matrix_unregistered'),
                      builtin_registry),
                Rejected = true),
          retractall(fun('__registry_matrix_unregistered'))),
      Rejected == true,
      format('[PASS] an unregistered builtin fails the completeness check~n', []),
      halt"

if ! swipl -q -s "$ROOT_DIR/src/metta.pl" -g "$GOAL" -- >"$OUT" 2>&1; then
    echo "[FAIL] builtin registry validation"
    cat "$OUT"
    exit 1
fi

cat "$OUT"
