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
      builtin_signature(implies, 2, unspecified, ['Bool','Bool'], 'Bool'),
      declared_fn_type(implies, ['Bool','Bool'], 'Bool', unspecified),
      format('[PASS] the declaration store is seeded from registered signatures~n', []),
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

for optimize in '' '-O'; do
    if ! swipl $optimize -q -s "$ROOT_DIR/src/metta.pl" -g "$GOAL" -- >"$OUT" 2>&1; then
        echo "[FAIL] builtin registry validation $optimize"
        cat "$OUT"
        exit 1
    fi
    cat "$OUT"
done

# Compile the same source from a stream with one ordinary implementation added.
# Its source ownership remains metta.pl, so the implementation check must see it.
CONTROL="absolute_file_name('$ROOT_DIR/src/metta.pl', Source),
         read_file_to_string(Source, Text, []),
         format(string(Probe), '~nregistry_unregistered_implementation_probe(_, _).~n', []),
         string_concat(Text, Probe, Modified),
         setup_call_cleanup(open_string(Modified, Input),
                            load_files(Source, [stream(Input), if(true), silent(true)]),
                            close(Input)),
         catch(validate_builtin_implementation_coverage,
               error(unregistered_metta_builtin_implementation(registry_unregistered_implementation_probe/2),
                     builtin_registry),
               Rejected = true),
         Rejected == true,
         format('[PASS] an unregistered implementation still fails the completeness check~n', []),
         halt"

for optimize in '' '-O'; do
    if ! swipl $optimize -q -g "$CONTROL" -- >"$OUT" 2>&1; then
        echo "[FAIL] builtin implementation negative control $optimize"
        cat "$OUT"
        exit 1
    fi
    cat "$OUT"
done
