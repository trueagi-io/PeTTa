#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
OUT=$(mktemp /tmp/typecheck_boundary_matrix_XXXX)
trap 'rm -f "$OUT"' EXIT

cd "$ROOT_DIR"

# Deliberately reverse the ordinary non-module load order and import the real
# registry module last. This process does not load typecheck.pl: success proves
# that no directive or clause-contiguity dependency is hidden in its ordering.
PERMUTED_GOAL="maplist(ensure_loaded,
    ['src/typecheck/value_checks.pl',
     'src/typecheck/type_lang.pl',
     'src/typecheck/oracles.pl',
     'src/typecheck/inference.pl',
     'src/typecheck/flags_arrows.pl',
     'src/typecheck/det_validate.pl',
     'src/typecheck/det_proofs.pl',
     'src/typecheck/det_builtins.pl',
     'src/typecheck/det_analysis.pl',
     'src/typecheck/dependency_graph.pl',
     'src/typecheck/decl_store.pl',
     'src/typecheck/clause_checks.pl',
     'src/typecheck/analysis_proofs.pl']),
    use_module('src/typecheck/builtin_registry.pl'),
    format('[PASS] checker units load in a permuted order~n', []),
    halt"

if ! swipl -q -g "$PERMUTED_GOAL" >"$OUT" 2>&1; then
    echo "[FAIL] permuted checker-unit load"
    cat "$OUT"
    exit 1
fi
cat "$OUT"

# Static ownership audit: after normalizing module qualification, no predicate
# indicator may be defined by two different typecheck files.
OWNERSHIP_GOAL="use_module(library(prolog_xref)),
    Files=['src/typecheck/analysis_proofs.pl',
           'src/typecheck/builtin_registry.pl',
           'src/typecheck/clause_checks.pl',
           'src/typecheck/decl_store.pl',
           'src/typecheck/dependency_graph.pl',
           'src/typecheck/det_analysis.pl',
           'src/typecheck/det_builtins.pl',
           'src/typecheck/det_proofs.pl',
           'src/typecheck/det_validate.pl',
           'src/typecheck/flags_arrows.pl',
           'src/typecheck/inference.pl',
           'src/typecheck/oracles.pl',
           'src/typecheck/type_lang.pl',
           'src/typecheck/value_checks.pl'],
    maplist(xref_source, Files),
    findall(F/A-File,
            ( member(File, Files),
              xref_defined(File, Head, _),
              strip_module(Head, _, Plain),
              functor(Plain, F, A) ),
            Pairs0),
    sort(Pairs0, Pairs),
    ( select(PI-File1, Pairs, Rest),
      member(PI-File2, Rest),
      File1 \\== File2
      -> format(user_error,
                '[FAIL] split predicate ~q is defined in ~w and ~w~n',
                [PI, File1, File2]),
         halt(1)
      ; format('[PASS] each checker predicate has one owning file~n', []),
        halt )"

if ! swipl -q -g "$OWNERSHIP_GOAL" >"$OUT" 2>&1; then
    cat "$OUT"
    exit 1
fi
cat "$OUT"
