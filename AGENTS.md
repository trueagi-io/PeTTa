# Compile-Time Typechecking (typecheck-v2) — IMPLEMENTED

This branch implements compile-time typechecking for PeTTa from scratch. A previous
attempt (branch `compile-time-typechecking`, commit `a0e0746` plus uncommitted work in
that worktree) was abandoned after analysis; only its test corpus and design lessons
were salvaged. All six planned phases are implemented and green.

## What it does

MeTTa `(: f (-> A B))` declarations are checked at compile time: provably ill-typed
programs are rejected during translation (`literal_type_mismatch`, `type_conflict`,
`no_matching_overload`), and **no runtime type checks** are emitted into compiled
Prolog when all relevant types are fully resolved. Runtime `typecheck_or_error` /
`typecheck_match` guards remain only where types are genuinely unknown or polymorphic
at the call site; bound values are then checked through the user-extensible `get-type`
reflection (so runtime refinement types like `examples/types_dependent.metta` work).
The `--strict` flag requires every compiled function to have a declared or inferable
type and rejects any residual runtime type goal. Determinism arrows (`-[det]->`,
`-[nondet]->`) are parsed into the same store and validated (deterministic bodies,
non-overlapping clause heads).

Undeclared functions get local type inference (assumption type variables on the
clause parameters, bound by unification at typed call sites, with a provisional
arrow for self-recursion). Inferred types live in an internal store
(`inferred_fn_type/3`, never asserted into `&self`) and are used only to *add*
knowledge — eliminating guards, typing call outputs, satisfying strict mode —
never to reject a program: a static mismatch against an inferred type degrades to
the runtime guard the call would have had anyway. Conflicting uses taint the
assumption (recorded as `%Undefined%`); clause joins widen position-wise;
declarations supersede inference.

## Implementation map

- `src/typecheck.pl` — the type store (`declared_fn_type/4`, `declared_value_type/2`,
  seeded from `lib/lib_builtin_types.metta` at startup), the single compatibility
  relation `type_unify/2`, attributed-variable type channel (`tknown`/`treq`
  translation-time, `mreq` runtime), value typing, clause-level checks, strict mode,
  determinism validation.
- `src/translator.pl` — type-directed dispatch (`translate_typed_call`), typed clause
  translation, output-type propagation through translator constructs.
- `src/spaces.pl` hooks `add_sexp`/`remove_sexp` into the store; `forget_symbol` in
  `src/specializer.pl` forgets cached types.
- Error message wording lives in `src/main.pl` (`prolog:error_message//1`); the tests
  grep on it, so treat it as part of the spec.
- Type declarations must precede the definitions and calls they are meant to check
  (forms are processed in file order; later declarations only affect later forms).

## Tests

The former `examples_typecheck/` spec suite is folded into `examples/` and runs as
part of `sh test.sh`: `fail_*.metta` files must fail compilation with a
type/determinism error, `strict_*`/`fail_strict_*` run with `--strict`, and
`examples/type_dispatch_matrix.sh` makes precise assertions about generated code
(e.g. no `typecheck_match` goals in fully-resolved cases).

## Hard-won design rules from the abandoned attempt (kept honored here)

- **One type channel.** The old attempt threaded a `TypeCtx` list through every
  predicate AND returned a `Cands` list from `translate_expr`; reconciling them forced
  re-translating subexpressions, which was O(2^depth) in compile time (measured 7s at
  superpose nesting depth 14) and duplicated side effects (lambdas asserted twice).
  Use ONE mechanism. Attributed variables on the Prolog vars representing MeTTa vars
  are a natural fit: the type travels with the variable, branch merging falls out of
  copy/unify, and no predicate signatures change. Never translate the same
  subexpression twice.
- **One compatibility relation.** A single `type_compat/2` (`%Undefined%`/`Atom`/
  `Expression` as wildcards, primitives, `['List', T]`, arrows, vars) used everywhere.
  The old attempt had ~6 scattered variants plus ad-hoc carve-outs
  (`refinement_like_type_pair`) — that path leads to whitelist hell.
- **"Statically fails" is a translation result, not generated code.** Do not emit
  `throw(...)` goals and then pattern-match the generated Prolog to find them (the old
  `static_throw_goal` / `goal_needs_runtime_type_guard` scans missed goals nested in
  maplist/once/lambdas). Throw during translation, or return a marker.
- **The checker must not force the library to get less typed.** The old attempt made
  `lib/lib_roman.metta` annotations *weaker* (`Expression` instead of precise
  polymorphic types) to pass its own checks. If that starts happening, the
  inference/compatibility rules are wrong — stop and fix them instead. The precise
  annotations to adopt once polymorphism works:
  `git show a0e0746:lib/lib_roman.metta` in the main repo.
- Asserting inferred types into `&self` is user-visible (`match`, `get-type`). If
  inference records types, keep them in an internal store.
- Type declarations appearing after function definitions (order dependence) need an
  explicit decision: either re-check on declaration or document that types must
  precede use.

## Reference material

- Abandoned implementation: `git show a0e0746` (branch `compile-time-typechecking`,
  worktree `/nexus/Dev/OpenCog/PeTTa-compile-typechecking` with further uncommitted
  fixes — its `typecheck_support.pl` has workable leaf predicates like
  `decompose_function_type`, `partial_value_type`, `typecheck_match`).
- Full failure analysis: session notes summarized above; the measured problems were
  exponential re-translation, dual type channels, scattered compatibility logic,
  goal-scanning strict mode, and determinism plumbing tangled through everything.
