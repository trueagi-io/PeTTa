# Compile-Time Typechecking — Fresh Attempt (typecheck-v2)

This branch is a **fresh start** on compile-time typechecking for PeTTa. A previous
attempt (branch `compile-time-typechecking`, commit `a0e0746` plus uncommitted work in
that worktree) was abandoned after analysis. Its test corpus is salvaged here as an
executable spec; its implementation is deliberately NOT carried over.

## Goal

Check MeTTa `(: f (-> A B))` declarations at compile time: reject provably ill-typed
programs during translation, and emit **no runtime type checks** into compiled Prolog
when all relevant types are fully resolved at compile time. Runtime guards are only
acceptable where types remain genuinely unknown or polymorphic at the call site. An
optional `--strict` flag requires every compiled function to have a declared or
inferable type and rejects any residual runtime type goal.

## The spec suite

- `examples_typecheck/` — 39 salvaged test files. `fail_*.metta` must fail compilation
  with a type/determinism error; the rest must pass; `strict_*` / `fail_strict_*` run
  with `--strict`. `type_dispatch_matrix.sh` makes precise assertions about generated
  code (e.g. no `typecheck_match` goals in fully-resolved cases).
- `sh test_typecheck.sh` runs everything and reports N/M passing — it does not abort on
  first failure, so it doubles as a progress meter. `sh test_typecheck.sh 'fail_*'`
  runs a subset.
- The regular regression suite `test.sh` must stay green at all times; the spec suite
  starts red and goes green phase by phase. When the feature is complete, fold
  `examples_typecheck/` into `examples/` and port the expected-fail handling into
  `test.sh` (the abandoned worktree's `test.sh` already has that harness to copy).
- Error message wording lives in `src/main.pl` (`prolog:error_message//1`); the spec
  greps on it, so treat it as part of the spec.

## Implementation phases (each lands with its spec subset green, test.sh green)

1. **Type store.** Canonicalize and cache `(: f ...)` declarations when they are added
   to the space — one dynamic predicate like `declared_fn_type(F, ArgTypes, OutType, Det)`,
   prefix arrow form only. Never re-query `match('&self', [':', F, _], ...)` per
   expression node. Seed builtins from `lib/lib_builtin_types.metta`, do not hardcode a
   second table. Handle redefinition/forgetting (see `forget_symbol` in specializer.pl).
2. **Monomorphic checking.** Single, non-polymorphic declaration: check literal and
   known-typed arguments at call sites during translation, throw
   `literal_type_mismatch` / `type_conflict` at compile time, emit plain calls with no
   guards. Check declared output type against the clause body's inferred type.
3. **Polymorphism and partial application.** Type variables via `subsumes_term`/
   unification on copies; `['List', T]` element checking; `partial(F, Bound)` values
   get the remaining-arrow type.
4. **Overloads.** If exactly one declaration statically survives, compile a direct
   call. Otherwise compile the guarded disjunction (pre-branch behavior) and throw
   `no_matching_overload` only when no branch can apply.
5. **Strict mode.** `--strict` flag: `strict_missing_function_type` when a compiled
   function has no type; `strict_runtime_typecheck` when a residual guard would be
   emitted.
6. **Determinism arrows** (`-[det]->`, `-[nondet]->`) — separate feature, last, ideally
   its own PR. Parse the infix arrow into the same canonical store plus a det flag.

## Hard-won design rules from the abandoned attempt (do not rediscover these)

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
