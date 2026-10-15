# Unified relational checker

This directory is being migrated from several syntax walkers to one analysis
pipeline.  The diagram and the invariants below describe the target design:

```
MeTTa clause
    -> relational IR
    -> flow-sensitive abstract interpretation
    -> function summary
    -> declaration/cardinality diagnostics and translation facts
```

In the completed design, the IR is the only component that understands source control forms.  Type,
shape, instantiation, and cardinality consumers must not recursively inspect
MeTTa source or generated Prolog independently.

## Domains

The checker keeps three independent kinds of information.

* `type(T)` is a value-type claim.  Examples are `type('Bool')` and
  `type(['List', T])`.
* Shape and instantiation facts describe the value available at a program
  point: `proper_bool`, `proper_list`, `nonempty_list`,
  `proper_list_length(N)`, `nonvar`, `ground`, and related facts.  A value of
  type `Bool` is not necessarily a
  `proper_bool`; it may still be an unbound relational variable.
* `card(Min, Max)` describes the number of solutions.  `card(1, 1)` is det,
  `card(0, 1)` is semidet, `card(1, many)` is multi, and `card(0, many)` is
  nondet.

This separation is intentional.  Neither a declared list type nor a declared
Bool type proves that the runtime value is instantiated.

## Flow

Every source variable receives a stable IR value ID.  Abstract states map IDs
to facts.  A successful match refines only its success edge; branch joins keep
only facts common to every reachable edge.  Consequently a constructor field
type is available while translating the successful arm of an `and-then`,
`if`, `case`, or `let`, without leaking into a fallback path.

Branch cardinality is computed from the same graph.  Exhaustive, mutually
exclusive branches use an interval hull, while ordinary relational choice is
additive.  `once` caps the upper bound and `collapse` constructs exactly one
proper list.

## Calls and function summaries

Builtin behavior is data in `call_summaries.pl`: guarded modes state their
required argument facts, result/argument postconditions, solution interval,
and effects.  Unknown calls remain unknown; absence from the table is never a
determinism proof.

In the target design, user functions are summarized from their clause IRs.  A summary contains at
least result facts, cardinality, effects, dependencies, and any boundary
requirements consumed by the proof.  Recursive groups are solved together to
a fixed point.  Call sites consume this summary instead of re-walking callee
source.  Thus a function whose every reachable result constructs `true` or
`false` exports `proper_bool`, regardless of whether it used `if`, `case`, or
another producer.

## Compatibility boundary

The core modules do not read attributed variables, dynamic declaration
tables, or generated Prolog.  During migration, one bridge may:

1. seed clause argument IDs from canonical declarations;
2. resolve constructor and function signatures;
3. project success-edge facts into the translator while that branch is
   compiled;
4. convert consumed assumptions into runtime boundary guards; and
5. convert analysis failures into the existing public diagnostics.

The bridge is temporary plumbing, not a second checker.  It may translate
facts and diagnostics, but it must not contain another recursive source walk.

## Implemented migration slice

The current bridge batches and lowers each parsed clause occurrence once, runs
fixed-point IR analysis over the prepared clauses, retains the final
per-occurrence analysis produced by that fixed point, exports `proper_bool`
result facts, and projects concrete types added specifically by a successful `if`,
`and-then`, or `or-else` condition into that branch's existing translator
context. Recursive components use a per-function worklist and revisit callers
only when the resolver-visible callee contract changes. Equivalent copies made
by dependency recompilation reuse an aligned prepared record while its file
scope exists.

Recursive result-shape properties use a separate greatest-fixed-point pass.
For each finite candidate fact, such as `proper_bool` for a declared `Bool`
result, the checker provisionally assumes that one fact inside the recursive
component, analyzes every clause, and monotonically removes functions which do
not preserve it. The declaration proposes a candidate but never proves it;
one open or unsupported result removes the fact transitively. Cardinality,
effects, and productivity remain products of the ordinary solver and are not
accepted coinductively.

Closed, ground function summaries are cached between source batches in the
same SWI process. Cache rows contain no source variables, clause records, or
IR; they carry a separate ground dependency set for clause sets, declarations,
effects, constructors, aliases, callable classification, and callee summaries.
Semantic mutation invalidates matching rows and their transitive summary
dependents. Recompilation always rebuilds its roots but stops traversal at a
valid cached callee. A solved batch is published atomically only after source
translation succeeds. Nested imports invalidate the enclosing batch
generation, and a failed runtime add restores the exact pre-transaction source
occurrences and closed cache slice.

If recompilation has no usable cache row after an ephemeral file scope has
ended, the bridge freshly lowers the current stored direct-call closure, solves
its summaries (including recursive groups), and uses the retained analysis for
the target clause without seeding it from stale summaries.
Compiler-generated lambda clauses receive their own nested ephemeral analysis
record, so facts from a deferred body cannot escape into the enclosing clause
and facts from one lambda branch cannot certify another. Unsupported lowering
falls back to the legacy checker per clause; unsupported IR nodes preserve no
child-flow facts because their children are not yet known to execute.

The analyzer also closes `proper_list` plus exclusion of `()` to
`nonempty_list`, represents variable-headed fixed-width list patterns as
positional patterns, and records `decons`'s exact two-field result. During the
migration, the legacy determinism walker may consume the retained cardinality
of an exact registered-builtin source occurrence; user-function cards are not
used for that purpose because they can still contain the declaration being
validated.

The legacy determinism walker, whole-clause exhaustiveness checks, contextual
`case`/`let` pattern checker, and runtime-boundary guards remain authoritative
where the IR analyzer has not reached parity.  Current summary cardinality is
a call contract for recursive analysis, not yet an independent proof of the
declaration. Consumed boundary requirements are not yet exported as a complete
function contract.

The cache is process-local: a fresh `run.sh` process still parses, lowers, and
solves first-seen source. Invalidation also happens before recomputation, so a
changed leaf currently evicts transitive callers even when its newly derived
public summary is identical. Stopping propagation on an unchanged export, and
persisting canonical source/summary fingerprints across process restarts, are
separate incremental-compilation milestones.

## Migration invariants

* Lower each clause once and cache the IR by clause identity.
* Analyze source IR, never generated Prolog.  Prolog loses the distinction
  between source evaluation, data construction, and match refinement.
* Keep one compatibility relation for value types.
* Treat unknown and unsupported operations conservatively.
* Do not discharge a shape or instantiation precondition from a value type.
* Record every assumed boundary fact as an explicit requirement in the proof.
* Invalidate function summaries when a declaration, clause set, or builtin
  summary they depend on changes.
* Retire old syntax walkers only after their behavioral tests pass through the
  IR path; generated-Prolog grep tests are backend tests, not checker tests.
