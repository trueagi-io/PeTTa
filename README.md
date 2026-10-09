## PeTTa

Efficient MeTTa language implementation in Prolog.

Please check out the [Wiki](https://github.com/patham9/PeTTa/wiki) for more information.

### Dependencies

- SWI-Prolog >= 9.3.x
- Python 3.x (for janus Python interop)

### Usage

Example run:

`time sh run.sh ./examples/nars_tuffy.metta`

Several `.metta` files may be named, and they are loaded in exactly the order
given, followed by any flags:

`sh run.sh ./examples/a.metta ./examples/b.metta --strict`

Load order is semantically significant — a declaration or a constructor that
arrives in a later file cannot retroactively inform code already compiled from
an earlier one.

### MORK and FAISS spaces

If MORK and FAISS is installed, execute `sh build.sh` to support MORK-based atom spaces and FAISS-based atom-vector spaces.

The following projects are cloned and built by build.sh:

**Repository:** [mork_ffi](https://github.com/patham9/mork_ffi) dependent on [trueagi-io/mork](https://github.com/trueagi-io/mork)

**Repository:** [faiss_ffi](https://github.com/patham9/faiss_ffi) dependent on [facebookresearch/faiss](https://github.com/facebookresearch/faiss)

### Extension libraries

Please check out [Extension libraries](https://github.com/trueagi-io/PeTTa/wiki/Extension-libraries) for a set of extension libraries that can be invoked from MeTTa files directly from the git repository.

### Compile-time typechecking

PeTTa checks `(: f (-> A B))` declarations during translation: provably
ill-typed programs are rejected at compile time with the file, line, and
offending form, and fully resolved calls compile to plain Prolog with **no
runtime type checks**. Runtime guards remain only where types are genuinely
unknown at the call site; they check bound values through the user-extensible
`get-type` reflection, so runtime refinement types keep working.

```metta
(: inc (-> Number Number))
(= (inc $x) (+ $x 1))

!(inc "oops")   ; rejected at compile time: Type mismatch: got "oops" but expected 'Number'
```

Types make code *faster*, not slower: resolved arithmetic fuses to native
`is/2` and comparisons fuse into if-conditions, so an annotated (or inferred,
see below) `fib` compiles to the same clause a Prolog programmer would write
by hand — about twice as fast as the untyped translation was before.

**Inference.** Undeclared functions get local type inference (parameters are
inferred from how the body uses them, including through self-recursion).
Inferred types only ever *add* knowledge — they eliminate guards, type call
outputs, and enable the fused code generation — and, unlike a declaration,
they are never *demanded* of callers. How a body uses a parameter is not what
it requires of one: in

```metta
(= (score $current $cand) (if (== $current none) $cand (max $cand $current)))
```

`$current` is inferred `Number` from the branch the sentinel test rules out,
so `(score none 0.42)` is a correct program and runs — a declared parameter
type would (rightly) reject it. Inference stays silent wherever the compiler
cannot see that a value is definitely of another type: no compile error, no
strict-mode rejection and no runtime guard, so nothing fails that would not
have failed without inference (`examples/inferred_sentinel_param.metta`). An
argument it *can* see is definitely mismatched still fails, at the call site's
guard, because inference already elided the checks inside the callee
(`examples/fail_inferred_literal_mismatch.metta`). Declared types are
unaffected — they are checked, and rejected, exactly as before
(`examples/fail_declared_sentinel_param.metta`).

A variable bound by a *destructuring* head pattern is a parameter too, and is
inferred the same way: `(= (tvf (stv $s $c)) (* $s $c))` infers `(-> (stv
Number Number) Number)` and multiplies without guarding, where before only a
plain `$s`/`$c` parameter did. The pattern's own type is rebuilt from its
fields' — that shape is what keeps `(tvf (stv "a" "b"))` an error rather than a
silently wrong number — and is kept only when it reads back as the shape it was
built from, so a pattern headed by a declared *type name* (which reads
positionally, see **Tuple types** below) simply infers nothing. A pattern
headed by a uniquely declared constructor takes its field types straight from
that declaration, whether it appears in a head, a `let`/`let*` binder or a
`case` branch, and needs no type for the scrutinee to do it. See
`examples/strict_destructured_param.metta`,
`examples/destructured_param_shapes.metta` and
`examples/fail_destructured_param_mismatch.metta`.

**Match schemas.** Space reads are typed through declared relation schemas:
with `(: age (-> String Number Atom))` declared, the pattern variables of
`(match &self (age $who $n) ...)` acquire `String` and `Number`, so match
results are fully typed and guard-free code is generated for the body. Type
queries like `(match &self (: $x Fruit) $x)` bind `$x : Fruit` directly.

**Strict mode.** `sh run.sh file.metta --strict` additionally requires every
compiled function to have a declared or inferable type and rejects compilation
if any residual runtime type guard would be emitted — a machine-checked
guarantee that the compiled program contains no *implicit* runtime type checks.

**Branches are merged conservatively.** When `if`, `case`, `let`/`let*`,
`superpose`, `hyperpose` or `collapse` merge several alternatives into one
value, that value's type is what **every** alternative is known to produce.
One alternative of undetermined type — a field of an `Expression`, a call to
an untyped function — makes the merged value undetermined, whatever the other
alternatives contribute:

```metta
(: f (-> Expression Bool Number))
(= (f $x $c) (need (if $c 0.1 $x)))     ; not a Number: $x is arbitrary data
```

The `0.1` branch is not evidence about the `$x` branch, so this costs a
runtime guard where the value is used, and `--strict` rejects it. See
`examples/fail_strict_branch_unknown_arg.metta`,
`examples/fail_strict_branch_unknown_output.metta` and
`examples/fail_strict_collapse_unknown_elem.metta`.

**A type variable in a declaration is a promise.** `(: sumh (-> (List $a)
Number))` says *sumh works for a list of any element type* — so no call site is
checked on the element, at any depth. A body that needs a particular element
type has therefore not proved anything: every call site gets a fresh copy of
the declaration, so a binding the body makes reaches nobody. It does not
*check* the element type, it *elides* the check.

```metta
(: sumh (-> (List $a) Number))
(= (sumh (cons $h $t)) (+ $h 1))   ; rejected: $a is used as Number here
```

Such a body is a compile error naming the type it actually needs — write
`(-> (List Number) Number)` and the call sites are checked again. This applies
to every type variable in the argument types, nested ones included: before, a
variable buried in `(List $a)` or in a closure parameter `(-> $a Number)` was
exempt, and `(sumh (cons "x" ()))` compiled to zero runtime checks under
`--strict` and printed `121` — SWI reads a one-character string as its
character code. See `examples/fail_nested_parametric_param.metta` and
`examples/fail_parametric_closure_arg.metta`.

The reading side follows: a promised variable is not evidence *about* a value
either. The result of applying a `(-> Number $b)` closure parameter has type
`$b`, which is whatever the caller's function returns, so certifying it as the
declared `Number` output is the same "I don't know" read as "compatible with
everything" that the branch merge above rules out. It costs a runtime guard,
and `--strict` rejects it
(`examples/fail_strict_closure_output_promise.metta`). A type variable that is
genuinely universal — an output variable occurring in no argument type, which
by parametricity only a bottom function like `(: empty (-> $a))` can produce —
is unaffected, and so is a bare `(-> $a $b)` closure applied inside a
polymorphic function.

**Type ascription.** Genuinely dynamic values (reads from schema-less
relations, `catch` results, `eval`) can be given an author-stated type with
`(the Type Expr)`: the checker treats the type as knowledge for everything
downstream and emits one explicit, visible runtime check at the boundary —
permitted even under `--strict`. An ascription that contradicts what the
checker already knows is a compile-time error.

```metta
(: greet (-> String))
(= (greet) (the String (match &self (name $n) $n)))
```

**Auditing runtime checks.** `--warn-runtime-checks` (independent of the other
flags) prints a warning for every runtime type check the compiler emits —
implicit residual guards and explicit `(the ...)` ascriptions alike, each with
the containing function and the checked type. Combined with `--strict` (which
already forbids the implicit ones) a warning-free compile certifies that the
program contains no runtime type checks at all.

**The list builtins are typed contextually, not declared.** A global
signature for `cons`, `append` or `car-atom` would have to name an element
type, and that would reject the legal heterogeneous expressions MeTTa is built
out of. So they carry no declaration and the checker derives their result from
the argument at each call site instead: the constructors (`cons`, `cons-atom`,
`union-atom`, `append`, `subtraction-atom`, `list_to_set`) yield the list type
their operands agree on, and the accessors (`car-atom`, `first`, `last`) yield
that list's **element** type, with `cdr-atom` yielding the list type again. A
declaration would defeat this rather than help it: it is consulted *first*, so
`(: cdr-atom (-> $expression (List %Undefined%)))` did not merely fail to
narrow, it replaced what the argument knew — `(car-atom (cdr-atom $xs))` on a
`(List Choice)` came back untyped, needed a runtime guard on every later use,
and `--strict` rejected the guard it could not discharge. Whether an accessor
*succeeds* is a separate question, and belongs to the determinism table — with
one caveat the certification forces: `(car-atom ())` **raises** (an empty
expression has no head), because the certified element type leaves the runtime
no licence to answer `()` where a `Number` was proven. `first` and `last`
simply fail on input they have no answer for, which threatens no
certification. See `examples/strict_list_accessor_types.metta` and
`examples/fail_strict_car_atom_empty.metta`.

**Structural tuple types.** A parenthesized type describes an expression, in
one of two readings — and which one you get is decided by the *declaration* of
its head atom, not by how the atom is spelled:

- **Tagged** — the value must carry the head as a literal atom. Chosen when the
  head is a declared constructor of matching arity, or when it has no type
  declaration at all. `(STV Number Number)` with `(: STV (-> Number Number TV))`
  declared, and `(Stats Number Number Number)` with `Stats` undeclared, both
  describe the three-element expressions `(STV 0.5 0.8)` and `(Stats 1.0 2.0 3)`.
- **Positional** — an n-element record whose i-th element has the i-th listed
  type, head included. Chosen when the head is not an atom (`($value Number)`),
  is a primitive or wildcard type (`(Number Number)`), or is an atom declared as
  something *other* than a constructor of that arity — typically a type name:

```metta
(: Statement Type)   (: KBContext Type)   (: TV Type)

(: fact-conf (-> (Statement KBContext TV) Number))   ; three fields
(= (fact-conf ($_statement $_context $tv)) (tv-conf $tv))
```

So: declare the field types of a positional tuple, and leave a tag undeclared
(or declare it as a constructor). See `examples/strict_tuple_types.metta` and
`examples/strict_positional_tuple_types.metta`.

**Explicit data construction.** `(data E1 E2 ...)` constructs the expression
whose fields are the evaluated values of `E1`, `E2`, and so on; `data` itself
is erased. In particular, the first field is always data, even when it is an
unbound variable or later holds a function:

```metta
(: pair (-> Atom Number (Atom Number)))
(= (pair $head $value) (data $head $value))
```

This is the unambiguous form for positional tuples with a computed head.
Existing parenthesized forms retain their call-or-data behavior.

Because `list` is already used as data by an existing example,
`(make-list E1 E2 ...)` is the explicit runtime-list constructor. Under an
expected `(List T)`, each element is checked against `T` at compile time;
elements of `(List Expression)` remain unevaluated source data. The same list
expectation flows through `cons`/`cons-atom` chains and the empty list `()`.

When a function declares a positional tuple result, that expected shape is
propagated into `(data ...)` fields in result position, including through
`if`, `case`, `let`/`let*`, and `chain`. Every field is checked against its
position before the result is certified, so a mismatch is a compile-time
error and an unresolved field remains a residual check (and is rejected by
`--strict`). This is deliberately narrower than general bidirectional
inference. One focused higher-order rule also applies: at a call with a unique
declaration, closure arguments resolve shared type variables before the
remaining arguments are translated. Positional accumulator or state arguments
therefore receive contextual field typing, including when the closure is
partially applied. When a `(data ...)` initializer is staged in an earlier
`let*` binding, the same lookahead applies to any such call and carries that
expectation back to its producer. A variable already carrying the required
nested product type is accepted directly; it does not need a repeated
`(the Type ...)` ascription. See
`examples/strict_contextual_data_tuple.metta` and
`examples/strict_nested_product_parameter.metta`.

The as-pattern `(@ Whole InnerPattern)` is transparent to this typing:
`Whole` receives the complete matched type and `InnerPattern` is destructured
against that same type. This also works when `@` is nested below a `cons`
pattern, so positional field types are retained in list-head destructuring.

**Union types.** A heterogeneous position can declare its alternatives with
`(| T1 T2 ...)` — for example `(List (| (CPU %Undefined% %Undefined%
%Undefined%) (: %Undefined% %Undefined% %Undefined% TV)))` for a mixed
execution list. A value fits a union if it fits some member, and `case` or
clause-head patterns narrow to the member their shape selects, typing the
pattern's variables. Unions are declared, never inferred.

A *tagged* pattern narrows by its tag. An *untagged* one — `($type $ctx $prf
$tv)` — only counts elements, which by itself proves nothing: another member's
constructor may build a value of exactly that width. Such a pattern narrows
only when every other member is ruled out, either because no constructor of it
has that width, or because an earlier branch of the same `case` already
consumed all of them (`case` is first-match: the branches compile to a nested
if-then-else, so a value matched earlier never reaches a later branch). Move
the untagged branch first and the narrowing is refused. Both directions are
pinned by `examples/strict_union_prior_branch_narrowing.metta` and
`examples/fail_strict_union_var_branch_first.metta`.

*Constructor* here means what it means everywhere else in PeTTa: a declared
symbol with **no** equations, which therefore stays literal data. A declared
symbol that has equations is a function — `(make-goal $f $a $r)` is rewritten
at the call site and no such expression ever survives as a value — so a helper
that merely *returns* the type does not count as one of its constructors and
does not block an exclusion. Give that same helper a declaration but no
equation and it becomes a genuine constructor that does block. See
`examples/strict_union_reducible_helper_not_ctor.metta` and
`examples/fail_strict_union_undefined_second_ctor.metta`. Definition order
inside one file is irrelevant (definedness is recorded in the parse prepass).
The exclusion does read the constructor set as it stands at that point in the
compilation, but that snapshot is now recorded and honoured — see "Knowledge
that arrives late" below.

**Erased nominal newtypes.** `(: KB (Newtype Expression))` declares a
distinct compile-time role over an existing representation: nothing is
wrapped at runtime and no guards are emitted. A branded value fits its
representation, but different brands never unify merely because their
representations do — swapping a `Proof` into a `KB` position is a compile
error. The representation is an *upper bound* on what the brand can be used
as, never a licence to be anything: a **wildcard** representation
(`(: Proof (Newtype Expression))`) says the payload shape is unconstrained,
which is not the same as saying it fits every type, so such a brand fits
wildcards and itself and nothing else. Nor does anything implicitly fit a
brand, wildcards included — a value acquires a brand in exactly one way, by
being written `(brand T v)`. See `examples/fail_newtype_wildcard_leak.metta`.
Raw literals and constructed values acquire a brand contextually from
the expected position; an unknown variable does not (under `--strict`) —
brand it explicitly with `(brand KB $x)`, an erased trust operation that
rejects conflicting brands but generates no check (a role has no runtime
witness). Declared relation schemas restore brands on `match`. Use
`(the KB ...)` instead when you want the representation checked at runtime.

**Structural type aliases.** `(: Row (Alias (Number String)))` gives a
structural name to any type expression. The name is expanded when a
declaration is processed, so it adds no runtime tag and no nominal
distinctness — unlike `Newtype`, `Row` is exactly `(Number String)`. Aliases
should normally be declared before use and may compose with other type forms,
such as `(List Row)`. Because the checker retains the expanded representation,
error messages show that representation rather than the alias name. If an
alias does arrive late, prior declarations are re-normalized and any
already-compiled functions that use them are recompiled against the expanded
types.

**Opaque foreign types.** `(: Agenda (Foreign))` declares an opaque type for
values produced by native or foreign code; `(: Heap (Foreign 1))` declares a
type constructor used as `(Heap T)`. Foreign types are tracked statically by
name and optional parameter arity, erased at runtime, and never value-checked
or structurally inspected. Unlike a bare type variable such as `$heap`, a
foreign type does not unify with unrelated types.

**Typed spaces.** `(: &jobs (SpaceOf (job-row Number String)))` declares the
row type of a statically named space. Declaring the schema validates rows
already present in the space and rejects any definite contradiction. Its
`match` patterns receive the row's field types, including narrowing when the
schema is a union of row shapes; updates through `add-atom` and `remove-atom`
are likewise prevalidated for definite mismatches. Open rows and patterns
whose fields are filled dynamically remain trusted, and spaces without a
`SpaceOf` declaration retain their existing untyped behavior.

**Determinism arrows.** `(: f (-[det]-> A B C))` — prefix, like `->` and
every other MeTTa form — declares a deterministic function: the compiler validates
that its clauses cannot overlap and its body is deterministic, then commits to
the first matching clause — guaranteeing choicepoint-free execution and
constant-memory deep recursion via last-call optimization
(see `examples/determinism_lco.metta`). `-[nondet]->` documents intentional
nondeterminism. Result cardinality is a total order: `-[det]->` (exactly one)
< `-[semidet]->` (zero or one) < `-[nondet]->` (any), and a closure fits an
arrow that allows at least as many results as it can produce.

**Effect-polymorphic arrows.** `-[$e]->` gives one declaration a bounded
cardinality variable. For example,
`(: map (-[$e]-> (-[$e]-> $a $b) (List $a) (List $b)))` says that `map` has the
join of its own intrinsic effect and the effect of the supplied closure:
passing a det closure can prove a det call, while passing a nondet closure
makes the call nondet. If several direct closure parameters use `$e`, their
effects are joined (`det < semidet < nondet`). An unknown closure effect leaves
the call effect unknown.

Version 1 permits one effect-variable name per declaration. It may occur only
on the top-level declared arrow and on direct arrow-typed parameter positions;
it may not be nested inside those parameter arrows or appear in an output
type. A top-level effect variable must occur on at least one closure parameter,
so every call can instantiate it. Effect-polymorphic functions compile without
commit cuts: the conditional cardinality proof belongs to each call site, not
to one globally specialized body. See
`examples/strictdet_effect_polymorphism.metta`.

**Inferred closure determinism (in every mode).** A closure over an *undeclared*
function — an inline `|->` lambda, or a bare/underapplied reference to a function
with no arrow declaration — has no written arrow, so the compiler derives one
from the same clause-set analysis: a body that provably yields exactly one result
gets `-[det]->`, one that may fail but never branch gets `-[semidet]->`, and
anything unproven claims nothing. That proof is worth the same in every mode:
`--strict-det` exists to force a determinism claim out of code that made none,
not to be a precondition for checking one you already wrote. So passing an inline
det lambda to a `-[det]->` parameter resolves at compile time under plain
`--strict` — no residual runtime check for `--strict` to reject. It is a proof,
not a rubber stamp: a `superpose` body still does not fit. See
`examples/strict_det_closure_inferred.metta`,
`examples/strict_semidet_closure_inferred.metta` and
`examples/fail_strict_nondet_closure_inferred.metta`.

**Determinism of builtins.** A builtin — a registered symbol backed by a
Prolog predicate rather than MeTTa equations — is deterministic only if the
compiler's table says so, and that table was written by reading the
predicates. Everything not in it claims nothing, so a `-[det]->` body calling
it is rejected. `(get-atoms &self)` is the reason: it enumerates a space one
solution per atom, and a body using it is not deterministic no matter how the
result is discarded (`examples/fail_det_nondet_builtin.metta`,
`examples/fail_semidet_nondet_builtin.metta`).

Each entry is read under the calling convention that actually holds, which
assumes **nothing** about the arguments: any of them may be unbound, and any
may be of the wrong type. Nothing the compiler emits rules either out — the
residual guard is `typecheck_or_error/2`, whose variable branch *records* a
requirement rather than testing one, so it succeeds on an unbound variable. A
declared `Bool` therefore never implies a bound boolean, and a declared
`(List T)` never implies a proper list; an unbound argument of a declared type
comes straight out of well-typed code, e.g. the unfilled field of `(B $u)`.
An exception is not a solution, so a predicate that *raises* on a mode it
cannot serve is still `det`; one that *fails* is at best `semidet`, and one
that enumerates is `nondet`.

Read that way, arithmetic, comparison, reflection, the cell operations
(`cons`, `car-atom`, `cdr-atom`) and `sort-atom`/`unique-atom`/`sort`/`msort`
are `det` — each raises rather than guessing. But the operations that
**invert** are not, and there are more of them than the table used to admit:
`and`/`or`/`not`/`xor`/`implies` enumerate a boolean they were not given
(`bool/1` is two facts), `index-atom` enumerates when the index is unbound,
and `size-atom`, `union-atom`, `subtraction-atom`, `intersection-atom`,
`exclude-item`, `alpha-unique-atom`, `append`, `reverse`, `last` and `length`
all enumerate over an open list — `(size-atom $u)` does not terminate. Those
modes are used deliberately (`examples/booleansolver.metta`,
`examples/logicprogset.metta`, `lib_roman`'s `mylast`/`init`/`rcons`), so they
are recorded `nondet` rather than suppressed. `random-int`, `bind!`,
`get-metatype`, `add-atom` and `remove-atom` can produce **zero** results —
`random_between/3` fails when `Max < Min`, `bind!` only matches a
`(new-state ...)` second argument, `get-metatype`'s clauses do not cover a
partial application, and `add-atom` needs an expression — so they are
`semidet`, and a `-[det]->` body promising exactly one result may not call
them either. `match`, `get-type`, `member`, `is-member` and `callPredicate`
stay nondet, and the higher-order builtins (`maplist`, `foldl`, `map-atom`,
`filter-atom`, `foldl-atom`) have no table entry at all, because their
determinism is their closure's and a (name, arity) table cannot say so.
`examples/determinism_builtins.metta` pins the honest arrow for each family;
the `fail_det_*` examples pin one rejection per cause.

**Foreign promises.** A determinism arrow declared for a symbol with **no
MeTTa clauses** — a Prolog predicate — is a *trusted promise* about code the
analysis cannot read, the same standing `-[nondet]->` on a reflective wrapper
has. `callPredicate` stays nondet by default, but with a manifest goal —
`(callPredicate (Predicate (g A1 .. An)))` — it reads `g`'s declared arrow at
that arity: `lib_builtin_types` ships `(: assertz (-[det]-> $c $r Bool))` and
`(: erase (-[det]-> $r Bool))`, and user code declares further predicates the
same way (`examples/strictdet_callpredicate_declared.metta`; an undeclared
goal keeps the honest nondet,
`examples/fail_strictdet_callpredicate_undeclared.metta`). These promises are
believed, not validated, and the cardinality oracle does not wrap inner
Prolog goals — a wrong arrow here is the author's, exactly as in the flat
table.

**Higher-order builtins take it from the closure.** `map-atom`, `filter-atom`
and `foldl-atom` come in two forms and both are live: the *pseudo-lambda* form
`(map-atom $l $x Body)`, which the translator rewrites inline, and the
*closure* form `(map-atom $l $f)`, which is what `src/metta.pl` defines —
`'map-atom'/3`, `'foldl-atom'/4` and `'filter-atom'/3`, i.e. two, three and two
MeTTa arguments plus the result. Either way the construct is exactly as
deterministic as what it is handed, so the closure argument has to carry det
evidence the same way it does at a user-written higher-order call site: an
explicit `-[det]->` or an inline lambda with a deterministic body. A closure
parameter therefore has to be declared as an arrow, and under `--strict-det`
that nested arrow must be explicit too:
`(: for-each-in-atom (-[det]-> $l (-[det]-> $a $b) %Undefined%))`, not
`(-[det]-> $l $f ...)`, which says nothing about `$f` and leaves the wrapper
unprovable. See `examples/strictdet_higher_order_builtins.metta`. Both forms
require a proper list input. A direct committed parameter earns that proof
through a proper-list boundary proviso; an open or partial list is rejected
before the traversal can enumerate list shapes.

**The table outranks a declaration.** `lib_builtin_types.metta` gives many
builtins a type, while `det_builtins.pl` supplies their authoritative effect.
A declared builtin therefore takes its arrow head from the table both as a
value and at a direct call; a type signature cannot certify a nondeterministic
builtin as deterministic in closure position. See
`examples/fail_strictdet_nondet_builtin_closure.metta`.

**Argument-aware verdicts.** A (name, arity) table has to give one worst-case
answer for every call site, and most of the weak entries above are weak for a
*shape* reason: `length/2` and `append/3` invert over an open list, `min_list/2`
has no answer for `()`, `bool/1` invents a boolean it was not given. Where the
shape is manifest in the source, the reason does not apply and the call site
gets the stronger verdict — `(min-atom ($a $b))` is `det` because a
two-element list literal cannot be empty, and `(and (> $x 0) (< $x 10))` is
`det` because both operands are already booleans. What counts as manifest is
deliberately narrow — only a spine the compiler itself builds at the call
site: a list literal whose head is data (recursively, so `((c) 1 2)` with a
constant `c` counts but `((foo) 2 3)` does not — that is an *application*,
and its result is whatever the closure returns), or a `cons` onto a
manifestly proper tail. A **declared** type never qualifies, not even a
fixed-width tuple like `(Number Number)`: the residual guard succeeds on an
unbound variable, so no type implies a bound value, and such a parameter can
arrive unbound out of ordinary well-typed code. Anything else falls back to
the flat table, the same provable-only
discipline the `-[det]->` exhaustiveness check uses. This strengthens
`min-atom`, `max-atom`, `size-atom`, `length`, `reverse`, `last`, `append`,
`union-atom`, `subtraction-atom`, `intersection-atom`, `exclude-item`,
`alpha-unique-atom`, `index-atom` and the five boolean operators.
`examples/strictdet_builtin_arg_shapes.metta` pins the strengthened cases and
`examples/fail_strictdet_min_atom_unknown_shape.metta` pins the fallback.

**An output-properness certificate crosses the clause boundary.** The manifest
judgement above reads a *spine the caller builds*, so it goes blind the moment a
proper list is produced by a *call* — a helper that wraps `(collapse …)` returns
a bound proper list, but at its call site only the declared `(List Number)`
output type is visible, and a declared type never implies properness (a `det`
function may still return an unbound variable). The checker closes that gap with
a per-function certificate: `proper_list_output(F, N)` holds when **every** clause
of `F/N` provably results in a bound proper list — a `collapse` form (which
compiles to `findall/3`, always a bound proper list), a literal proper-list
spine, or, same file only, a call to an already-certified function. It is derived
per clause during translation and is "all clauses qualify": one non-certifying
clause withdraws the whole certificate (a clause that returns a `(List _)`
*parameter* does not qualify, for the same reason a declared type never does).
`manifest_proper_list` then accepts `(G …)` when `G/N` is certified, so
`union-atom` (and `append`/`subtraction-atom`/`intersection-atom`/`exclude-item`/
`size-atom`/`length`/`reverse`, which all consult it) become `det` on such a call.
**Nonempty is not implied** — `collapse` can yield `()` — so the certificate feeds
only `manifest_proper_list`, never `manifest_nonempty_list`.
`examples/strictdet_collapse_proper_list.metta` pins the certificate (and its
transitivity within a file) and `examples/fail_strictdet_collapse_proper_list.metta`
pins the boundary where one non-certifying clause withdraws it. Invalidation is
transitive for runtime clause changes: a late clause of a certified `F`
clears the certificate memo and recompiles recorded callers, while
`recompile_function_clauses` / `forget_symbol_types` also clear the local
certificate state.

**Boundness enforcement at a committed boundary, need-based.** The paragraph
above says a *declared* type never qualifies, because "typed" does not imply
"bound" — a `(Number Number)` or `Bool` parameter can arrive unbound out of
ordinary well-typed code (`(P $u)` leaves its field unfilled), and no residual
guard rules that out. An explicit `-[det]->`/`-[semidet]->` arrow can change that
at its own boundary, but only where the commitment actually depends on it: the
compiler emits a `nonvar` check for a parameter **only when the clause's
determinism proof consumed its boundness** — when one of the strengthenings
below treated it as an enforced-bound direct parameter. The check emitted is
then exactly the proviso the certificate relied on. A pure data constructor
(`(= (pair-up $x $y) ($x $y))`) is genuinely `det` with unbound arguments,
consumes no boundness, and gets **no** check — it compiles and runs with an
unbound argument, the variable flowing through into the result
(`examples/det_constructor_unbound_arg.metta`). Where a check *is* emitted,
passing an unbound argument throws a clear `unbound_det_argument` error
(`examples/fail_unbound_det_argument.metta`, whose body `(and $b true)` consumes
its parameter's boundness) — where the same call used to enumerate a finite type
through `bool/1` or crash downstream inside a builtin's `=..`. The consumed
positions are unioned across the function's clauses, so a parameter any clause
relies on is checked in every clause — a sound superset, since an extra `nonvar`
check is never wrong. Explicit `-[det]->`/`-[semidet]->` enforcement is
**mode-independent**; plain arrows remain uncommitted where they are legal.

Because "typed implies bound" now holds for those parameters, five call-site
verdicts that a bare declared type could not earn become sound when the
argument is such a parameter: `and`/`or`/`not`/`xor`/`implies` over `Bool`
params, `remove-atom`/`add-atom` over a **nominal**-typed param (every value is
a constructor application, a nonempty list spine), `is-member` with a bound
probe against a ground duplicate-free literal, and `min-atom`/`max-atom` and the
list operations over a fixed-width **tuple** param — the `(Number Number)` case
the flat table had to drop, re-admitted because its only objection was unbound
arrival and the boundary check removes exactly that.

The check is **spine-level, deliberately**. A non-variable (destructuring)
parameter like `(P $u)` is bound as a whole by head unification, so it is
skipped — and its field `$u` is **not** checked. Chainer proof terms
legitimately carry unbound variables inside otherwise-bound data, and
field-level enforcement would reject them. The honest asymmetry is that a field
does **not** count as bound: `(= (f (P $u)) (and $u true))` under `-[det]->`
still rejects, because `$u` can arrive unbound even though `(P $u)` did not
(`examples/fail_det_field_in_and.metta`). The strengthenings mirror the check
exactly — they key on *direct* parameters, never fields.

**The `is-var` exemption.** A parameter the body tests with `is-var` gets no
boundary check: the author wrote exactly the branch the check would preempt,
and throwing before their handler runs would be the compiler overruling them
(`examples/isvar_det_param.metta` — a `-[det]->` function that answers `0` for
an unbound argument instead of throwing). The exemption cuts both ways, from
one shared list: such a parameter also stops counting as enforced-bound, so
the body must prove its determinism *with* a possibly-unbound value —
`(and $b True)` behind an `is-var` test is the `bool/1` generator again and
rejects (`examples/fail_det_isvar_unenforced_bool.metta`).
`examples/strictdet_det_bool_ops.metta`,
`examples/strictdet_remove_atom_nominal.metta`,
`examples/strictdet_is_member_literal.metta` and
`examples/strictdet_min_atom_tuple.metta` pin the unlocked cases.

**A determinism commitment is never deferred to a runtime check.** Nothing a
runtime type check can inspect tells a det function from a nondet one, so
where a `-[det]->`/`-[semidet]->` closure is required and the compiler could
not *prove* the value fits, it
rejects at compile time rather than emitting a guard that cannot decide it —
the same reason a conflicting newtype brand rejects instead of guarding. This
is why an undeclared function whose clauses do not analyse as deterministic
cannot be passed to a `-[det]->` parameter
(`examples/fail_det_closure_unproven_builtin.metta`).

**`-[semidet]->`: partial, but still committed.** `-[semidet]->` is the answer
to "I want a function that may legitimately have no answer" — a lookup with no
entry, a division by zero, a parse that does not apply. Its body is checked
exactly like a `-[det]->` body except that it is allowed to FAIL: `(empty)` and
calls to other `-[semidet]->` functions are fine, while `superpose`, `match`
and overlapping clause heads stay rejected, because failing is not the same as
answering twice. **It costs nothing.** semidet commits to its first matching
clause exactly like det — the right to fail leaves no choicepoint — so it keeps
choicepoint-free execution and last-call optimization
(`examples/strictdet_semidet_lco.metta` is `determinism_lco.metta` with the
arrow changed, one million levels deep). Reaching for `-[nondet]->` instead is
what actually costs: it throws away the commit for a nondeterminism that is not
there. A `-[det]->` function may not call a `-[semidet]->` one — that is
precisely the promise `-[det]->` makes — so partiality is visible in the types
all the way up. See `examples/strictdet_semidet_arrow.metta`.

**Exhaustiveness of `-[det]->`.** A `-[det]->` function whose clauses PROVABLY
cannot match some input of its declared argument types is a compile error naming
the unmatched case and suggesting `-[semidet]->`. This applies in **every mode**,
flags or not, like the overlap and body-determinism checks an explicit
`-[det]->` already gets: `--strict-det` forces you to make a determinism claim,
it is not what makes a claim you already wrote mean something. Writing `->`
claims nothing and is checked for nothing. Two things are provable: an argument position every
clause pins to a literal of an uncoverable domain (`(= (lookup 1) 10)`,
`(= (lookup 2) 20)` over `Number`), and a position discriminating on the
constructors of a nominal type whose constructor set is known in full, with one
constructor unmatched. Everything else is accepted in silence — variables and
wildcards, guards, arithmetic conditions, `Atom`/`Expression`/`%Undefined%`
arguments, and any type whose constructors cannot be enumerated.

The check is deliberately one-sided: **provably incomplete is an error, cannot
tell is accepted**. It is not GHC's "warn unless proven exhaustive" — PeTTa's
nominal types are open, a constructor may be declared in a later file, so
"cannot tell" is the normal case and erroring on it would reject working code
with no way out. What the check reports is therefore a lower bound on
incompleteness, never a totality guarantee: it can only see the constructors
declared before it, and it judges a function on the clauses visible in the file
being loaded. `examples/strictdet_det_exhaustive_limits.metta` pins the
conservative side; `examples/fail_nonexhaustive_det.metta` and
`fail_nonexhaustive_ctor.metta` pin the two provable ones. Only an explicit
`-[det]->` carries and triggers this totality promise.

**Partiality of `once`, `if` and `case`.** Exhaustiveness above is about clause
*heads*; a clause with a single variable head is exhaustive at that level and
can still be partial inside its body. Three constructs produce nothing for some
inputs, and all three now say so:

- `(once E)` is deterministic only when `E` is. `once` removes a callee's extra
  solutions, but it does not manufacture one where there was none, so it is
  `may_fail` whenever `E` may fail — including when `E` is nondeterministic or
  unanalysable, which `once` really does cap at one solution but cannot make
  productive. (That last part is also a *refinement*: an unanalysable `E` under
  a `once` is now "at most one" rather than "unknown", which `-[semidet]->`
  accepts.)
- A two-argument `(if C T)` has no else branch and is `may_fail`
  unconditionally. The failure belongs to the construct, not to any of its
  subexpressions, so combining the parts never saw it.
- A `case` compiles to a nested if-then-else with no final else, so a value no
  branch matches makes the whole `case` fail. A `case` whose branches
  *provably* do not cover the scrutinee is `may_fail`.

`-[semidet]->` accepts all of them, `-[det]->` rejects them. The `case` verdict
is asymmetric in exactly the way clause-head exhaustiveness is — only a
provably uncovered value counts, and a scrutinee whose type is unknown,
unenumerable or open stays silent. See `examples/semidet_partial_constructs.metta`
for what stays accepted and `examples/fail_det_once_semidet.metta`,
`fail_det_two_arg_if.metta`, `fail_det_case_no_catchall.metta` for what does not.

**Flow-sensitive nonemptiness.** An `(if (== $values ()) T E)` — with `$values`
a variable of a `(List _)` type — runs its else branch `E` exactly when
`$values` is a **nonempty** list. A `-[semidet]->` accessor whose only
incompleteness is the empty case is therefore `det` inside `E`, and the checker
upgrades the call there. The upgrade is **proven, not assumed**, because
`-[semidet]->` means *at most one* and a callee still produces *zero* in two
ways — no clause head matches, or a clause body fails — so both legs are
required: **coverage**, the clause heads cover every nonempty list at the
argument position (a `(cons $h $t)` head, stored as a var-headed/var-tailed cons
cell, matches all of them — a head that pins an element or fixes the tail length
does not), and **no-fail bodies**, every clause body is may-not-fail. It is
kept minimal: single-argument arity-1 callees, an `== ()`-shaped condition (in
either order), and a unique declaration at the arity. Provable-only, so it only
ever *upgrades* — any leg unproven leaves the `-[semidet]->` verdict standing and
nothing new is rejected. `examples/strictdet_nonempty_branch.metta` pins the
upgrade and `examples/fail_strictdet_nonempty_branch_failing_body.metta` pins the
no-fail-body boundary: a callee whose body calls another `-[semidet]->` function
keeps the semidet verdict at the narrowed site. Runtime clause additions and
removals recompile recorded callers transitively, so a later callee change
cannot leave this site verdict stale.

**Knowledge that arrives late.** Type declarations are pre-cached per file, so
within one file order never matters. Across files it does, and two kinds of
late knowledge used to be *believed* without being *enforced*:

- A **type or determinism declaration** in a later file than the definition it
  constrains. The clauses were compiled with no declaration in sight — no
  argument or output certification, no determinism validation, no commit cut —
  while every later caller was told the declaration holds. Such a declaration
  now warns and **recompiles** the function's clauses against it, which is what
  the in-file prepass would have done had the files been one. If they cannot
  satisfy it, the ordinary error is thrown. Enforced, or rejected.
- A **constructor** declared after a clause whose compilation read that type's
  constructor set. Union member exclusion and the exhaustiveness domain are
  both read as snapshots, and a new constructor invalidates them. Every
  snapshot is recorded with the key set it saw; a declaration that changes the
  set recompiles the clauses that read it, and re-runs any exhaustiveness
  verdict that was made on it (the latter is a property of a whole clause set,
  so there is no single clause to redo).

Both share one mechanism, and both are exercised by the multi-file cases in
`examples/soundness/`. The cost is confined to programs that actually declare
things late: nothing is recorded for a program that narrows no union and
declares no `-[det]->`.

**Strict determinism mode.** `--strict-det` (implies `--strict`) requires every
arrow in a function declaration to state its effect explicitly:
`-[det]->`, `-[semidet]->`, `-[nondet]->`, or the bounded polymorphic form
`-[$e]->`. This requirement is recursive, so higher-order parameter and output
arrows must be explicit as well. A plain `->` is accepted only in default and
`--strict` (types-only) modes, where it remains uncommitted. The builtin
signature file is an internal exception: builtin effects come authoritatively
from `det_builtins.pl`.

An explicit `-[det]->` function is validated for overlapping clause heads and
for nondeterministic bodies such as `superpose`, `match`, or dynamic `eval`;
`-[semidet]->` additionally permits failure, and `-[nondet]->` opts out.
Closure parameters carry the same explicit commitment. A clause that commits
with `(cut)` may overlap with later clauses. See
`examples/strictdet_basics.metta`,
`examples/fail_strictdet_plain_arrow.metta`, and
`examples/fail_strictdet_plain_arrow_param.metta`.

**Soundness oracles.** The checker discharges obligations statically and then
emits nothing — which means a wrong certification leaves no trace at run time.
Three flags turn the certifications back into runtime checks so the checker can
be audited against real executions. All three are pure additions: they change
only what a program verifies as it runs, never which programs compile.

- `--oracle` re-emits every statically discharged type certification as a
  runtime check — both clause outputs and the argument obligations discharged
  at call sites. If the checker certified a type the value does not have, the
  call site that certified it throws, instead of the program limping on to some
  unrelated Prolog error.
- `--oracle-det` counts the solutions of every call believed `det` or `semidet`,
  including a `-[$e]->` call whose closure arguments instantiate it to one of
  those levels. It throws on zero (for `det`) or on two or more (for either).
  This is the only check on a determinism *claim*: nothing else in the compiled
  program verifies it, and a fixed arrow's clause-entry commit actively prunes
  the choicepoints that would reveal a violation. Zero-solution violations are
  adjudicated only where the call site left the result unbound — a call whose
  result is already bound is being used as a filter, and is allowed to fail.
- `--no-det-cut` suppresses the determinism commit itself, exposing
  clause-selection alternatives.

`examples/soundness_matrix.sh` runs the first three over the whole example
suite (phases A–C), plus a set of counterexample programs in
`examples/soundness/` (phase D) that would violate a certification, each pinned
to the exact finding that must reject it. Those are all multi-file: `run.sh`
accepts more than one `.metta` file and loads them in the order given, and the
holes they pin (a constructor declared in a later file than the code that
matched on it, a determinism declaration arriving after the definition it
constrains) exist only across a file boundary. None of them needs an oracle
flag any more — they are rejected at compile time, which is the goal state; a
counterexample that only fails under an oracle is a hole that has been
instrumented rather than closed.

Specialization is audited by the same certifications. A specialized copy of a
clause (`f_Spec_[g]`, with a higher-order argument fixed) is an instance, so
its output certification can only come out more specific — it either discharges
statically, reproduces the general clause's guard, or finds a definite conflict,
and a typecheck error while specializing simply means "do not specialize", so
the call falls back to the general, guarded clause. Skipping it let the
specializer drop a guard the general clause had been compiled with
(`examples/fail_specialized_output_guard.metta`).

The oracles adjudicate with the checker's own value relation, so they audit the
checker's *certifications*, not its type model: where the model itself is too
permissive, the oracle agrees with the certification it should contradict. That
is why the `(Newtype <wildcard>)` hole had to be fixed in the compatibility
relation rather than instrumented — no oracle built on the checker's own value
relation could ever have seen it.

Notes and caveats:

- Function type declarations are pre-cached per file, so helpers may be
  declared and defined after their callers within the same file. Across files,
  imports must still precede use. Value declarations like `(: a A)` stay
  order-sensitive (they are knowledge atoms). A function declaration arriving
  after its function was already compiled in an earlier file warns and
  recompiles that function's clauses against it — see "Knowledge that arrives
  late". Results already printed by a form that ran before the recompile are
  not revisited, which is what the warning is for.
- `Expression`-typed arguments are passed unevaluated (as data), except
  underapplied closures like `(+ 1)`.
- Interpreters that call `eval` per iteration pay for a (typed) translation
  each time — that pattern was always slow and types do not change it.
- The executable specification lives in `examples/type_*.metta`,
  `examples/fail_*.metta` (must fail compilation), `examples/strict_*.metta`,
  `examples/soundness/` (multi-file counterexamples that must be rejected), and
  `examples/type_dispatch_matrix.sh`, which asserts properties of the
  generated code itself.
- Residual holes of this kind are tracked in `examples/soundness/`. The four
  the oracles used to merely document are now closed in the checker — see
  "Partiality of `once`, `if` and `case`" and "Knowledge that arrives late"
  above, and `examples/fail_newtype_wildcard_leak.metta`.

## Notebooks, Servers, Browser

### Jupyter Notebook Support

A Jupyter kernel for PeTTa is available in a separate repository for interactive MeTTa development in notebooks.

**Repository:** [trueagi-io/jupyter-petta-kernel](https://github.com/trueagi-io/jupyter-petta-kernel)

Quick install:

```bash
# Set PETTA_PATH to this PeTTa installation
export PETTA_PATH=/path/to/PeTTa

# Clone and install the kernel
git clone https://github.com/trueagi-io/jupyter-petta-kernel.git
cd jupyter-petta-kernel
./install.sh
```

Please see the [jupyter-petta-kernel README](https://github.com/trueagi-io/jupyter-petta-kernel/blob/main/README.md) for detailed installation instructions and usage.

### MeTTa server

A HTTP server running MeTTa code is also available:

**Repository:** [MettaWamJam](https://github.com/trueagi-io/MettaWamJam)

Please see the [MettaWamJam README](https://github.com/trueagi-io/MettaWamJam/blob/main/README.md) for detailed installation instructions and usage.

### MeTTa in WASM

Since Swi-Prolog can be compiled to Web Assembly, one can embed PeTTa into websites.

Please see [Execution-in-browser](https://github.com/patham9/PeTTa/wiki/Execution-in-browser) for more information.
