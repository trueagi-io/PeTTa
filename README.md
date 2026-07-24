## PeTTa

Efficient MeTTa language implementation in Prolog.

Please check out the [Wiki](https://github.com/patham9/PeTTa/wiki) for more information.

### Dependencies

- SWI-Prolog >= 9.3.x
- Python 3.x (for janus Python interop)

### Usage

Example run:

`time sh run.sh ./examples/nars_tuffy.metta`

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
outputs, and enable the fused code generation — but never reject a program:
a call that statically mismatches an inferred type fails at run time exactly
as it would have without inference.

**Match schemas.** Space reads are typed through declared relation schemas:
with `(: age (-> String Number Atom))` declared, the pattern variables of
`(match &self (age $who $n) ...)` acquire `String` and `Number`, so match
results are fully typed and guard-free code is generated for the body. Type
queries like `(match &self (: $x Fruit) $x)` bind `$x : Fruit` directly.

**Strict mode.** `sh run.sh file.metta --strict` additionally requires every
compiled function to have a declared or inferable type and rejects compilation
if any residual runtime type guard would be emitted — a machine-checked
guarantee that the compiled program contains no *implicit* runtime type checks.

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

**Union types.** A heterogeneous position can declare its alternatives with
`(| T1 T2 ...)` — for example `(List (| (CPU %Undefined% %Undefined%
%Undefined%) (: %Undefined% %Undefined% %Undefined% TV)))` for a mixed
execution list. A value fits a union if it fits some member, and `case` or
clause-head patterns narrow to the member their shape selects, typing the
pattern's variables. Unions are declared, never inferred.

**Erased nominal newtypes.** `(: KB (Newtype Expression))` declares a
distinct compile-time role over an existing representation: nothing is
wrapped at runtime and no guards are emitted. A branded value fits its
representation, but different brands never unify merely because their
representations do — swapping a `Proof` into a `KB` position is a compile
error. Raw literals and constructed values acquire a brand contextually from
the expected position; an unknown variable does not (under `--strict`) —
brand it explicitly with `(brand KB $x)`, an erased trust operation that
rejects conflicting brands but generates no check (a role has no runtime
witness). Declared relation schemas restore brands on `match`. Use
`(the KB ...)` instead when you want the representation checked at runtime.

**Determinism arrows.** `(: f (-[det]-> A B C))` — prefix, like `->` and
every other MeTTa form — declares a deterministic function: the compiler validates
that its clauses cannot overlap and its body is deterministic, then commits to
the first matching clause — guaranteeing choicepoint-free execution and
constant-memory deep recursion via last-call optimization
(see `examples/determinism_lco.metta`). `-[nondet]->` documents intentional
nondeterminism.

**Strict determinism mode.** `--strict-det` (implies `--strict`) makes a
plain `->` itself a determinism commitment: every declared function is
validated as deterministic unless its arrow says `-[nondet]->`. Overlapping
clause heads, `superpose`/`match` bodies and dynamic `eval` become compile
errors on `->` functions — in MeTTa every matching clause fires, so each such
error is either an accidental source of multiple results or a missing
`-[nondet]->`. Closure parameters carry the same commitment: a
`(-> $a $b)`-typed parameter may be applied inside a deterministic body,
a `-[nondet]->` one may not. A clause that commits with `(cut)` may overlap
with later clauses. See `examples/strictdet_basics.metta`.

Notes and caveats:

- Function type declarations are pre-cached per file, so helpers may be
  declared and defined after their callers within the same file. Across files,
  imports must still precede use. Value declarations like `(: a A)` stay
  order-sensitive (they are knowledge atoms), and a function declaration
  arriving after its function was already compiled in an earlier file warns
  and has no retroactive effect.
- `Expression`-typed arguments are passed unevaluated (as data), except
  underapplied closures like `(+ 1)`.
- Interpreters that call `eval` per iteration pay for a (typed) translation
  each time — that pattern was always slow and types do not change it.
- The executable specification lives in `examples/type_*.metta`,
  `examples/fail_*.metta` (must fail compilation), `examples/strict_*.metta`,
  and `examples/type_dispatch_matrix.sh`, which asserts properties of the
  generated code itself.

Git imports retain the legacy URL-only, URL/build-command, and
URL/build-command/base-directory forms. For a reproducible detached checkout,
pass a fourth input in the order URL, build command, base directory, commit:

```metta
!(git-import! "https://example/repo.git" "" "./repos" "0123456789abcdef0123456789abcdef01234567")
```

Pinned imports accept only a full 40-character hexadecimal commit SHA;
abbreviated SHAs, branches, and tags are rejected.

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
