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

**Strict mode.** `sh run.sh file.metta --strict` additionally requires every
compiled function to have a declared or inferable type and rejects compilation
if any residual runtime type guard would be emitted — a machine-checked
guarantee that the compiled program contains no runtime type checks.

**Determinism arrows.** `(: f (A B -[det]-> C))` (or the chained form
`(A -[det]-> B)`) declares a deterministic function: the compiler validates
that its clauses cannot overlap and its body is deterministic, then commits to
the first matching clause — guaranteeing choicepoint-free execution and
constant-memory deep recursion via last-call optimization
(see `examples/determinism_lco.metta`). `-[nondet]->` documents intentional
nondeterminism.

Notes and caveats:

- Type declarations must precede the definitions and calls they should affect;
  a declaration arriving after its function is compiled warns and has no
  retroactive effect.
- `Expression`-typed arguments are passed unevaluated (as data), except
  underapplied closures like `(+ 1)`.
- Interpreters that call `eval` per iteration pay for a (typed) translation
  each time — that pattern was always slow and types do not change it.
- The executable specification lives in `examples/type_*.metta`,
  `examples/fail_*.metta` (must fail compilation), `examples/strict_*.metta`,
  and `examples/type_dispatch_matrix.sh`, which asserts properties of the
  generated code itself.

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
