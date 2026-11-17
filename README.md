## PeTTa

Efficient MeTTa language implementation in Prolog.

Please check out the [Wiki](https://github.com/patham9/PeTTa/wiki) for more information.

### Dependencies

- SWI-Prolog >= 9.3.x
- Python 3.x (for janus Python interop)

### Usage

Example run:

`time sh run.sh ./examples/nars_tuffy.metta`

### List shape and term text

`get-metatype` reports `Expression` for a finite outer list ending in `()`.
`is-expr` checks the same shape. The expression `(quote (1 $x))` has a
complete outer list and an unknown element. The cell `(cons 1 $tail)` has an
open tail, so `is-expr` returns `False` and `get-metatype` has no result.
Use `is-ground` to check for unbound variables anywhere in a value. It
returns `False` for both examples and `True` for the improper cell
`(cons 1 2)`.

`decons-atom` exposes the head and tail of an existing cell. On an open
list, `size-atom` enumerates completions of increasing length.
`(once (size-atom (cons 1 $tail)))` returns `1` and binds the tail to `()`
in that solution.

`repr` formats values for display. The native cell `(cons 1 2)` and the
quoted expression `(quote (cons 1 2))` both display as `(cons 1 2)`.
`parse` reads that text as a three-element MeTTa expression.

To read back plain finite data, pair `repra` with SWI-Prolog's
[`atom_to_term/3`](https://www.swi-prolog.org/pldoc/doc_for?object=atom_to_term/3).
The writer and reader use Prolog term syntax and preserve the cell shape and
sharing of ordinary variables within the value.

```metta
!(import_prolog_function atom_to_term)
(= (read-repra $text)
   (let $_ (atom_to_term $text $value) $value))

!(test (read-repra (repra (cons 1 2))) (cons 1 2))
!(test (read-repra (repra (quote (cons 1 2)))) (quote (cons 1 2)))
```

Read the text with compatible Prolog syntax settings. Constraints and live
resources such as spaces require their own representation. See the
[runnable example](examples/list_shape_and_term_text.metta) for a round trip
with an open tail and shared variables.

### MORK and FAISS spaces

If MORK and FAISS is installed, execute `sh build.sh` to support MORK-based atom spaces and FAISS-based atom-vector spaces.

The following projects are cloned and built by build.sh:

**Repository:** [mork_ffi](https://github.com/patham9/mork_ffi) dependent on [trueagi-io/mork](https://github.com/trueagi-io/mork)

**Repository:** [faiss_ffi](https://github.com/patham9/faiss_ffi) dependent on [facebookresearch/faiss](https://github.com/facebookresearch/faiss)

### Extension libraries

Please check out [Extension libraries](https://github.com/trueagi-io/PeTTa/wiki/Extension-libraries) for a set of extension libraries that can be invoked from MeTTa files directly from the git repository.

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
