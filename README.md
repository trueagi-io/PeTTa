## PeTTa
PeTTa is a Prolog-based transpiler and execution framework for the MeTTa programming language. It translates MeTTa programs into efficient Prolog clauses and executes them using SWI-Prolog, leveraging decades of optimization from modern Prolog systems. PeTTa enables MeTTa code to run at performance levels comparable to hand-written Prolog while preserving MeTTa’s expressive functional-logic style.

### Overview
MeTTa (Meta Type Talk) is a multi-paradigm language developed within the OpenCog Hyperon project. It supports:
* Functional programming
* Logic programming
* Non-determinism
* Unification
* Higher-order functions
* Knowledge graph manipulation

The reference MeTTa interpreter is written in Rust and uses AST traversal for execution. While flexible, this approach incurs overhead for recursive and large-scale symbolic computations.

PeTTa addresses this by:
* Parsing MeTTa S-expressions
* Translating them into Prolog predicates
* Executing them natively in SWI-Prolog

This approach achieves performance on par with native Prolog implementations while preserving MeTTa semantics.

### Architecture
PeTTa consists of the following components:
```
Main
 ├── FileReader
 ├── Parser
 ├── Translator
 └── Metta (standard library)
```
Please check out the [Wiki](https://github.com/patham9/PeTTa/wiki) for more information.

### Key Features
1. Smart Dispatch: In MeTTa, an S-expression may represent either a function call or a literal data list. PeTTa resolves this distinction at compile time whenever possible.
2. Function Specialization: MeTTa supports higher-order functions and PeTTa dynamically specializes higher-order functions when invoked with concrete function arguments.
3. Non-Determinism Support: PeTTa preserves MeTTa’s relational semantics for non-determinism such as `supperpose`, `collapse` and `hyperpose`. Non-deterministic execution is fully supported via Prolog backtracking.
4. Seamless Prolog Interoperability: because PeTTa generates ordinary Prolog predicates, all SWI-Prolog libraries can be used directly with available Foreign language interfaces.

### Design Philosophy

PeTTa demonstrates that:
* Dynamic S-expression languages can be efficiently compiled
* Prolog is a powerful execution substrate for modern symbolic languages
* Higher-order functional logic programming can be specialized effectively
* Runtime dispatch can be largely eliminated via compile-time analysis

PeTTa bridges the gap between:
* MeTTa’s expressive functional-logic semantics
* Prolog’s mature, optimized execution model

### Usage
Example run:
`time sh run.sh ./examples/nars_tuffy.metta`

### Dependencies
- SWI-Prolog >= 9.3.x
- Python 3.x (for janus Python interop)

### MORK and FAISS spaces
If MORK and FAISS is installed, execute `sh build.sh` to support MORK-based atom spaces and FAISS-based atom-vector spaces.
The following projects are cloned and built by build.sh:

**Repository:** [mork_ffi](https://github.com/patham9/mork_ffi) dependent on [trueagi-io/mork](https://github.com/trueagi-io/mork)

**Repository:** [faiss_ffi](https://github.com/patham9/faiss_ffi) dependent on [facebookresearch/faiss](https://github.com/facebookresearch/faiss)

### Extension libraries
Please check out [Extension libraries](https://github.com/trueagi-io/PeTTa/wiki/Extension-libraries) for a set of extension libraries that can be invoked from MeTTa files directly from the git repository.

### Documentation
Please see [PeTTa Wiki](https://github.com/patham9/PeTTa/wiki).

### Authors
* Hammer Patrick — SingularityNET Foundation
* Isaev Peter — SingularityNET Foundation


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

**Repository:** [MettaWamJam](https://github.com/jazzbox35/MettaWamJam)

Please see the [MettaWamJam README](https://github.com/jazzbox35/MettaWamJam/blob/main/README.md) for detailed installation instructions and usage.

### MeTTa in WASM

Since Swi-Prolog can be compiled to Web Assembly, one can embed PeTTa into websites.

Please see [Execution-in-browser](https://github.com/patham9/PeTTa/wiki/Execution-in-browser) for more information.
