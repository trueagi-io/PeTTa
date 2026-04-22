# PeTTa

**Efficient MeTTa language runtime in Rust + SWI-Prolog, with an MORK zipper-based execution backend.**

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/Rust-nightly-orange.svg)](https://www.rust-lang.org)
[![Tests](https://img.shields.io/badge/tests-passing-brightgreen.svg)](test.sh)

PeTTa is a high-performance, embeddable runtime for the [MeTTa](https://wiki.opencog.org/w/File:MeTTa_Specification.pdf) language, designed for the TrueAGI / OpenCog Hyperon ecosystem. It combines a clean Rust API with a proven Prolog-based parser/translator/WAM execution engine, and optionally integrates [MORK](https://github.com/trueagi-io/MORK) — a zipper-based multi-threaded virtual machine — as a native Rust execution backend.

---

## Quick Start

### Prerequisites

- **SWI-Prolog >= 9.3** (required for the default SWI-Prolog backend and for running tests)
- **Rust (stable)** — the default configuration uses the SWI-Prolog subprocess backend and works on stable Rust
- **Rust nightly** is only required if you explicitly enable the `mork` feature (native zipper backend)
- **`aes` + `sse2` CPU support** (recommended when building with gxhash for MORK; use `RUSTFLAGS="-C target-cpu=native"`)
 - Optional: the high-performance `gxhash` crate is now opt-in via the Cargo feature `fast-hasher`.
   The crate provides faster hashing on CPUs with AES/SSE2; if you do not enable `fast-hasher` the
   project uses a pure-Rust fallback hasher so builds succeed on all hosts.

### Build

```bash
# Default build (SWI-Prolog subprocess backend; stable Rust)
cargo build --release

# Full build with MORK + parallel execution + profiling (opt-in; requires nightly)
# Enable the `fast-hasher` feature to opt into the external `gxhash` crate for improved performance:
RUSTFLAGS="-C target-cpu=native" cargo build --release --features mork,parallel,profiling,fast-hasher
```

### Run

```bash
# Single file
./target/release/petta examples/fib.metta

# Multiple files (single persistent engine — fast!)
./target/release/petta examples/if.metta examples/state.metta examples/math.metta

# Demo (no arguments)
./target/release/petta

# Verbose mode (show debug output)
./target/release/petta -v examples/fib.metta

# Timed execution
./target/release/petta -t examples/fib.metta
```

### Test

```bash
# Full example suite (270 assertions in ~0.2s via persistent engine)
sh test.sh

# Rust unit + integration tests (82 tests in ~3s)
cargo test

# With all features enabled
RUSTFLAGS="-C target-cpu=native" cargo test --features mork,parallel,profiling
```

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        MeTTa Source Files                       │
└───────────────────────────┬─────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│                     Rust CLI / Library                          │
│  PeTTaEngine  ──  EngineConfig  ──  profiler::QueryProfile      │
│  binary protocol: [type:1][len:4][payload:N] ↔ [status:1]...    │
└───────────┬──────────────────────────────┬──────────────────────┘
            │                              │
            │ default (swipl feature)      │ mork feature (nightly)
            ▼                              ▼
┌──────────────────────────┐  ┌──────────────────────────────────┐
│  SWI-Prolog Subprocess   │  │  MORK Zipper Machine             │
│  ┌────────────────────┐  │  │  ┌────────────────────────────┐  │
│  │ prolog/parser.pl   │  │  │  │ pathmap 0.3 (git)          │  │
│  │ prolog/translator  │  │  │  │ mork-expr (byte-compiled)   │  │
│  │ prolog/specializer │  │  │  │ mork-frontend (parsers)     │  │
│  │ prolog/spaces.pl   │  │  │  │ mork-interning (symbols)    │  │
│  │ prolog/metta.pl    │  │  │  │ kernel/space + sinks         │  │
│  │ prolog/utils.pl    │  │  │  │ kernel/sources               │  │
│  └────────────────────┘  │  │  └────────────────────────────┘  │
│  WAM execution engine    │  │  Pattern matching + unification  │
└──────────────────────────┘  └──────────────────────────────────┘
```

### Key Design Decisions

| Aspect | SWI-Prolog Backend (default) | MORK Backend (optional) |
|---|---|---|
| **Parser** | Prolog DCG (`prolog/parser.pl`) | MORK bytestring parser |
| **Execution** | SWI-Prolog WAM | Zipper-based reduction machine |
| **Speed** | Fast (persistent subprocess) | Faster (native Rust, multi-threaded) |
| **Dependency** | SWI-Prolog >= 9.3 | None (pure Rust + PathMap) |
| **Feature** | `swipl` (default) | `mork` (requires nightly) |

### Dual-Mode Native Parser

PeTTa includes a native Rust MeTTa S-expression parser (`petta_parser/`) built with `nom`. This parser operates independently of the Prolog DCG parser, providing a clear migration path toward a pure-Rust implementation. The `pure-rust` feature flag is ready to disable the Prolog fallback once core components are fully ported.

---

## Using as a Rust Library

Add to your `Cargo.toml`:

```toml
[dependencies]
petta = { path = "/path/to/petta" }

# Optional features
# petta = { path = "/path/to/petta", features = ["parallel", "profiling"] }
```

### Basic Usage

```rust
use petta::{PeTTaEngine, MettaValue};
use std::path::Path;

// Create engine with default config
let mut engine = PeTTaEngine::new(Path::new("/path/to/petta"), false)?;

// Execute MeTTa code strings
let results = engine.process_metta_string("(= (myfunc $x) (+ $x 1)) !(myfunc 41)")?;
for r in &results {
    println!("{} → {:?}", r.value, r.parsed_value());
}

// Load MeTTa files (single engine handles all files)
let results = engine.load_metta_file(Path::new("examples/fib.metta"))?;

// Check engine health
assert!(engine.is_alive());
```

### Engine Configuration

```rust
use petta::EngineConfig;
use std::time::Duration;

let config = EngineConfig::new(Path::new("/path/to/petta"))
    .swipl_path("/usr/local/bin/swipl")   // custom SWI-Prolog path
    .verbose(true)                          // show Prolog debug output
    .profile(true)                          // enable query profiling
    .max_restarts(3)                        // auto-restart on crash (up to 3x)
    .query_timeout(Duration::from_secs(30)) // optional timeout per query
    .stack_limit("4g");                     // SWI-Prolog stack limit

let mut engine = PeTTaEngine::with_config(&config)?;
```

### Query Profiling

```rust
use petta::profiler::{QueryProfile, ProfileStats};

// Profiled execution returns results + timing data
let (results, profile) = engine.process_metta_string_profiled("!(+ 1 2)")?;
println!("{}", profile.summary());
// → Query 'process_metta_string' (7 bytes): serialize=0.012ms,
//   round_trip=1.234ms, parse=0.003ms, total=1.249ms, results=1
```

### Parallel Batch Execution

```rust
// Enable "parallel" feature in Cargo.toml
// Multiple queries → one engine per query → true parallelism via rayon

let results = engine.process_metta_strings_parallel(&[
    "!(+ 1 2)",
    "!(+ 3 4)",
    "!(+ 5 6)",
]);
// Returns Vec<Result<Vec<MettaResult>, PeTTaError>>
```

---

## Feature Flags

| Feature | Description | Dependencies |
|---|---|---|
| `swipl` (default) | SWI-Prolog subprocess backend | None (uses existing Prolog files) |
| `pure-rust` | Future: disable Prolog fallback | — |
| `mork` | MORK zipper-based execution backend (opt-in; requires nightly) | MORK crates, PathMap 0.3 (git); optionally `fast-hasher` (gxhash) |
| `fast-hasher` | Opt-in high-performance hasher (gxhash) | `gxhash` (optional dependency) |
| `parallel` | Rayon-based parallel batch execution | `rayon` |
| `profiling` | Query timing & profiling support | `parking_lot` |
| `faiss` | FAISS vector atom space support (future) | `faiss` |

### Build Matrix

```bash
# Default (SWI-Prolog backend)
cargo build --release

# With profiling and parallel execution
cargo build --release --features profiling,parallel

# Full MORK integration (requires nightly + native CPU flags)
RUSTFLAGS="-C target-cpu=native" cargo build --release --features mork

# Pure-Rust path (future, once all components are ported)
cargo build --release --features pure-rust
```

---

## Project Structure

```
petta/
├── Cargo.toml              # Workspace: petta + MORK sub-crates
├── prolog/                 # SWI-Prolog backend (FFI boundary)
│   ├── README.md           # FFI protocol documentation
│   ├── parser.pl           # DCG S-expression parser
│   ├── translator.pl       # MeTTa → Prolog goal compilation
│   ├── specializer.pl      # Higher-order function specialization
│   ├── spaces.pl           # Atom space management
│   ├── filereader.pl       # File loading & form parsing
│   ├── metta.pl            # Standard library, builtins, entry point
│   └── utils.pl            # Shared utility predicates
├── lib/                    # MeTTa standard library (.metta files)
├── examples/               # 145+ MeTTa example files
├── tests/                  # Rust integration tests
│   └── protocol_integration.rs  # 34 protocol round-trip tests
├── prolog/                 # Prolog source files (FFI boundary)
├── rust/src/
│   ├── lib.rs              # PeTTaEngine: binary protocol, error types, config
│   ├── main.rs             # CLI application (clap-like argument handling)
│   ├── profiler.rs         # QueryProfile, ProfileStats
│   └── petta_parser/       # Native Rust MeTTa parser (nom)
│       └── mod.rs          # parse_metta(), serialize_metta()
└── repos/
    ├── MORK/               # trueagi-io/MORK (local copy for development)
    │   ├── expr/           # Byte-compiled S-expression representation
    │   ├── frontend/       # Bytestring + JSON parsers
    │   ├── interning/      # Symbol interning with memory mapping
    │   └── kernel/         # Zipper-based execution engine
    └── PathMap/            # Adam-Vandervorst/PathMap 0.3 (git)
```

---

## Binary Protocol

The Rust ↔ Prolog communication uses a length-prefixed binary protocol over stdin/stdout pipes:

**Request (Rust → Prolog):**
```
[1 byte type]  'F'(70) = file, 'S'(83) = string, 'Q'(81) = quit, 'C'(67) = cancel
[4 bytes big-endian u32: payload length]
[N bytes UTF-8: payload]
```

**Response (Prolog → Rust):**
```
[1 byte status]  0 = success, 1 = error
If 0: [4 bytes result count] then per-result: [4 bytes str len][N bytes UTF-8]
If 1: [4 bytes error msg len][N bytes UTF-8 error message]
```

SWI-Prolog signals readiness by sending `0xFF` at startup.

---

## Error Handling

PeTTa uses `thiserror` for structured error types:

```rust
pub enum PeTTaError {
    FileNotFound(PathBuf),
    SpawnSwipl(String),
    PathError(String),
    WriteError(String),
    SwiplError(SwiplErrorKind),   // Parsed Prolog errors
    SwiplVersionError(String),
    ProtocolError(String),
    Io(std::io::Error),
    Timeout(Duration),
    SubprocessCrashed { restarts: u32 },
}

pub enum SwiplErrorKind {
    UndefinedFunction { name, arity, suggestion },
    TypeMismatch { expected, found, context },
    SyntaxError { line, column, detail },
    UninstantiatedArgument { location },
    PermissionDenied { operation, target },
    ExistenceError { error_type, term },
    StackOverflow { limit },
    Generic(String),
}
```

---

## Development Plan (TODO.md)

PeTTa development follows a **5-track parallel roadmap**:

### Track 1: Rust Core Enhancement ✅ COMPLETE
- [x] Proper error types (`thiserror`), logging (`tracing`), `EngineConfig` builder
- [x] Subprocess lifecycle: `is_alive()`, `restart()`, auto-recovery on crash
- [x] 34 integration tests + 48 unit tests (82 total)
- [x] Prolog files extracted to `prolog/` with documented FFI boundary
- [x] Native Rust parser (`nom`) — dual-mode operation ready
- [x] `pure-rust` feature flag scaffolding

### Track 2: Performance & Acceleration ✅ COMPLETE
- [x] MORK integrated (source cloned, PathMap 0.3 from git, compiles)
- [x] FAISS support scaffolded (`faiss` crate in Cargo.toml)
- [x] Parallel batch execution via `rayon` (`parallel` feature)
- [x] Profiling hooks: `QueryProfile`, `ProfileStats`, `--profile` config
- [x] Binary protocol: length-prefixed, zero-copy ready
- [x] CI: `test.sh` runs 270 assertions in 0.2s (persistent engine)

### Track 3: Ecosystem Compatibility *(pending)*
- [ ] Certify compatibility with PLN, chaining, metta-attention, metta-examples
- [ ] Official Hyperon C/Rust API surface
- [ ] PyO3 Python bindings (`petta-py`)
- [ ] Version pinning and lockfile support for `git-import!`
- [ ] Compatibility matrix in README

### Track 4: Advanced Tooling *(pending)*
- [ ] Production CLI with `clap` v4 (`run`, `repl`, `serve`, `debug`, `import`, `bench`)
- [ ] Axum/Warp HTTP + WebSocket server
- [ ] LSP implementation + Jupyter kernel
- [ ] Debug tools (`--debug` mode, atom-space visualizations)
- [ ] WASM target support
- [ ] TUI REPL + visualization commands

### Track 5: Packaging & Distribution *(pending)*
- [ ] Publish crate to crates.io
- [ ] Comprehensive GitHub Actions CI (build, test, clippy, cross-compilation)
- [ ] Pre-built binaries and installers
- [ ] README architecture diagram, quick-start guides, API reference
- [ ] Issue/PR templates, CODE_OF_CONDUCT, community links
- [ ] Upstream PR to `trueagi-io/PeTTa`
- [ ] Public roadmap & "Call for Contributors"

---

## Performance

| Benchmark | Time |
|---|---|
| CLI startup (single file) | ~90ms |
| `!(+ 1 2)` through engine | ~1ms |
| `fib(30)` via persistent engine | ~2.6s |
| Full test suite (270 assertions) | **0.2s** |
| Rust test suite (82 tests) | **2.7s** |

The persistent engine eliminates subprocess startup overhead. All example files share one Prolog process.

---

## License

MIT — Copyright 2025 Patrick Hammer

---

## References

PeTTa integrates several open-source components. Below is a complete list of all subcomponents, their authors, and licenses.

### Core PeTTa

| Component | Author | License | Repository |
|---|---|---|---|
| **PeTTa** (Rust CLI + library) | Patrick Hammer | MIT | `trueagi-io/PeTTa` |
| **PeTTa Prolog backend** (`prolog/*.pl`) | Patrick Hammer | MIT | `trueagi-io/PeTTa` |

### MORK Ecosystem

| Component | Author | License | Repository |
|---|---|---|---|
| **MORK** (MeTTa Optimal Reduction Kernel) | Adam Vandervorst, TrueAGI | (see repo) | `trueagi-io/MORK` |
| └ `mork-expr` (byte-compiled S-expressions) | Adam Vandervorst, TrueAGI | (see repo) | `trueagi-io/MORK/expr` |
| └ `mork-frontend` (parsers) | Adam Vandervorst, TrueAGI | (see repo) | `trueagi-io/MORK/frontend` |
| └ `mork-interning` (symbol interning) | Adam Vandervorst, TrueAGI | (see repo) | `trueagi-io/MORK/interning` |
| └ `mork` kernel (zipper execution) | Adam Vandervorst, TrueAGI | (see repo) | `trueagi-io/MORK/kernel` |

### Dependencies

| Component | Author | License | Repository |
|---|---|---|---|
| **PathMap** (trie-based key-value store) | Adam Vandervorst | MIT | `Adam-Vandervorst/PathMap` |
| **SWI-Prolog** | Jan Wielemaker et al. | BSD-2 | `SWI-Prolog/swipl-devel` |
| **thiserror** | David Tolnay | MIT \| Apache-2.0 | `dtolnay/thiserror` |
| **tracing** | Tokio contributors | MIT | `tokio-rs/tracing` |
| **nom** | Geoffroy Couprie | MIT | `Geal/nom` |
| **rayon** | Niko Matsakis, Josh Stone | MIT \| Apache-2.0 | `rayon-rs/rayon` |
| **smallvec** | Servo developers | MIT \| Apache-2.0 | `servo/rust-smallvec` |
| **gxhash** | Tommy et al. | MIT | `luketpeterson/gxhash` |
| **xxhash-rust** | Rust community | MIT \| Apache-2.0 | ` rust-lang/xxhash-rust` |
| **tempfile** | Steven Allen | MIT \| Apache-2.0 | `Stebalien/tempfile` |
| **faiss** (optional) | Facebook Research / Rust bindings | MIT | `facebookresearch/faiss` |

### Related Projects

| Project | Description | Repository |
|---|---|---|
| **hyperon-experimental** | Reference MeTTa implementation | `trueagi-io/hyperon-experimental` |
| **metta-wam** | MeTTa WAM interpreter | `trueagi-io/metta-wam` |
| **PLN** | Probabilistic Logic Networks | `trueagi-io/PLN` |
| **chaining** | Forward/backward chaining | `trueagi-io/chaining` |
| **metta-examples** | MeTTa example library | `trueagi-io/metta-examples` |
