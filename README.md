# PeTTa 🧠

**A production-ready MeTTa runtime — built for embedding, testing, and scale.**

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/Rust-stable-brightgreen.svg)](https://www.rust-lang.org)
[![Tests](https://img.shields.io/badge/145-tests-brightgreen.svg)](test.sh)

PeTTa is an efficient, embeddable runtime for the [MeTTa](https://wiki.opencog.org/w/File:MeTTa_Specification.pdf) language — the declarative substrate for the TrueAGI / OpenCog Hyperon ecosystem. This implementation delivers MeTTa as a **Rust crate** with a proven Prolog WAM engine at its core, designed for production use in services, tools, and embedded systems. ⚙️

---

## What is MeTTa? 🤔

MeTTa (Meta Type Talk) is a typed, eager, expressive language built on unified pattern matching and direct expression manipulation. It serves as both a **programming language** and a **knowledge representation format** — ideal for systems that must reason about their own code and data.

The runtime compiles MeTTa source to the **Warren Abstract Machine (WAM)**, leveraging decades of proven Prolog execution semantics. The result is mathematically rigorous, battle-tested, and genuinely comprehensible. ✨

---

## Key Features ✨

### 🔧 Embeddable Library

Import MeTTa into any Rust application:

```rust
use petta::{PeTTaEngine, MettaValue};
use std::path::Path;

let mut engine = PeTTaEngine::new(Path::new("/path/to/petta"), false)?;
let results = engine.process_metta_string("!(+ 1 2)")?;
// → ["3"]
```

### 🧪 Comprehensive Test Suite

| Layer | Count | Description |
|-------|-------|------------|
| Unit tests | 111 | Core engine verification |
| Integration | 34 | Binary protocol round-trip |
| Examples | 270 | Stdlib validation |

Every change is verified. Every feature has coverage. ✅

### ⚡ Persistent Engine

One subprocess, reused across all calls. No spawn overhead. No cold starts. 🔥

```rust
// Load multiple files — same engine
engine.load_metta_file(Path::new("examples/math.metta"))?;
engine.load_metta_file(Path::new("examples/state.metta"))?;
// Queries hit a warm engine immediately
```

### 🎯 Structured Error Handling

Every failure mode is typed and traceable:

```rust
pub enum PeTTaError {
    FileNotFound(PathBuf),
    SpawnSwipl(String),
    ProtocolError(String),
    SwiplError(SwiplErrorKind),
    Timeout(Duration),
    SubprocessCrashed { restarts: u32 },
}

pub enum SwiplErrorKind {
    UndefinedFunction { name, arity, suggestion },
    TypeMismatch { expected, found, context },
    SyntaxError { line, column, detail },
    StackOverflow { limit },
}
```

### 📊 Query Profiling

Built-in timing instrumentation:

```rust
let (results, profile) = engine.process_metta_string_profiled("!(fib 30)")?;
println!("{}", profile.summary());
// → Query 'process_metta_string': serialize=0.012ms, 
//   round_trip=1.234ms, parse=0.003ms, total=1.249ms
```

### 🔀 Dual Parser

Two parsers, one goal:

- **Prolog DCG** — full MeTTa semantics, complete feature set
- **Native Rust (nom)** — fast S-expression parsing for common cases

The native parser also provides a migration path toward a pure-Rust future. 🌉

### 🚀 Optional MORK Backend

For performance-critical workloads, swap in the MORK zipper-based execution engine:

```bash
RUSTFLAGS="-C target-cpu=native" cargo build --release --features mork
```

---

## Quick Start 🚀

### Prerequisites

- **SWI-Prolog >= 9.3** 🐠
- **Rust (stable)** 🦀

### Build

```bash
cargo build --release
```

### Run

```bash
# Single file
./target/release/petta examples/fib.metta

# Multiple files (persistent engine)
./target/release/petta examples/if.metta examples/state.metta examples/math.metta

# Interactive REPL
./target/release/petta

# Timed execution
./target/release/petta -t examples/fib.metta
```

### Test

```bash
# Rust test suite
cargo test

# Example suite
sh test.sh
```

---

## Architecture 🏗️

```
┌─────────────────────────────────────────────────────────────────┐
│                       MeTTa Source Files                         │
└───────────────────────────┬─────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Rust Host / CLI                              │
│   PeTTaEngine  ──  EngineConfig  ──  profiler::QueryProfile        │
│   binary protocol: [type][len][payload] ↔ [status][results]        │
└───────────────────────────┬─────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────────────────────┐
│                     Prolog Core                                │
│   parser.pl → translator.pl → specializer.pl               │
│   metta.pl → spaces.pl → utils.pl                     │
│              WAM execution engine                      │
└─────────────────────────────────────────────────────────────────┘
```

The **Prolog WAM is the engine** — mathematically proven, declaratively clear, and remarkably resilient. This implementation embeds it behind a clean Rust FFI boundary with proper lifecycle management. 🛡️

---

## Using as a Library 📦

Add to `Cargo.toml`:

```toml
[dependencies]
petta = { path = "/path/to/petta" }

# Optional features
# petta = { path = "/path/to/petta", features = ["parallel", "profiling"] }
```

### Configuration

```rust
use petta::EngineConfig;
use std::time::Duration;

let config = EngineConfig::new(Path::new("/path/to/petta"))
    .swipl_path("/usr/local/bin/swipl")
    .verbose(true)
    .profile(true)
    .max_restarts(3)
    .query_timeout(Duration::from_secs(30))
    .stack_limit("4g");

let mut engine = PeTTaEngine::with_config(&config)?;
```

### Parallel Execution

```rust
let results = engine.process_metta_strings_parallel(&[
    "!(+ 1 2)",
    "!(+ 3 4)",
    "!(+ 5 6)",
])?;
```

---

## Feature Flags 🎛️

| Feature | Description | Default |
|---------|-------------|---------|
| `swipl` | SWI-Prolog subprocess backend | on |
| `mork` | MORK zipper-based backend (nightly) | off |
| `parallel` | Rayon parallel batch | off |
| `profiling` | Query timing instrumentation | off |
| `fast-hasher` | gxhash acceleration | off |

---

## Project Structure 📁

```
petta/
├── Cargo.toml          # Workspace
├── prolog/           # WAM engine
│   ├── parser.pl
│   ├── translator.pl
│   ���── specializer.pl
│   ├── spaces.pl
│   ├── filereader.pl
│   ├── metta.pl
│   └── utils.pl
├── lib/              # MeTTa stdlib
├── examples/         # 145+ examples
├── rust/
│   ├── src/
│   │   ├── lib.rs      # PeTTaEngine, protocol, errors
│   │   ├── main.rs     # CLI
│   │   ├── cli.rs     # Arg parsing
│   │   ├── repl.rs    # REPL
│   │   ├── profiler.rs
│   │   ├── parser/    # Native Rust parser
│   │   ├── engine/   # Subprocess management
│   │   ├── mork/    # MORK backend
│   │   └── pathmap/  # PathMap 0.3
│   └── tests/       # 111 Rust tests
```

---

## Binary Protocol 🔌

Language-agnostic communication over stdin/stdout:

**Request**: `[type:1][len:4][payload:N]`
- `F` = file, `S` = string, `Q` = quit

**Response**: `[status:1][...results...]`
- `0` = success, `1` = error

Any language can implement a client. 🌐

---

## Performance 📈

| Benchmark | Time |
|-----------|------|
| CLI startup | ~90ms |
| Query (`(+ 1 2)`) | ~1ms |
| `fib(30)` | ~2.6s |
| Example suite (270 assertions) | ~0.2s |
| Rust test suite (111) | ~3s |

The **persistent engine** eliminates spawn overhead. All queries share one warm subprocess. 🎯

---

## Why PeTTa? 💡

| Quality | What It Means |
|---------|---------------|
| **Embeddable** | Import into Rust apps, services, tools |
| **Tested** | 145 tests across unit, integration, example layers |
| **Reliable** | Structured errors, health checks, auto-restart |
| **Fast** | Persistent engine, parallel execution, optional MORK |
| **Maintainable** | Clean boundaries, documented protocol, modular |
| **Future-proof** | Dual parser, MORK integration, WASM-ready |

This is the foundation for building real systems with MeTTa. 🏗️

---

## The Opportunity 🌟

MeTTa represents a unique convergence — a language that is both **executable logic** and **knowledge representation**. It is the substrate for reasoning systems that can manipulate their own representations.

But language potential is realized only through runtime quality. A language without a tested, embeddable, production-ready runtime is a research prototype, not an engineering foundation.

**This implementation is that foundation.** 🌉

Built for:
- Services that expose MeTTa over HTTP/gRPC
- Tools that embed MeTTa as a DSL
- Edge deployments with minimal resources
- Research requiring reliable, reproducible execution

The best is yet to come. Build something remarkable. 🚀

---

## License 📜

MIT — Copyright 2025 Patrick Hammer
