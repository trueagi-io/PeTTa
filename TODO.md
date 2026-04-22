# PeTTa Rust Port Development Plan

**Project**: autonull/petta (swipl_b branch)
**Version**: 1.0 (self-contained actionable roadmap)

---

## Vision

Transform the current hybrid Rust + SWI-Prolog MeTTa implementation into the leading embeddable, high-performance, and developer-friendly runtime for the MeTTa language within the TrueAGI / OpenCog Hyperon ecosystem.

The foundation is already solid: a clean Rust CLI and library (`PeTTaEngine`) that wraps a persistent SWI-Prolog subprocess using a binary protocol, preserving the proven Prolog-based parser, translator, specializer, spaces, and WAM execution engine while eliminating Python/Janus dependencies.

This plan organizes development into **five parallel, independent tracks** that can proceed simultaneously. Each track contains concrete, prioritized, actionable steps. Tracks are designed to be minimally interdependent, allowing multiple contributors to work in parallel with clear integration points.

Success for the entire project: a production-ready MeTTa runtime that is:

1. Easier to install and embed than any existing backend.
2. Fully compatible with downstream TrueAGI projects (PLN, attention, chaining, etc.).
3. Positioned as a first-class alternative or drop-in replacement for hyperon-experimental in many workflows.

---

## Track 1: Rust Core Enhancement & Pure-Rust Migration Path

**Objective**: Strengthen the Rust layer, improve reliability of the Prolog subprocess, and create a clear incremental path toward a pure-Rust implementation (removing the SWI-Prolog runtime dependency).

### Actionable Steps

1. Refactor `PeTTaEngine` into a stable public crate with proper error types (`thiserror`), logging (`tracing`), and configuration options (Prolog binary protocol flags, timeout handling, resource limits).
2. Implement robust subprocess lifecycle management: automatic restart on crash, graceful shutdown, and connection pooling for concurrent queries.
3. Add comprehensive Rust unit and integration tests for the binary protocol (covering all current MeTTa → Prolog message types).
4. Extract and isolate Prolog-specific modules (`parser.pl`, `translator.pl`, `specializer.pl`, `spaces.pl`, `filereader.pl`) into a dedicated `prolog/` directory with clear FFI boundaries.
5. Begin incremental porting of the DCG parser and translator to Rust (using `pest` or `nom`); maintain dual-mode operation so the project remains functional during migration.
6. Add a compile-time feature flag `pure-rust` that disables Prolog fallback once core components are ported.

**Dependencies / Prerequisites**: None (self-contained starting point).
**Integration Points**: Feeds into Track 2 (performance) and Track 3 (compatibility).

---

## Track 2: Performance & Acceleration

**Objective**: Achieve maximum execution speed by integrating native Rust acceleration technologies while retaining Prolog strengths where they excel.

### Actionable Steps

1. Integrate `trueagi-io/MORK` (Rust zipper-machine) directly into `PeTTaEngine` as an optional execution backend for pattern matching and reduction (bypassing Prolog WAM when enabled).
2. Add native FAISS support via the existing `faiss_ffi` or a pure-Rust FAISS binding for vector atom spaces.
3. Implement parallel query execution using `rayon` or `tokio` for batch processing of independent MeTTa expressions.
4. Add built-in profiling hooks (timing of parser → translator → execution stages) and expose them via CLI (`--profile`) and Rust API.
5. Optimize the binary protocol: add zero-copy data transfer for large atom spaces and streaming results for long-running queries.
6. Create automated benchmarks against `hyperon-experimental`, original PeTTa, and metta-wam; include them in CI.

**Dependencies / Prerequisites**: Track 1 (stable `PeTTaEngine`).
**Integration Points**: Directly benefits Track 3 (ecosystem projects) and Track 4 (tooling).

---

## Track 3: Ecosystem Compatibility & Interoperability

**Objective**: Ensure seamless adoption by TrueAGI / OpenCog Hyperon downstream projects and enable easy swapping with other MeTTa backends.

### Actionable Steps

1. Test and certify full compatibility with key libraries: `trueagi-io/PLN`, `trueagi-io/chaining`, `metta-attention`, and all examples in `trueagi-io/metta-examples`.
2. Implement the official Hyperon C/Rust API surface so `PeTTaEngine` can be used as a drop-in backend replacement for `hyperon-experimental`.
3. Add PyO3-based Python bindings (`petta-py`) that mirror the original `hyperon` PyPI package API.
4. Formalize the `git-import!` and `lib/` system with version pinning and lockfile support to guarantee reproducible environments.
5. Create a compatibility matrix in the README documenting which MeTTa features and extensions work 100% on this runtime.

**Dependencies / Prerequisites**: Track 1 and Track 2 (stable core).
**Integration Points**: Essential for Track 5 (community adoption).

---

## Track 4: Advanced Tooling & Developer Experience

**Objective**: Make PeTTa the most pleasant MeTTa runtime to develop with, debug, and deploy.

### Actionable Steps

1. Build a production-grade CLI with `clap` v4: subcommands for `run`, `repl`, `serve`, `debug`, `import`, and `bench`.
2. Develop an Axum/Warp-based HTTP + WebSocket server (inspired by MettaWamJam) exposing `PeTTaEngine` over REST and streaming endpoints.
3. Create a Language Server Protocol (LSP) implementation and Jupyter kernel (using the new Python bindings).
4. Add debugging tools: `--debug` mode that dumps intermediate Prolog goals, unification traces, and atom-space visualizations (Graphviz or Mermaid output).
5. Implement WASM target support (via `wasm-bindgen` once pure-Rust components are ready) for browser-based MeTTa playgrounds.
6. Add built-in visualization commands (`petta viz atomspace`) and a lightweight TUI REPL.

**Dependencies / Prerequisites**: Track 1 (enhanced engine) and Track 2 (performance hooks).
**Integration Points**: Feeds directly into Track 5.

---

## Track 5: Packaging, Distribution & Community

**Objective**: Turn the project into a professionally maintained, easily consumable open-source project with broad adoption.

### Actionable Steps

1. Publish the Rust crate to crates.io as `petta` (or `metta-petta`).
2. Set up GitHub Actions CI for: build, test, benchmarks, clippy, rustfmt, and cross-compilation (Linux, macOS, Windows).
3. Create pre-built binaries and installers via GitHub Releases; add `cargo install petta` support.
4. Expand the README with: architecture diagram, quick-start guides, full API reference, contribution guidelines, and a public roadmap (this document).
5. Add issue templates, pull-request template, CODE_OF_CONDUCT, and a Discord/Matrix channel link.
6. Prepare and submit a pull request upstream to `trueagi-io/PeTTa` offering the Rust layer as an official alternative backend.
7. Create a public roadmap issue and a "Call for Contributors" post highlighting the parallel tracks.

**Dependencies / Prerequisites**: Any of the other tracks (once a deliverable is ready).
**Integration Points**: All tracks converge here for release.

---

## Cross-Track Coordination

- **Weekly sync point**: Maintain a single GitHub Project board with columns per track and a "Ready for Integration" column.
- **Shared artifacts**: All tracks must keep the `examples/`, `lib/`, and `test.sh` suites passing at all times.
- **Versioning**: Use semantic versioning on the `petta` crate; maintain a `CHANGELOG.md`.
- **Release criteria**: All tracks must have at least their core steps (steps 1–3 in each) completed before a 1.0 release.

---

*This document is complete, self-contained, and ready for immediate execution. Every step is written as a direct, verifiable action. Contributors can claim any track or step independently, and the parallel structure maximizes velocity while preserving architectural integrity.*

*The project is already actively maintained (recent commits to Rust core and examples demonstrate momentum). Implementing this plan will establish PeTTa as a cornerstone runtime in the MeTTa ecosystem.*
