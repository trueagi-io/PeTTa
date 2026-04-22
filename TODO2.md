# PeTTa Development Plan v2

> "More for less" - maximize impact per unit of effort

## Core Philosophy

- Keep both SWI-Prolog and MORK backends (user choice)
- Make MORK the default; SWI-Prolog remains available
- Unify public APIs regardless of backend choice
- Add value through smart integration, not code volume

## Quick Start

```
Priority: A1 + A4  → MORK default with CLI flag
Follow:  B1 + B2  → Unified errors
Then:    C1 + C2  → Basic REPL with :help
Finally: H1 + H2  → Clippy/doc pass
```

---

## Track A: MORK as Default (Priority: HIGH)

**Goal**: Elevate MORK to production status, make it the default.

**Tasks**:
- [ ] A1. Flip default in `EngineConfig` from Prolog to MORK
- [ ] A2. Run comprehensive MORK vs Prolog benchmarks
- [ ] A3. Document performance characteristics of each backend
- [ ] A4. Add `--backend prolog|mork` CLI flag to override default
- [ ] A5. Remove "experimental" labels from MORK documentation
- [ ] A6. Add integration test suite that runs on both backends

**Depends on**: B (unified errors helps benchmark output)

**Effort**: Medium | **Impact**: High

---

## Track B: Unified Error Model (Priority: HIGH)

**Goal**: One error type serves all backends. Users don't need to know which backend is running.

**Tasks**:
- [ ] B1. Extend `PeTTaError` enum covering all failure modes
- [ ] B2. Backend-specific errors map to common variants
- [ ] B3. Error messages include source location when available
- [ ] B4. Unified error display in CLI (not "Prolog said..." vs "MORK said...")

**Effort**: Low | **Impact**: High

---

## Track C: Developer Experience (Priority: MEDIUM)

**Goal**: Make interactive use pleasant and productive.

**Tasks**:
- [ ] C1. REPL with command history (using `rustyline`)
- [ ] C2. Built-in `:help` command listing available commands
- [ ] C3. `--watch` mode: reload file on change (using `notify`)
- [ ] C4. Colored output for results vs errors
- [ ] C5. `--verbose` flag shows backend communication (for debugging)

**Effort**: Low | **Impact**: High

---

## Track D: Parser Rationalization (Priority: MEDIUM)

**Goal**: Eliminate redundant parsing code.

**Analysis required** (prerequisite):
- [ ] D0. Audit: Prolog parser vs `petta_parser/` vs MORK frontend
- [ ] D0. Decide: Keep one primary parser, deprecate others

**If keeping Rust parser**:
- [ ] D1. Integrate `petta_parser/` as production parser
- [ ] D2. Remove parsing logic from Prolog backend
- [ ] D3. Parser becomes backend-agnostic

**If keeping Prolog parser**:
- [ ] D1. Remove `petta_parser/` directory (dead code)
- [ ] D2. Document that parsing happens in Prolog

**Effort**: Medium | **Impact**: Medium (maintenance)

---

## Track E: Embedding & FFI (Priority: MEDIUM)

**Goal**: Make PeTTa easy to embed in other Rust applications.

**Tasks**:
- [ ] E1. Document `PeTTaEngine` API for library use
- [ ] E2. Add example: minimal Rust program that runs MeTTa
- [ ] E3. Ensure no static globals (thread-safety for embedding)
- [ ] E4. Consider `no_std` support for embedded systems

**Effort**: Medium | **Impact**: High (ecosystem)

---

## Track F: Distribution (Priority: LOW)

**Goal**: Make PeTTa easy to install and distribute.

**Tasks**:
- [ ] F1. Add `build.rs` for conditional SWI-Prolog detection
- [ ] F2. Cargo feature: `default = ["mork"]` (no Prolog by default)
- [ ] F3. Cargo feature: `full = ["mork", "prolog", "parallel"]`
- [ ] F4. CI: Build and publish binary releases
- [ ] F5. Optional: WebAssembly target (`wasm32-unknown-unknown`)

**Effort**: Low | **Impact**: Medium

---

## Track G: Tooling (Priority: LOW)

**Goal**: Richer development ecosystem.

**Tasks**:
- [ ] G1. `:load` command in REPL to load files
- [ ] G2. `:ast` command to print parsed MeTTa AST
- [ ] G3. `:match` command to show pattern matching results
- [ ] G4. Basic profiler: `:profile <expression>` reports timing

**Effort**: Low | **Impact**: Low-Medium

---

## Track H: Code Health (Priority: ONGOING)

**Goal**: Maintainable codebase.

**Tasks**:
- [ ] H1. Ensure `cargo clippy` passes with no warnings
- [ ] H2. Ensure `cargo doc --document-private-items` builds
- [ ] H3. Add `rustfmt.toml` and enforce formatting in CI
- [ ] H4. Minimum Rust version (MSRV) documented and tested

**Effort**: Low | **Impact**: Medium

---

## Suggested Execution Order

```
Start with: A → B → C (quick wins, high impact)

Then:       E → D (embedding helps test parser decisions)

Then:       F → G → H (polish)

Optional:   Return to any track for deeper work
```

---

## Effort Summary

| Track | Effort | Impact | Start With |
|-------|--------|--------|------------|
| A: MORK Default | Medium | High | ✓ |
| B: Unified Errors | Low | High | ✓ |
| C: Dev Experience | Low-Medium | Medium | ✓ |
| D: Parser Cleanup | Medium | Medium | |
| E: Embedding | Medium | High | |
| F: Distribution | Low | Medium | |
| G: Tooling | Low | Low-Medium | |
| H: Code Health | Low | Medium | |

---

## Notes

- **Tracks are independent** - pick any to start
- **Skip what you don't need** - e.g., skip F if not distributing
- **Combine for efficiency** - e.g., B (errors) helps C (REPL)
- **Revisit later** - some tracks have natural "phase 2" items