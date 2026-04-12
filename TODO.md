# PeTTa Status

## Current State (branch `swipl_b`)
- ✅ Rust subprocess wrapper works (24/24 unit tests, 139/139 example tests)
- ✅ SWI-Prolog `.pl` files are untouched originals
- ✅ Python files removed, scripts updated
- ✅ Typed MeTTa values (`MettaValue` enum) with S-expression parser
- ✅ Human-readable error messages (`SwiplErrorKind`)
- ✅ CLI: `--time` flag, colored output, batch file loading
- ❌ Still depends on external `swipl` binary (not self-contained)
- ❌ No persistent state between calls (one process per query)

## Architecture
```
Rust binary (petta) + Rust library (petta crate)
  └── SWI-Prolog subprocess (swipl, one per call)
        └── MeTTa runtime (7 Prolog files in src/)
              └── MeTTa code (.metta files)
```

## Migration Attempts (Not Viable)

### Scryer Prolog (0.10.0)
- **In-process Rust API**: Crashes with `index out of bounds` on complex Prolog codebases (internal Scryer heap/trail bug)
- **Scryer subprocess**: `:- dynamic` and `:- discontiguous` directives cause `syntax_error(incomplete_reduction)` when loading via `consult/1`
- **DCG incompatibility**: Scryer's `library(dcgs)` uses different semantics than SWI's `library(dcg/basics)`

### SWI-Prolog embedding via `swipl` crate (0.3.16)
- **Compilation errors**: bindgen-generated bindings return `i32` where crate expects `bool`
- **Root cause**: Version incompatibility between swipl-rs and installed SWI-Prolog
- **The `swipl` crate is designed for writing Proloadable Rust libraries, not embedding SWI in Rust binaries**

### What the Python version had that we don't
- **Persistent engine**: Python uses `janus_swi` — SWI-Prolog's official Python FFI that loads SWI as a `.so` into the Python process. One startup, one engine, all queries share state.
- **No Rust equivalent exists**: The `swipl-fli` crate provides raw FFI bindings but requires manual `PL_initialise()`/`PL_open_query()`/`PL_next_solution()` calls — essentially re-implementing janus_swi in Rust.

## Running
```bash
cargo test            # 24 unit tests
sh test.sh            # 139 example tests (~20s parallel)
cargo run -- file.metta        # Run a MeTTa file
cargo run -- -t file.metta     # With timing
```

## Performance
- **SWI startup**: ~73ms per subprocess call
- **Small file (identity.metta)**: 92ms total (73ms startup + ~19ms overhead)
- **Large file (matespace2.metta)**: 16.2s (startup is negligible)
- **139 tests parallel**: ~20s wall time (limited by slowest single file)
- **Startup overhead dominates small files** — ~12s of 76s total CPU time is subprocess startup
