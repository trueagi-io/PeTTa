# TODO.md - Self-Contained PeTTa Implementation

## Goal
Make PeTTa **entirely self-contained** - no FFI, no stubs, no non-crate external dependencies. All functionality inlined as Rust modules that can be refactored as one coherent unit.

---

## Current State Analysis

### What We Have
- `rust/src/engine/` - SWI-Prolog subprocess backend (working)
- `rust/src/mork/` - MORK execution modules (PARTIALLY FULL)
- `rust/src/pathmap/` - Trie data structure (working, but `arena_compact.rs` has syntax bug)
- `rust/pathmap-macros/` - Proc-macro crate (working)
- `prolog/` - Prolog standard library (working)
- `lib/` - MeTTa standard library (working)
- External `mork_ffi/` - C FFI bridge (fetched by build.sh)

### MORK Implementation Status

| Module | Status | Description |
|--------|--------|-------------|
| `space/mod.rs` | FULL | Space implementation using PathMap (2000+ lines) |
| `expr/mod.rs` | FULL | Expression types and operations |
| `frontend/mod.rs` | FULL | Parser implementations |
| `interning/mod.rs` | FULL | Symbol interning |
| `execution/pure.rs` | STUB | FFI interface - needs implementation |
| `engine/mork_engine.rs` | IN_PROGRESS | Created, needs wiring to engine |
| `pathmap/arena_compact.rs` | BROKEN | Unclosed braces - syntax error |

---

## Implementation Plan

### Phase 1: Fix Compilation Errors

#### 1.1 Fix arena_compact.rs syntax errors
- [ ] Remove duplicate closing brace at line 236
- [ ] Review and fix any remaining unclosed delimiters
- [ ] Verify `--features mork` compiles successfully

### Phase 2: Connect Internal MORK

#### 2.1 Wire MORKEngine to PeTTaEngine
- [ ] In `rust/src/engine/mod.rs`: Check `config.backend` value
- [ ] When `Backend::Mork`: Use `MORKEngine` instead of subprocess
- [ ] Implement `process_metta_string()` for MORKEngine
- [ ] Implement `load_metta_file()` for MORKEngine

#### 2.2 Implement MORK commands (if not already in space/mod.rs)
- [ ] `add_atoms()` - Add atoms to space
- [ ] `remove_atoms()` - Remove atoms from space  
- [ ] `match_pattern()` - Pattern matching query
- [ ] `get_atoms()` - List all atoms
- [ ] `mm2_exec()` - MM2 calculus execution

#### 2.3 Remove stub in execution/pure.rs
- [ ] Either remove or connect to actual implementation

### Phase 3: Remove FFI Dependencies

#### 3.1 Remove external mork_ffi
- [ ] Remove `mork_ffi` git clone from `build.sh`
- [ ] Delete `mork_ffi/` directory from repo

#### 3.2 Remove Prolog FFI loading
- [ ] Remove FFI loading from `prolog/metta.pl` (line with `../mork_ffi/morkspaces`)
- [ ] Remove `morkspaces.pl` reference if no longer needed

### Phase 4: Verify Self-Contained

- [ ] `cargo build --release` succeeds with no external downloads
- [ ] All examples pass with `-b mork`
- [ ] All examples pass with default backend
- [ ] No FFI, no stubs, no external non-crate dependencies
- [ ] Code can be refactored as single coherent unit

---

## Architecture Target

```
petta/
├── rust/
│   ├── src/
│   │   ├── engine/      # Subprocess + MORK backends (unified)
│   │   ├── mork/       # Pure Rust MORK implementation
│   │   ├── pathmap/    # Trie structures
│   │   └── ...        # Other modules
│   ├── pathmap-macros/ # Proc-macros (crate)
│   └── petta/        # Main binary
├── lib/              # MeTTa stdlib
└── prolog/           # Prolog stdlib (minimal)
```

---

## Success Criteria
- [ ] `cargo build --release` succeeds with no external downloads
- [ ] All examples pass with `-b mork`
- [ ] No FFI, no stubs, no external non-crate deps
- [ ] Code can be refactored as single coherent unit