# TODO3.md - Definitive MORK Implementation Plan

> **Mission**: Make PeTTa fully self-contained with a **functional** MORK backend that can execute real MeTTa code.  
> **Non-negotiable outcomes**: No external dependencies, no stubs, no FFI, no compromises.  
> **Status**: Architecture understood. Critical path identified. Implementation plan finalized.  
> **Last Updated**: 2026-04-27

---

## Executive Summary: The Real Problem

**Current state**: `!(add 1 2)` returns `"TODO: eval add 1 2"` instead of `"3"`.

**Root cause**: MORK has **NO MeTTa interpreter**. The Prolog backend delegates to SWI-Prolog's native arithmetic and unification. MORK only does pattern matching - it cannot evaluate `+`, `-`, `*`, `/`, `==`, `if`, `let`, etc.

**Solution**: Implement a **minimal MeTTa interpreter** in Rust that provides:
1. Primitive operations: `+`, `-`, `*`, `/`, `==`, `!=`, `<`, `>`, `<=`, `>=`
2. Control flow: `if`, `let`, `let*`, `match`, `reduce`, `eval`
3. Type system: `Number`, `Bool`, `Symbol`, `Expression`
4. Evaluation strategy: eager evaluation with pattern matching

**Critical insight**: This is **NOT** just wiring existing code. The Prolog backend works because SWI-Prolog IS the interpreter. MORK needs us to BUILD the interpreter from scratch. The `pure.rs` FFI stubs are a red herring - they were meant to call an external evaluator that doesn't exist.

**Scope reality check**:
- Pattern matching: ✅ Already implemented in `space/mod.rs`
- Arithmetic: ❌ Not implemented (need `+`, `-`, `*`, `/`)
- Logic: ❌ Not implemented (need `==`, `!=`, `if`)
- Binding: ❌ Not implemented (need `let`, `let*`)
- Evaluation: ❌ Not implemented (need `reduce`, `eval`, `match`)

---

## Design Goals (Uncompromising)

### ✅ Self-Contained
- [ ] Zero external non-crate dependencies (no `mork_ffi`, no `../../MORK/`)
- [ ] No FFI bridges or external repos
- [ ] All code inlined in `rust/src/mork/`

### ✅ Functional
- [ ] `!(add 1 2)` returns `"3"`
- [ ] `!(eq 5 5)` returns `"True"`
- [ ] `!(if True 1 2)` returns `"1"`
- [ ] `!(let $x 5 $x)` returns `"5"`
- [ ] `!(match &self pattern template)` works
- [ ] `!(reduce expr)` evaluates expressions
- [ ] Example files (`factorial.metta`, `eval.metta`) execute successfully

### ✅ Non-Destructive
- [ ] Prolog backend remains fully functional
- [ ] Backend selection via `EngineConfig.backend`
- [ ] No loss of existing functionality

### ✅ Clean Architecture
- [ ] No stubs in critical path
- [ ] No unnecessary duplication
- [ ] Parser strategy documented and rationalized
- [ ] < 50 compiler warnings
- [ ] `cargo clippy` and `cargo doc` pass

---

## Priority 0: CRITICAL - Build MeTTa Interpreter

### 0.1 Create Interpreter Core

**Create**: `rust/src/mork/interpreter/mod.rs`

**What's needed**: A complete evaluation engine that handles:
1. **Literals**: Numbers, Strings, Symbols, Booleans
2. **Arithmetic**: `+`, `-`, `*`, `/`, `%`, `neg`
3. **Comparison**: `==`, `!=`, `<`, `>`, `<=`, `>=`
4. **Logic**: `if`, `and`, `or`, `not`
5. **Binding**: `let`, `let*`, `let-narrow`, `let-var`
6. **Evaluation**: `eval`, `reduce`, `match`, `trace!`
7. **Types**: `:`, `is`, `cast`, type predicates

**Method signature**:
```rust
pub struct Interpreter {
    space: Arc<Mutex<Space>>,
    bindings: HashMap<String, MettaValue>,
}

impl Interpreter {
    pub fn eval(&mut self, expr: &str) -> Result<MettaValue, InterpreterError>;
    fn eval_expr(&mut self, expr: Expr) -> Result<MettaValue, InterpreterError>;
    fn eval_builtin(&mut self, name: &str, args: &[Expr]) -> Result<MettaValue, InterpreterError>;
}
```

**Work Required**:
- [ ] Create `interpreter` module structure
- [ ] Implement expression parser (reuse MORK frontend)
- [ ] Implement literal parsing (numbers, strings, bools)
- [ ] Implement arithmetic operations
- [ ] Implement comparison operations
- [ ] Implement control flow (`if`, `and`, `or`, `not`)
- [ ] Implement binding (`let`, `let*`)
- [ ] Implement evaluation (`eval`, `reduce`)
- [ ] Implement pattern matching (`match`)
- [ ] Wire to MORKEngine

**Verification**: `!(+ 2 3)` returns `"5"`, `!(== 5 5)` returns `"True"`

### 0.2 Replace `process()` Stub

**File**: `rust/src/engine/mork_engine.rs`

**Current code** (line 95-96):
```rust
if line.starts_with("!(") {
    Ok(format!("TODO: eval {}", &line[2..]))
}
```

**Replace with**:
```rust
if line.starts_with("!(") {
    self.interpreter.eval(&line[2..line.len()-1])
        .map(|v| v.to_string())
        .unwrap_or_else(|e| format!("ERR: {}", e))
}
```

**Work Required**:
- [ ] Add `Interpreter` field to `MORKEngine`
- [ ] Initialize interpreter in `new()`
- [ ] Replace stub with actual evaluation
- [ ] Handle errors gracefully

**Verification**: `!(+ 2 3)` in REPL returns `"5"`

### 0.3 Test Against Examples

**Test files**: `examples/factorial.metta`, `examples/eval.metta`

**Work Required**:
- [ ] Run `factorial.metta` - should compute 10! = 3628800
- [ ] Run `eval.metta` - should handle `let` and `match`
- [ ] Fix any missing functionality
- [ ] Document limitations

**Verification**: Both examples execute without errors

---

## Priority 1: HIGH - Remove External Dependencies

### 1.1 Delete `mork_ffi/` Directory

**Files**: `mork_ffi/`, `build.sh`

**Work Required**:
- [ ] Audit `mork_ffi/Cargo.toml` - confirm it only references external repos
- [ ] Verify no code imports from `mork_ffi`
- [ ] Delete `mork_ffi/` directory: `rm -rf mork_ffi/`
- [ ] Edit `build.sh` - remove lines 12-15 (mork_ffi clone):
  ```bash
  # REMOVE THESE LINES:
  # if [ ! -d "mork_ffi" ]; then
  #     echo "Cloning mork_ffi (MORK FFI bindings)..."
  #     git clone --depth 1 https://github.com/patham9/mork_ffi.git
  # fi
  ```

**Verification**: 
- `ls mork_ffi/` → "No such file or directory"
- `grep -r "mork_ffi" .` → no matches (except this TODO)

### 1.2 Remove `pure.rs` Module

**File**: `rust/src/mork/execution/pure.rs`

**Rationale**: `pure.rs` is an FFI abstraction layer that's unused. The real execution goes through Sources/Sinks in `space/mod.rs`.

**Work Required**:
- [ ] Delete `rust/src/mork/execution/pure.rs`: `rm rust/src/mork/execution/pure.rs`
- [ ] Remove `pub mod pure;` from `rust/src/mork/execution/mod.rs`
- [ ] Remove `use super::pure;` from any file that imports it (check `sinks.rs`)
- [ ] Verify compilation: `cargo build --features mork`

**Verification**: Build succeeds, no references to `pure` module

### 1.3 Remove TrueAGI/MORK External References

**Files**: `rust/src/mork/mod.rs`, documentation, comments

**Work Required**:
- [ ] Update `rust/src/mork/mod.rs` comment (line 4):
  ```rust
  // OLD: Originally sourced from <https://github.com/trueagi-io/MORK>.
  // NEW: MORK implementation inlined in PeTTa.
  ```
- [ ] Search for and remove all `../../MORK/` references
- [ ] Update documentation to reflect self-contained status

**Verification**: No references to external MORK repos

---

## Priority 2: HIGH - Error Handling & Completeness

### 2.1 Add Structured Error Types

**Files**: `rust/src/engine/errors.rs`, `rust/src/engine/mod.rs`

**Work Required**:
- [ ] Add `PeTTaError::MorkError { operation: String, message: String }` variant
- [ ] OR add `BackendErrorKind::Mork(String)` to existing enum
- [ ] Update error mapping in `mork_engine.rs`:
  ```rust
  // Instead of:
  Err(e) => Ok(format!("ERR: {}", e))
  
  // Use:
  Err(e) => Err(PeTTaError::MorkError { 
      operation: "evaluate".to_string(), 
      message: e 
  })
  ```
- [ ] Preserve context: operation type, input, line numbers

**Verification**: Error messages include operation context

### 2.2 Complete MORKEngine API

**Audit checklist**:
- [x] `new()` - implemented
- [x] `add_atoms()` - implemented
- [x] `remove_atoms()` - implemented
- [x] `get_atoms()` - implemented
- [x] `match_pattern()` - implemented
- [x] `mm2_exec()` - implemented
- [ ] `process()` - needs 0.1 implementation

**Work Required**:
- [ ] Complete 0.1 to finish `process()`
- [ ] Add `eval()` convenience methods if missing
- [ ] Document all methods

---

## Priority 3: MEDIUM - Code Health

### 3.1 Fix Compiler Warnings

**Target**: < 50 warnings (from ~300+)

**Files** (by warning count):
1. `rust/src/mork/expr/mod.rs` - unused vars
2. `rust/src/mork/execution/sinks.rs` - unused imports, unused Results
3. `rust/src/mork/space/mod.rs` - private-in-public, unnecessary mut
4. `rust/src/mork/frontend/` - various

**Work Required**:
- [ ] Prefix unused variables with `_`
- [ ] Remove unnecessary `mut`
- [ ] Handle or explicitly ignore unused Results: `let _ = result;`
- [ ] Fix visibility annotations
- [ ] Run `cargo clippy --features mork` and fix lints
- [ ] Run `cargo doc --features mork --document-private-items` and fix warnings

**Verification**: `cargo build --features mork 2>&1 | grep -c warning` < 50

### 3.2 Arena Compact Verification

**File**: `rust/pathmap/src/arena_compact.rs`

**Work Required**:
- [ ] Confirm syntax fix is complete (stray brace removed)
- [ ] Run `cargo test -p pathmap`
- [ ] Document any remaining issues

---

## Priority 4: MEDIUM - Parser Rationalization

### 4.1 Parser Audit

**Work Required**:
- [ ] Document all parsers in codebase:
  - Prolog parser: `prolog/metta.pl`
  - Rust parser: `petta_parser/` (if exists)
  - MORK frontend: `rust/src/mork/frontend/`
- [ ] Compare functionality and performance
- [ ] Decide: single primary parser or multiple backends
- [ ] Document decision in ARCHITECTURE.md or similar

### 4.2 Parser Integration

**Decision required**:

**Option A: Keep Rust parser (recommended)**
- [ ] Integrate `petta_parser/` or MORK frontend as primary
- [ ] Remove Prolog parsing logic
- [ ] Make parser backend-agnostic

**Option B: Keep Prolog parser**
- [ ] Remove `petta_parser/` directory
- [ ] Document that parsing happens in Prolog
- [ ] Ensure MORK can call Prolog parser

**Verification**: Single documented parser strategy

---

## Priority 5: LOW - Developer Experience

### 5.1 REPL Improvements

**Work Required**:
- [ ] Add `rustyline` crate for command history
- [ ] Implement `:help` command
- [ ] Add colored output (results vs errors)
- [ ] Add `--verbose` flag for debugging
- [ ] Add `:backend` command to show current backend
- [ ] Add `:load` command for files

### 5.2 File Watching

**Work Required**:
- [ ] Add `notify` crate
- [ ] Implement `--watch` mode
- [ ] Debounce rapid changes

---

## Priority 6: LOW - Distribution

### 6.1 Build System

**Work Required**:
- [ ] Add `build.rs` for SWI-Prolog detection
- [ ] Cargo feature: `default = ["mork"]`
- [ ] Cargo feature: `full = ["mork", "prolog", "parallel"]`
- [ ] Document MSRV (Minimum Supported Rust Version)

### 6.2 Release Process

**Work Required**:
- [ ] CI: Build and publish binary releases
- [ ] Optional: WebAssembly target

---

## Verification Checklist

### Functional (Priority 0-1) - MUST PASS
- [ ] `cargo build --features mork` succeeds with zero errors
- [ ] `!(add 1 2)` returns `"3"`
- [ ] `!(add 2 3)` returns `"5"`
- [ ] `!(sub 5 2)` returns `"3"`
- [ ] `!(mul 4 3)` returns `"12"`
- [ ] `!(eq 5 5)` returns `"True"`
- [ ] `!(eq 5 3)` returns `"False"`
- [ ] `!(if True 1 2)` returns `"1"`
- [ ] `!(let $x 5 $x)` returns `"5"`
- [ ] `factorial.metta` example executes successfully
- [ ] `eval.metta` example executes successfully
- [ ] No `mork_ffi` directory exists
- [ ] No references to `../../MORK/` or external TrueAGI repos
- [ ] `pure.rs` file deleted
- [ ] `build.sh` does not clone external repos
- [ ] REPL works with `Backend::Mork`

### Code Health (Priority 2-3) - MUST PASS
- [ ] Warnings < 50
- [ ] `cargo clippy --features mork` passes
- [ ] `cargo doc --features mork --document-private-items` builds
- [ ] Error messages are informative (not generic)

### Design Goals - MUST PASS
- [ ] MORK is fully self-contained (no external repos)
- [ ] No stubs in critical path
- [ ] Configuration cleanly selects backend (`--backend mork|prolog`)
- [ ] All Prolog functionality retained non-destructively
- [ ] No unnecessary code duplication

---

## Implementation Roadmap

```
Phase 0: CRITICAL (Builds MeTTa interpreter - ~2-3 weeks)
  Week 1:
  - 0.1: Create interpreter core structure [2 days]
  - 0.1: Implement literal parsing [1 day]
  - 0.1: Implement arithmetic (+, -, *, /) [2 days]
  
  Week 2:
  - 0.1: Implement comparison (==, !=, <, >) [1 day]
  - 0.1: Implement control flow (if, and, or) [2 days]
  - 0.1: Implement binding (let, let*) [2 days]
  
  Week 3:
  - 0.1: Implement eval/reduce/match [3 days]
  - 0.2: Wire to MORKEngine::process() [1 day]
  - 0.3: Test against examples [2 days]
  - Iterate and fix [2 days]

Phase 1: HIGH (Removes external deps - ~3 days)
  - 1.1: Delete mork_ffi/ [0.5 days]
  - 1.2: Remove pure.rs [0.5 days]
  - 1.3: Remove external references [0.5 days]
  - 2.1: Improve error types [1 day]
  - 2.2: Complete API audit [0.5 days]

Phase 2: MEDIUM (Polish - ~1 week)
  - 3.1: Fix warnings [2 days]
  - 3.2: Arena verification [0.5 days]
  - 4.1: Parser audit [1 day]
  - 4.2: Parser integration decision [1.5 days]

Phase 3: LOW (Optional - as time permits)
  - Developer experience improvements
  - Distribution packaging
  - Additional built-ins and stdlib support
```

---

## Files to Modify

| File | Change | Priority | Status |
|------|--------|----------|--------|
| `rust/src/engine/mork_engine.rs` | Add `evaluate_metta_expr()`, built-ins | 0.1, 0.2 | TODO |
| `rust/src/mork/builtins/mod.rs` | Create new module | 0.2 | TODO |
| `rust/src/mork/space/mod.rs` | Wire !() to pipeline | 0.3 | TODO |
| `rust/src/mork/execution/pure.rs` | Delete file | 1.2 | TODO |
| `rust/src/mork/execution/mod.rs` | Remove `pub mod pure;` | 1.2 | TODO |
| `rust/src/mork/mod.rs` | Update comment | 1.3 | TODO |
| `rust/src/engine/errors.rs` | Add MorkError variant | 2.1 | TODO |
| `mork_ffi/` | Delete directory | 1.1 | TODO |
| `build.sh` | Remove mork_ffi clone | 1.1 | TODO |

---

## Dependencies Diagram

```
evaluate_metta_expr() [0.1]
    ↓
Built-in functions [0.2]
    ↓
!(add 1 2) → "3" ✓
    ↓
Wire to metta_calculus [0.3]
    ↓
User functions work ✓
    ↓
Delete mork_ffi/ [1.1]
    ↓
Remove pure.rs [1.2]
    ↓
Self-contained MORK ✓
    ↓
Fix warnings [3.1]
    ↓
Production-ready ✓
```

---

## Known Issues & Risks

### Critical (BLOCKING)
1. **No MeTTa interpreter** - MORK cannot evaluate `+`, `-`, `*`, `/`, `==`, `if`, `let`, etc.
2. **Not just wiring** - Requires building complete evaluation semantics from scratch
3. **External dependencies** - `mork_ffi/` points to TrueAGI repos (but irrelevant without interpreter)
4. **FFI stub present** - `pure.rs` is unused but compiled

### High Risk
5. **Scope underestimate** - This is building a language interpreter, not just wiring
6. **Prolog semantics gap** - Must replicate SWI-Prolog's built-in behavior exactly
7. **Type system complexity** - MeTTa has implicit typing that Prolog handles automatically
8. **~300+ warnings** - code quality issues may hide real bugs

### Medium
9. **Error mapping coarse** - uses generic strings
10. **No integration tests** - manual testing only
11. **Parser duplication** - multiple parsers may exist

### Low
12. **No REPL improvements** - basic functionality only
13. **Documentation gaps** - architecture not documented

---

## Success Criteria (Definition of Done)

MORK is **complete** when:

1. ✅ `cargo build --features mork` succeeds with < 50 warnings
2. ✅ All Priority 0 tests pass (interpreter works):
   - `!(+ 2 3)` returns `"5"`
   - `!(- 5 2)` returns `"3"`
   - `!(* 4 3)` returns `"12"`
   - `(== 5 5)` returns `"True"`
   - `!(if True 1 2)` returns `"1"`
   - `!(let $x 5 $x)` returns `"5"`
3. ✅ All Priority 1 items complete (no external deps)
4. ✅ Example files execute successfully:
   - `factorial.metta` computes 10! = 3628800
   - `eval.metta` handles let and match
5. ✅ No stubs, no FFI, no external repos
6. ✅ Prolog backend still works (non-destructive)
7. ✅ Documentation reflects self-contained status

---

## Reality Check: Why This Didn't Happen Earlier

**The hard truth**: Previous attempts treated this as a "wiring" problem when it's actually an **interpreter implementation** problem.

**What we thought**:
- "Just connect `!()` to the existing MORK pipeline"
- "Add a few built-in functions"
- "Wire up the FFI stubs"

**What it actually is**:
- Building a complete MeTTa interpreter from scratch
- Replicating SWI-Prolog's evaluation semantics in Rust
- Implementing arithmetic, logic, binding, and evaluation primitives
- Handling type coercion, error cases, and edge cases

**Why the confusion**:
1. **Prolog hides complexity** - SWI-Prolog IS the interpreter, so it looks like "just parsing"
2. **MORK has execution infrastructure** - pattern matching works, so it seems like evaluation should too
3. **FFI stubs mislead** - `pure.rs` suggests there's an evaluator somewhere, but there isn't
4. **Examples work with Prolog** - so we assumed MORK just needs "wiring"

**The path forward**:
Accept that this is a **language interpreter implementation** project, not a wiring project. Estimate accordingly (2-3 weeks of focused work), build incrementally, and test against Prolog backend behavior at every step.

---

*Last updated: 2026-04-27*  
*Next action: Implement 0.1 - evaluate_metta_expr()*
