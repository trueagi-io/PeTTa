# Prolog Backend

This directory contains the SWI-Prolog source files that implement the core MeTTa
language semantics. These files are loaded by the `PeTTaEngine` at startup via the
binary protocol FFI boundary.

## Files

| File | Purpose |
|------|---------|
| `parser.pl` | DCG-based S-expression tokenizer/parser (`sread/2`, `swrite/2`) |
| `translator.pl` | MeTTa → Prolog goal compilation engine |
| `specializer.pl` | Higher-order function specialization |
| `spaces.pl` | Atom space management and pattern matching |
| `filereader.pl` | File loading and form parsing |
| `metta.pl` | Standard library, builtins, arithmetic, types, entry point |
| `utils.pl` | Shared utility predicates |
| `main.pl` | Standalone entry point (bypasses Rust, for Prolog-only testing) |

## FFI Boundary

The Rust ↔ Prolog boundary is defined as follows:

### Protocol (Rust → Prolog)
- **Request type byte**: `'F'(70)` = file path, `'S'(83)` = string, `'Q'(81)` = quit, `'C'(67)` = cancel
- **Payload**: `[4 bytes big-endian u32 length][N bytes UTF-8]`

### Protocol (Prolog → Rust)
- **Status byte**: `0` = success, `1` = error
- **Success**: `[4 bytes result count]` then per-result: `[4 bytes str len][N bytes UTF-8]`
- **Error**: `[4 bytes error msg len][N bytes UTF-8 error message]`

### Entry Points
- `load_metta_file(+FilePath:atom, -Results:list)` — Load and execute a `.metta` file
- `process_metta_string(+Query:string, -Results:list)` — Parse and execute MeTTa code

### Initialization
The Prolog server is initialized by the Rust engine via a temporary script that:
1. Consults all `.pl` files in this directory
2. Sets the `silent/1` flag based on verbosity configuration
3. Initializes the type lookup cache (`nb_setval(fun_types, [])`)
4. Enters `server_loop/0` which reads binary requests from stdin

## Migration Note

These files were moved from `src/` to `prolog/` to establish a clear FFI boundary
between the Rust layer and the Prolog backend. The Rust code in `rust/src/lib.rs`
builds the server script dynamically from these files at runtime.
