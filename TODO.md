# PeTTa Status

## Current State (branch `swipl_b`)
- ✅ Rust + persistent SWI-Prolog subprocess via binary protocol
- ✅ 24/24 unit tests passing (15 original + 9 new)
- ✅ SWI loaded once (~70ms) instead of per-call (~73ms × N)
- ✅ Typed MeTTa values (`MettaValue` enum)
- ✅ Human-readable error messages (`SwiplErrorKind`)
- ✅ CLI: `--time` flag, colored output
- ✅ Binary length-prefixed protocol over stdin/stdout pipes
- ❌ MeTTa test output ("is X, should Y") goes to stderr, not CLI stdout

## Architecture
```
Rust PeTTaEngine (persistent)
  └── SWI-Prolog subprocess
        └── 7 Prolog files consulted once at startup
              └── server_loop reads binary protocol on stdin
                    └── responds with [status][count][results...] on stdout
                    └── all MeTTa output (format, writeln) → stderr
```

## Protocol
```
Ready:          Prolog sends 0xFF after startup
Request:        [type:1][len:4][payload:N]
                  type: 'F'(70)=file, 'S'(83)=string, 'Q'(81)=quit
Response:       [status:1]
                  0 = success: [count:4][str_len:4][str:N]...
                  1 = error: [msg_len:4][msg:N]
```

## Migration Attempts (Not Viable)
- **Scryer Prolog in-process**: crashes with complex codebases
- **Scryer Prolog subprocess**: directive compatibility issues
- **SWI embedding via `swipl` crate**: bindgen type mismatches, won't compile

## Performance
- **Engine startup**: ~70ms (once per PeTTaEngine instance)
- **Per-query overhead**: <1ms (binary protocol on pipes)
- **139 example tests**: ~20s wall time (limited by slowest single file at 16s)
- **Sequential 4 files**: ~1.3s (70ms startup + 1.2s computation)

## Running
```bash
cargo test            # 24 unit tests
sh test.sh            # 139 example tests (~20s parallel)
cargo run -- file.metta        # Run a MeTTa file
cargo run -- -t file.metta     # With timing
cargo run -- -v file.metta     # Verbose (debug output)
```
