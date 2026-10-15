# Editor integration (Debug Adapter Protocol)

PeTTa ships a Debug Adapter Protocol (DAP) server so editors can drive the
debugger: set breakpoints, step, inspect the call stack and MeTTa variables, and
evaluate expressions while paused.

## The adapter

```bash
sh dap.sh
```

`dap.sh` launches `src/dap_server.pl`, which speaks DAP JSON over **stdin/stdout**
(`Content-Length`-framed messages). Program and debugger output go to **stderr**,
so the protocol channel stays clean.

The program to debug is taken from the `launch` request's `program` argument.

### Supported requests

| DAP request               | Mapped to                                             |
|---------------------------|-------------------------------------------------------|
| `initialize`              | capabilities + `initialized` event                    |
| `setBreakpoints`          | line breakpoints (`debug_break_line`)                 |
| `setFunctionBreakpoints`  | goal-head breakpoints (`debug_break_target`)          |
| `setExceptionBreakpoints` | break on `Error` results (`debug_break_error`)        |
| `launch` + `configurationDone` | load and run the `.metta` file with tracing      |
| `stackTrace`              | the MeTTa call stack                                  |
| `scopes` / `variables`    | the paused frame's MeTTa `$variable` bindings         |
| `evaluate`                | evaluate a MeTTa expression in the paused frame       |
| `continue` / `next` / `stepIn` / `stepOut` | resume / step                        |
| `disconnect` / `terminate`| stop the session                                      |

### Events

`initialized`, `stopped` (at a breakpoint), `terminated` and `exited` (when the
program finishes).

## Trying it without an editor (terminal client)

`dap_client.py` is a tiny readable DAP client. It launches `dap.sh`, sets a
function breakpoint, runs a file, and prints the stack + MeTTa variables at each
stop — the same exchange an editor performs, but in your terminal:

```bash
python3 editors/dap_client.py tests/debugger/fixtures/fib_buggy.metta fib 4
```

`tests/debugger/dap.sh` is a second worked example (it also exercises `evaluate`
and run-to-completion).

## VS Code

This folder is a ready-to-run (unpacked) VS Code extension that registers
`dap.sh` as a debug adapter of type `petta`. It is plain JavaScript — no build
or `npm install` is needed.

1. Open this extension folder in VS Code:
   ```bash
   code editors/vscode
   ```
2. Press **F5** ("Run Extension"). A second VS Code window opens (the Extension
   Development Host) with the extension active.
3. In that window, open the PeTTa repo folder and open a `.metta` file, e.g.
   `tests/debugger/fixtures/fib_buggy.metta`.
4. Set a breakpoint one of two ways:
   - click the gutter on the function's definition line (line 1 of `fib_buggy.metta`); or
   - in the **Run and Debug** view, add a **Function Breakpoint** named `fib`.
5. Press **F5** and choose **Debug PeTTa file** (it debugs the active file via
   `"program": "${file}"`).
6. Execution stops at the breakpoint. Use the **Call Stack** and **Variables**
   panels (you'll see `$N`), the step buttons (step in/over/out, continue), and
   the Debug Console's evaluate box (type a MeTTa expression like `(+ $N 1)`).

Notes / current limitations:

- Gutter breakpoints map to the breakpoint's source line; recursion through a
  function clause reports that clause's definition line.
- Program `stdout` (e.g. test results) goes to the adapter's stderr, not the
  Debug Console; the console is for `evaluate` results.
- The adapter resolves the repo root relative to this file, so keep the
  extension inside the PeTTa checkout (or open the repo as a workspace folder).
