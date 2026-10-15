# PeTTa Debugger Guide

This guide focuses on practical debugging workflows in PeTTa.

## Quick start (new to PeTTa? start here)

You only need one command to begin. `watch` traces your program and shows the
calls nested by depth — no flags to assemble:

```bash
# trace everything the program does
sh debug.sh watch your_file.metta

# trace just one function's calls (much less noise)
sh debug.sh watch your_file.metta yourFunction
```

For example, on the intentionally broken `tests/debugger/fixtures/fib_buggy.metta`:

```bash
sh debug.sh watch tests/debugger/fixtures/fib_buggy.metta fib
```

```text
ENTER  (fib 10)
  ENTER  (fib 9)
    ENTER  (fib 8)
      ...
                ENTER  (fib 2)
                  ENTER  (fib -1)      <- a negative input: the bug is here
                  OK     (fib -1) => -1
                OK     (fib 2) => 0    <- (fib 2) should be 1, not 0
```

The indentation shows who called whom; reading down to where a value first goes
wrong points at the bug.

When you want to stop and poke at a specific function, set a breakpoint:

```bash
sh debug.sh your_file.metta --debug-break=yourFunction
```

If you mistype a flag or category, PeTTa tells you (and suggests the closest
match) instead of silently doing nothing. Run `sh debug.sh --debug-help` for the
full list of options; the rest of this guide covers them in depth.

## Editor integration (DAP)

PeTTa includes a Debug Adapter Protocol server so editors can drive the debugger
(breakpoints, stepping, stack/variable inspection, evaluation):

```bash
sh dap.sh
```

It speaks DAP JSON over stdin/stdout. See [editors/README.md](./editors/README.md)
for the supported requests and a VS Code launch configuration.

## Entry points

Show debugger help:

```bash
sh debug.sh
```

Run a file with the debugger:

```bash
sh debug.sh your_file.metta --debug=runtime
```

Run the debugger smoke test:

```bash
sh debug_test.sh
```

## Main debug modes

- `--debug=runtime`: MeTTa-first runtime trace.
- `--debug=runtime-prolog`: low-level runtime trace with compiled Prolog goals.
- `--debug=source,parse,translate,result`: source-to-result pipeline trace.
- `--debug=space`: atom-space mutations and matches.
- `--debug-output=<file>`: write a plain-text copy of debugger output to a file while still showing terminal output.
- `--debug-format=json` (alias `--debug-jsonl`): emit events as machine-readable JSON, one object per line.
- `--silent`: suppress the legacy compile/run pretty-print and keep the debugger output.

## Machine-readable JSON output

Add `--debug-format=json` (or `--debug-jsonl`) to any debug run to emit one JSON
object per line instead of the human-readable trace. This is the format intended
for tooling and editor integration.

```bash
sh debug.sh examples/mettaset.metta --debug=runtime --debug-goal=match --debug-format=json --silent
```

```json
{"event":"runtime","stage":"enter","goal":"(match &self (set $x $y) (set $x $y))","index":2,"line":6,"depth":1,"vars":{}}
{"event":"runtime","stage":"success","goal":"(match &self (set 1 a) (set 1 a))","index":2,"line":6,"depth":1,"vars":{},"result":"(set 1 a)"}
{"event":"runtime","stage":"redo","goal":"(match &self (set 1 b) (set 1 b))","index":2,"line":6,"depth":1,"vars":{}}
```

Event kinds:

- `runtime`: `stage` is `enter`/`success`/`redo`/`fail`; carries `goal`, `index`,
  `line`, `depth`, `vars` (MeTTa variable bindings), and `result` on success.
- `space`: `op` is `add`/`remove`/`match`/`get_atoms`, with `space`, `term`/`pattern`,
  and `result`/`removed` as applicable.
- `result`: the final results of a runnable form.

Combine with `--debug-goal`, `--debug-depth`, and `--debug-max-events` to scope
the stream. Use `--silent` so program output does not interleave with the JSON.

## Common workflows

### Wrong result

Start with the function you suspect:

```bash
sh debug.sh your_file.metta --debug=runtime --debug-goal=yourFunc --silent
```

This shows calls and returned values such as:

```text
ENTER  (yourFunc 7 3)
OK     (yourFunc 7 3) => 4
```

### Nondeterministic goals (multiple solutions)

MeTTa goals such as `match`, `superpose`, and `case` can produce several
solutions through backtracking. The runtime trace models this with a `REDO`
port: the first solution is an `OK`, and each later solution reached on
backtracking is preceded by a `REDO`.

```bash
sh debug.sh examples/mettaset.metta --debug=runtime --debug-goal=match --silent
```

```text
ENTER  (match &self (set $x $y) (set $x $y))
OK     (match &self (set 1 a) (set 1 a)) => (set 1 a)
REDO   (match &self (set 1 b) (set 1 b))
OK     (match &self (set 1 b) (set 1 b)) => (set 1 b)
...
```

### Wrong operator

If a function may be calling the wrong built-in, trace the operator directly.

Example from the intentionally broken [tests/debugger/fixtures/add_buggy.metta](./tests/debugger/fixtures/add_buggy.metta):

```bash
sh debug.sh tests/debugger/fixtures/add_buggy.metta --debug=runtime --debug-goal=- --silent
```

This makes the suspicious call explicit:

```text
ENTER  (- 7 3)
OK     (- 7 3) => 4
```

### Wrong recursion

Use a conditional breakpoint to stop on impossible states.

Example from [tests/debugger/fixtures/fib_buggy.metta](./tests/debugger/fixtures/fib_buggy.metta):

```bash
sh debug.sh tests/debugger/fixtures/fib_buggy.metta --debug-break-if='fib:arg1<0' --debug-break-once --debug-goal=fib --silent
```

This stops the first time recursion reaches a negative input.

### Wrong return value

Break on a suspicious return value:

```bash
sh debug.sh tests/debugger/fixtures/fib_buggy.metta --debug-break-if='fib:result=0' --debug-break-once --debug-goal=fib --silent
```

Result conditions only trigger on successful returns.

### Space and match problems

Trace atom-space operations:

```bash
sh debug.sh examples/spaces.metta --debug=space --silent
```

This shows:

- atoms added to spaces
- successful matches
- failed matches
- `get-atoms` activity

## Breakpoints

Break on function head:

```bash
sh debug.sh your_file.metta --debug-break=yourFunc --debug-goal=yourFunc --silent
```

Break on a condition:

```bash
sh debug.sh your_file.metta --debug-break-if='yourFunc:arg1<0' --debug-goal=yourFunc --silent
```

Supported fields:

- `argN`
- `result`

Supported operators:

- `=`
- `!=`
- `>`
- `<`
- `>=`
- `<=`
- `~` substring / structural contains (text or MeTTa expression)

Values can be numbers, atoms, `true`/`false`, or a MeTTa s-expression in
parentheses for structural matching, e.g.:

```bash
sh debug.sh examples/mettaset.metta --debug-break-if='match:arg3~set' --debug-goal=match --silent
sh debug.sh your_file.metta --debug-break-if='f:arg1=(a b c)' --debug-goal=f --silent
```

Supported condition combinators:

- `&` for `and`
- `|` for `or`

### Break on space mutation, match failure, or error

Beyond goal-head and conditional breakpoints, you can break on space and
control-flow events:

```bash
# Break when an atom is added to or removed from a space (comma-separated, * = any space)
sh debug.sh examples/spaces.metta --debug-break-space='&self' --debug-break-once --silent

# Break when a space match fails
sh debug.sh your_file.metta --debug-break-match-fail --debug-break-once --silent

# Break when a goal returns an Error term
sh debug.sh examples/he_error.metta --debug-break-error --debug-break-once --silent
```

Examples:

```bash
sh debug.sh tests/debugger/fixtures/fib_buggy.metta --debug-break-if='fib:arg1=2&result=0' --debug-break-once --debug-goal=fib --silent
sh debug.sh tests/debugger/fixtures/fib_buggy.metta --debug-break-if='fib:arg1<0|result=0' --debug-break-skip=1 --debug-break-once --debug-goal=fib --silent
```

## Noise control

Limit trace depth:

```bash
sh debug.sh your_file.metta --debug=runtime --debug-depth=3 --silent
```

Limit event count:

```bash
sh debug.sh your_file.metta --debug=runtime --debug-max-events=40 --silent
```

Skip early breakpoint hits:

```bash
sh debug.sh your_file.metta --debug-break-if='yourFunc:arg1<0|result=0' --debug-break-skip=2 --silent
```

Save a trace to a file:

```bash
sh debug.sh your_file.metta --debug=runtime --debug-output=trace.log --silent
```

## Reading breakpoint output

A breakpoint hit now shows:

- the goal that triggered
- the hit number
- the reason it matched
- the matched values
- the originating MeTTa source expression (`source expr:`)
- the MeTTa `$variable` bindings by name for that expression (`var $Name = Value`)
- the current MeTTa stack
- source form index and line number through `f` or `p`
- goal head and current stack depth through `f` or `p`
- current arguments, and result on success events, through `f` or `p`
- the original MeTTa source form through `l`

Example:

```text
[BREAKPOINT ...] (fib 2) => 0 @ #compiled
hit: 3
reason: matched condition fib:arg1=2&result=0
match: arg1 = 2
match: result = 0
stack: (fib 10) → (fib 9) → (fib 8) → (fib 7) → (fib 6) → (fib 5) → (fib 4) → (fib 3) → (fib 2)
```

## Interactive breakpoint prompt

When running in a TTY, breakpoint hits accept:

- `Enter`: continue
- `l`: print the current MeTTa source form
- `s`: print stack again
- `f`: print the current frame, source location, depth, arguments, and MeTTa variable bindings
- `p`: print the current goal, source location, depth, arguments, MeTTa variable bindings, and result if available
- `i`: step into the next visible runtime event
- `n`: step over until execution returns to the same frame depth or shallower
- `o`: step out of the current frame
- `e <expr>`: evaluate a MeTTa expression in the current context and print its result(s)
- `:break <spec>`: add a breakpoint live (`<head>`, or `<head>:<condition>`)
- `:clear`: clear all breakpoints
- `:info`: list the active breakpoints
- `c`: continue without any more breakpoint pauses
- `q`: abort

The `e` command evaluates with tracing suspended (so it will not recurse into
the debugger) and binds the expression's `$variables` to the current frame's
values, so you can write things like `e (+ $N 1)` to probe the paused goal.

## Suggested first commands

Wrong result in one function:

```bash
sh debug.sh your_file.metta --debug=runtime --debug-goal=yourFunc --silent
```

Suspicious recursion:

```bash
sh debug.sh your_file.metta --debug-break-if='yourFunc:arg1<0' --debug-break-once --debug-goal=yourFunc --silent
```

Suspicious operator:

```bash
sh debug.sh your_file.metta --debug=runtime --debug-goal=- --silent
```

Space behavior:

```bash
sh debug.sh your_file.metta --debug=space --silent
```
