# lib_memo - Memoization Library for MeTTa
Thread-safe, policy-driven memoization system with multiple eviction strategies (LRU and WTinyLFU), variant-key support for non-ground calls, and multi-answer caching.
This document explains the public API, configuration options, internal behavior you should rely on, and practical recommendations for effective usage.
## Quick Start
### Enable Memoization
```metta
!(memoize fib)
!(memoize fib 1) ; only memoize fib with one input argument
```
### Check Status
```metta
!(is-memoized fib)  ; Returns: true or false
!(is-memoized fib 1) ; Returns: true if fib/1-input arity is memoized
```
### Configure Cache
```metta
; Set eviction strategy
!(config-memoize (strategy wtinylfu))
; Set max entries per function (unique entries)
!(config-memoize (unique-limit 1000))
; Set global memory limit (in GB)
!(config-memoize (size-limit 5))
; Set float precision (decimal places)
!(config-memoize (float 6))
; Combine options
!(config-memoize (strategy lru) (unique-limit 500) (size-limit 10))
```
### Get Configuration & Stats
```metta
!(get-memoize-config)
; Example return: ((strategy wtinylfu) ('unique-limit' 100) ('size-limit' 5368709120)
;                  (float 12) ('answer-limit' 2048) (aggregate none))
!(get-memoize-stats)
; Returns runtime counters as a list of [Key, Value] pairs, e.g. ((cache_miss 1001) (cache_hit 998))
```

### Clear Memoization
```metta
!(clear-memoize)            ; Clears all cached entries and resets generation/queue state
!(invalidate-memoize my-fun) ; Invalidate one function and its dependents
!(clear-memoize-stats)      ; Reset runtime counters
```
## Configuration Options
| Option | Default | Description |
|--------|---------|-------------|
| `strategy` | `wtinylfu` | Eviction policy: `wtinylfu` or `lru` |
| `unique-limit` | 100 | Maximum cached entries per function (per-function queue capacity) |
| `size-limit` | 5 | **Global** memory limit in GB (across all functions) |
| `float` | 12 | Decimal precision for float quantization (see notes) |
| `answer-limit` | 2048 | Maximum answers stored per cache key |
| `aggregate` | `none` | Ground-call aggregation mode: `none|min|max|sum|count` |

## Arity-Aware Memoization
- `!(memoize fun)` enables memoization for all arities of `fun` (backward compatible behavior).
- `!(memoize fun N)` enables memoization only for arity `N`, where `N` is the number of input arguments in MeTTa.
- Arities do not conflict: functions like `(fun $x)` and `(fun $x $y)` can be memoized independently, including in the same file.
- Status checks support the same shape: `!(is-memoized fun)` (any arity enabled) and `!(is-memoized fun N)` (specific arity).
## Eviction Policies
- LRU (Least Recently Used): simple FIFO per-function queue; evicts oldest entries when per-function capacity is reached. Good for workloads with recent locality.
- WTinyLFU (Window TinyLFU): uses a Count‑Min Sketch to estimate frequency and an admission policy that compares a candidate's frequency against the victim's frequency. Prevents "one‑hit wonders" from polluting the cache and is a good default when a small subset of keys are hot.
Choose `wtinylfu` when you expect a stable hot set; choose `lru` when recency is the dominant access pattern.
## Global Memory (`size-limit`)
- `size-limit` is a global cap across all cached entries. The code converts the GB value to bytes internally.
- Estimated entry size = (term_size(Args) + term_size(Results)) × 8 bytes (rough term-cell estimate).
- On store: if CurrentTotal + NewEntry > Limit, the runtime evicts the oldest entries across all functions until there is enough space, updating the global total accordingly.
- `size-limit` controls only the estimated cache entry memory; it does not cap the Prolog VM's other memory usage (stacks, atoms, etc.).
## Replay Semantics & Multi-Answer Caching
- Ground calls: cache keys are quantized (floats rounded to configured precision) and replay mode returns stored outputs only. Ground aggregation modes (`count|min|max|sum`) apply to the collected answers.
- Non-ground (variant) calls: the cache stores answer patterns `(Args, Out)` and replays bindings on hit; this preserves tabling-like semantics.
- Multi-answer support: probing collects up to `answer-limit` answers per key; excess answers are truncated and the `answer_limit_truncated` metric is incremented.
- In-progress guard: for variant keys, the runtime uses `metta_memo_in_progress/4` to avoid duplicated concurrent recomputation. Callers will briefly wait for in-progress work to finish and then replay results; if waiting fails they fall back to direct execution.
## Core State (short reference)
Dynamic predicates exposed in the runtime (for debugging and reasoning):
- `memo_enabled/1` — functions with memoization enabled
- `memo_enabled/2` — arity-specific memoization enables (`Fun`, InputArity)
- `metta_memo_entry/5` — cached results (Fun, Arity, Gen, Args, Results)
- `metta_memo_generation/3` — generation counter per function (used for invalidation)
- `metta_memo_count/3`, `metta_memo_head/3`, `metta_memo_tail/3`, `metta_memo_q/4` — per-function queue state
- `metta_memo_total_bytes/1` — global estimated bytes used by cache entries
- `metta_memo_in_progress/4` — keys currently being computed (variant path)
- `metta_memo_dep/4` — coarse caller→callee dependency graph (used for dependency-aware invalidation)
- `metta_memo_stat/2` — runtime counters (cache_hit, cache_miss, waited_on_in_progress, etc.)
Refer to source predicates if you need deeper internal debugging; avoid relying on internal facts for program logic unless you intend to keep compatibility with future changes.
## Integration Hooks & Synchronization
The library integrates with the MeTTa runtime via multifile hooks:
- `metta_try_dispatch_call/4` — intercepts dispatch to memoized functions
- `metta_on_function_changed/1` — triggers invalidation when a function implementation changes
- `metta_on_function_removed/1` — invalidates and disables memoization when a function is removed
Synchronization primitives:
- `with_cache_fun_mutex/3` — per-(Fun,Arity) mutex to protect queue/state for that function
- `with_cms_mutex/1` — global mutex used for the Count‑Min Sketch updates
## Practical Recommendations & Effective API Usage
These are concise, actionable guidelines derived from observed behaviors and common pitfalls.
1. Global vs per-function config
- Configuration options (strategy, unique-limit, size-limit, float, answer-limit, aggregate) are global to the running MeTTa/Prolog process. Which functions are memoized is per-function (`!(memoize <fun>)`).
- If you need different cache parameters for different examples, set the global config and clear the cache before running each example (procedural approach). See the helper snippet below.
2. Clear before experiments
Always run a reproducible preset when benchmarking:
```metta
!(clear-memoize)
!(clear-memoize-stats)
!(config-memoize (unique-limit 10000) (strategy wtinylfu) (size-limit 5))
!(<run workload>)
!(println! (get-memoize-stats))
```
3. Unique-limit guidance
- `unique-limit` is the per-function queue capacity. For divide-and-conquer or DP workloads (e.g., `fib`), set `unique-limit` ≥ number of distinct inputs you expect (for `fib(N)` that is `N+1`) to avoid cache thrashing and repeated recomputation.
4. Interpreting hits/misses
- Under a small `unique-limit`, the cache can thrash: entries are evicted and later recomputed, producing many `cache_miss` events and also many `cache_hit` events as recomputed entries are accessed. When capacity is sufficient, misses drop to ~distinct-keys and runtime improves.
5. Choosing strategy
- `wtinylfu` (default): good general-purpose choice when a small hot set exists.
- `lru`: use when temporal locality dominates and you want a simple recency-based policy.
6. Float precision
- `float` controls quantization of float arguments for canonical keys. Higher precision reduces quantization collisions but can increase cache fragmentation. Avoid extremely large values (e.g., >18) to prevent numerical issues.
7. Printing stats
- `!(get-memoize-stats)` returns counters; call it after running the workload. If your MeTTa host does not echo return values, use `!(println! (get-memoize-stats))` or add a helper `!(print-memoize-stats (println! (get-memoize-stats)))`.
8. Per-function overrides (optional)
- If you need per-function configuration without changing globals repeatedly, consider adding a small override layer (library change). A reasonable design is `memo_config_override(Fun, Option, Value)` and a helper `effective_config(Fun, Option, Value)` that prefers the per-function override over the global value. I can prepare a small patch if you want this behavior.
