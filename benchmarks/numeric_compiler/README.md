# Numeric compiler before/after

This records the complete compiler stack against current upstream main. The
MeTTa application and matched native Prolog algorithms stay fixed.

- Before: `ee5cacb6a6ecdd554a1c773aaf47e9a0f8a5af0f` (upstream main).
- After executable compiler: `c27e536364f7a6d6147c13abcda67169a76bf499`.
- MeTTa numeric application: PeTTaChainer `af4d6cca`.
- Companion harness: PeTTaChainer `ceeb991e`.
- SWI-Prolog 9.3.33, x86-64 Linux; native arithmetic enabled on both sides.

Later PR changes to this directory and protected-import test initialization do
not change the measured production compiler. The JSON runtime fingerprints
include the test source as it existed at the measured commit.

## Workload and controls

The captured DTV particle workload has 67 calls at 32 particles: 36 STV draws,
11 modus-ponens calls, four each of Bayes, complement and reweigh, and two each
of merge, merge-weight, restated and STV summary. It calls the modular public
particle API; the application's fused pipeline is not used. These are particle
measurements, not end-to-end reasoning-system timings.

Both implementations receive identical concrete inputs and use compact native
`p(X1,...,XP)` arrays. Native reference commit `4d39f8bf` is kept immutable; a
separate matched control receives the same known application improvements:
bounded permutation caching, hoisted CF products, direct merge accumulators
and reused complement logs. This separates compiler improvement from algorithm
changes made only on one side.

Cold traces clear drawing caches before each complete replay. Warm traces
reuse caches. Isolated API groups retain the captured arguments and call
counts. Groups change locality and outer traversal; their timings must not be
added to explain the interleaved trace.

Each compiler times MeTTa and its adjacent Prolog control in the same SWI
process. Each case gets ten warmups and seven CPU-time pairs, alternating
execution order. Batches replay 1,000 full traces or 2,000 API groups. Fresh
goal/output copying and GC during a batch are included. Explicit GC occurs
before each timed batch. Parsing, startup, compilation, Python/Janus and
printing are outside timing. Profiles are collected separately from timings.

## Results

Ratios are MeTTa CPU / matched direct-Prolog CPU. The reduction is
`100 * (1 - after_ratio / before_ratio)`; it compares compilers after
normalizing each to its adjacent native control. These are separate compiler
processes, one seven-pair series per case and compiler, without confidence
intervals. A negative reduction denotes an adverse result.

| Case | Before / Prolog | After / Prolog | Normalized CPU reduction |
|---|---:|---:|---:|
| Complete cold trace | 2.337x | 1.158x | 50.4% |
| Complete warm trace | 2.304x | 1.208x | 47.6% |
| STV draw group | 3.367x | 1.073x | 68.1% |
| Modus ponens group | 1.996x | 1.166x | 41.6% |
| Bayes group | 1.540x | 1.093x | 29.0% |
| Merge group | 2.631x | 1.470x | 44.1% |
| Merge-weight group | 2.079x | 1.258x | 39.5% |
| Restated group | 2.176x | 1.247x | 42.7% |
| Reweigh group | 1.756x | 0.894x | 49.1% |
| STV summary group | 1.481x | 0.964x | 34.9% |
| Complement group | 1.387x | 1.485x | -7.1% |

The tiny complement group worsened in this series and is retained; the result
is not a claim that every operation improves. Most of the workload improves,
and the complete traces remain slower than the matched direct-Prolog control.

Both compilers pass 6,973 numerical comparisons. Maximum absolute difference
against the native controls is `1.4210854715202004e-14`. Application and harness
source fingerprints match across runs; runtime fingerprints were checked
against their respective measured checkouts. All raw batches, profiling data
and fingerprints are in [before.json.gz](before.json.gz) and
[after.json.gz](after.json.gz). [comparison.json](comparison.json) retains
unrounded ratios and commit identities.

## Reproduction

In the companion PeTTaChainer checkout at the harness commit, create PeTTa
worktrees for the before/after commits, then run each sequentially:

```bash
unset DISPLAY
ulimit -v 16000000
/nexus/Dev/OpenCog/bench/capped --slots 1 env \
  PETTA_PATH=/absolute/path/to/petta-worktree \
  .venv/bin/python experiments/compact_particles/run.py \
  --compiler-cost --native-arithmetic --trace-attribution \
  --output scratch/compiler-comparison/variant
```

The runner invokes the companion repository's pinned `.venv/bin/petta` with
`--strict-det`. The runner, saved trace, MeTTa loops, primitive bridge and
matched controls are under `experiments/compact_particles/`; shared Beta
kernels are under `experiments/beta_drawing/`. These sources remain in the
companion project so this PR adds compiler changes and evidence only.
