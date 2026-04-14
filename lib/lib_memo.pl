/* -*- Mode: Prolog; -*- */
/** <module> lib_memo - Memoization Library for MeTTa

This module provides a thread-safe, policy-driven memoization system 
for MeTTa with multiple eviction strategies (LRU and WTinyLFU).

@author MeTTa Development Team
@version 1.0
@license MIT

## MeTTa Usage

Enable memoization for a function:
```metta
!(memoize my-function)
```

Check if a function is memoized:
```metta
!(is-memoized my-function)
```

Configure cache options:
```metta
!(config-memoize (strategy wtinylfu))
!(config-memoize (strategy lru) (unique-limit 500))
!(config-memoize (strategy wtinylfu) (unique-limit 1000) (float 6))
```

Get current configuration:
```metta
!(get-memoize-config)
```

Clear all memoization:
```metta
!(clear-memoize)
```

## Internal Prolog API

The following predicates implement the memoization system. They are
invoked internally by the MeTTa runtime but can also be called
directly from Prolog.

@see 'memoize'/2
@see 'is-memoized'/2
@see 'clear-memoize'/1
@see 'get-memoize-config'/1
@see 'config-memoize'/2
*/

:- use_module(library(lists)).
:- use_module(library(solution_sequences)).

/* ============================================================================
   Core State - Dynamic Predicates
   ============================================================================ */

%! memo_enabled(+Fun:atom) is semidet.
% Tracks functions currently enabled for memoization.
% Generated dynamically; do not assert directly.
:- dynamic memo_enabled/1.

%! arity(+Fun:atom, +Arity:nonneg) is semidet.
% Tracks arities for functions being memoized.
:- dynamic arity/2.

%! metta_memo_entry(+Fun:atom, +Arity:nonneg, +Gen:integer, +AVs:term, +Results:list) is semidet.
% Stores cached results for a specific function/arguments combination.
%
% @arg Gen Generation number for invalidation tracking
% @arg AVs Quantized argument values (cache key)
% @arg Results List of cached results (supports non-deterministic predicates)
:- dynamic metta_memo_entry/5.

%! metta_memo_generation(+Fun:atom, +Arity:nonneg, +Gen:integer) is semidet.
% Current generation per (Fun, Arity) pair.
% Generation is bumped on invalidation to mark stale entries.
:- dynamic metta_memo_generation/3.

%! metta_memo_count(+Fun:atom, +Arity:nonneg, +Count:nonneg) is semidet.
% Number of cached entries per function/arity.
:- dynamic metta_memo_count/3.

%! metta_memo_head(+Fun:atom, +Arity:nonneg, +Head:integer) is semidet.
% Queue head pointer - ID of oldest entry (for LRU eviction).
:- dynamic metta_memo_head/3.

%! metta_memo_tail(+Fun:atom, +Arity:nonneg, +Tail:integer) is semidet.
% Queue tail pointer - ID of newest entry.
:- dynamic metta_memo_tail/3.

%! metta_memo_q(+Fun:atom, +Arity:nonneg, +Id:integer, +AVs:term) is semidet.
% Maps queue IDs to argument values (cache keys).
:- dynamic metta_memo_q/4.

/* ============================================================================
   Core State - Multifile Hooks (Integration)
   ============================================================================ */

% Intercept function calls and dispatch to cache when enabled.
:- multifile metta_try_dispatch_call/4.

%! metta_try_dispatch_call(+Fun:atom, +Args:list, -Out:term, -Goal:callable) is semidet.
% Hook: Intercepts function calls and dispatches to cache_call/3 when
% memoization is enabled for the function.
metta_try_dispatch_call(Fun, Args, Out, Goal) :-
    memo_enabled(Fun),
    Goal = cache_call(Fun, Args, Out).

% Invalidate cache when function definition changes.
:- multifile metta_on_function_changed/1.

%! metta_on_function_changed(+Fun:atom) is det.
% Hook: Called when a function definition changes.
% Invalidates all cached entries for the function.
metta_on_function_changed(Fun) :-
    cache_invalidate(Fun).

% Invalidate and disable when function is removed.
:- multifile metta_on_function_removed/1.

%! metta_on_function_removed(+Fun:atom) is det.
% Hook: Called when a function is removed from the system.
% Invalidates cache and disables memoization.
metta_on_function_removed(Fun) :-
    cache_invalidate(Fun),
    disable_memoization(Fun).

/* ============================================================================
   Configuration
   ============================================================================ */

%! memo_strategy(+Strategy:atom) is semidet.
% Eviction policy: =wtinylfu= or =lru=.
:- dynamic memo_strategy/1.

%! memo_unique_limit(+N:pos_integer) is semidet.
% Maximum cached entries per function.
:- dynamic memo_unique_limit/1.

%! memo_size_limit(+Bytes:pos_integer) is semidet.
% Estimated maximum bytes per entry (safety guard).
:- dynamic memo_size_limit/1.

%! memo_float_precision(+N:nonneg) is semidet.
% Decimal places for float quantization.
:- dynamic memo_float_precision/1.

% Default configuration values
memo_unique_limit(100).
memo_strategy(wtinylfu).
memo_float_precision(12).
memo_size_limit(3221225472).  % ~3GB

%! normalize_memo_strategy(+In:term, -Out:atom) is semidet.
% Normalizes various strategy name formats to canonical atoms.
%
% @arg In Input strategy name (atom or string)
% @arg Out Canonical strategy name (=wtinylfu= or =lru=)
% @throws error(domain_error(memoize_option, In), _) if unrecognised

normalize_memo_strategy(In, wtinylfu) :-
    memberchk(In, [wtinylfu, 'WTinyLFU', 'W-TinyLFU', 'wtinylfu', 'w-tinylfu']), !.
normalize_memo_strategy(In, lru) :-
    memberchk(In, [lru, 'LRU']), !.
normalize_memo_strategy(In, Out) :-
    atom(In),
    downcase_atom(In, D),
    normalize_memo_strategy(D, Out).

%! apply_memo_option(+Opt:list) is det.
% Applies a single configuration option with validation.
%
% @param Opt Option as [Key, Value] pair
% @throws error(domain_error(memoize_option, Opt), _) on invalid option

apply_memo_option([strategy, Raw]) :-
    normalize_memo_strategy(Raw, S), !,
    retractall(memo_strategy(_)),
    assertz(memo_strategy(S)).
apply_memo_option(['unique-limit', N]) :-
    integer(N), N > 0, !,
    retractall(memo_unique_limit(_)),
    assertz(memo_unique_limit(N)).
apply_memo_option(['size-limit', N]) :-
    (integer(N) ; float(N)), N > 0, !,
    retractall(memo_size_limit(_)),
    Bytes is round(N * 1073741824),  % Convert GB to bytes
    assertz(memo_size_limit(Bytes)).
apply_memo_option([float, N]) :-
    integer(N), N >= 0, !,
    retractall(memo_float_precision(_)),
    assertz(memo_float_precision(N)).
apply_memo_option(Opt) :-
    throw(error(domain_error(memoize_option, Opt), 'config-memoize/2')).

%! 'config-memoize'(+Opt1:list, +Result:atom) is det.
%! 'config-memoize'(+Opt1:list, +Opt2:list, +Result:atom) is det.
%! 'config-memoize'(+Opt1:list, +Opt2:list, +Opt3:list, +Result:atom) is det.
% Configure memoization with one or more options.
% Always unifies Result with =true=.
%
% Internal Prolog API with explicit result argument.
% Called from MeTTa as: =!(config-memoize (opt1 val1) (opt2 val2))=
%
% @param Opt1 First option as [Key, Value]
% @param Opt2 Second option as [Key, Value]
% @param Opt3 Third option as [Key, Value]
% @param Result Always unified with =true=
% @see apply_memo_option/1

'config-memoize'(Opt1, true) :-
    apply_memo_option(Opt1).
'config-memoize'(Opt1, Opt2, true) :-
    apply_memo_option(Opt1),
    apply_memo_option(Opt2).
'config-memoize'(Opt1, Opt2, Opt3, true) :-
    apply_memo_option(Opt1),
    apply_memo_option(Opt2),
    apply_memo_option(Opt3).

%! 'get-memoize-config'(-Config:list) is det.
% Retrieves current memoization configuration.
%
% Internal Prolog API with explicit result argument.
% Called from MeTTa as: =!(get-memoize-config)= (returns config list)
%
% @param Config List of configuration pairs: [[strategy, S], ['unique-limit', N], ...]

'get-memoize-config'(Config) :-
    memo_strategy(S),
    memo_unique_limit(UniqueLimit),
    memo_size_limit(SizeLimit),
    memo_float_precision(Prec),
    Config = [[strategy, S], ['unique-limit', UniqueLimit], ['size-limit', SizeLimit], [float, Prec]].

/* ============================================================================
   Core Lifecycle Management
   ============================================================================ */

%! enable_memoization(+Fun:atom) is det.
% Enables memoization for a function (idempotent).
% Does not fail if already enabled.

enable_memoization(Fun) :-
    ( memo_enabled(Fun) -> true ; assertz(memo_enabled(Fun)) ).

%! disable_memoization(+Fun:atom) is det.
% Disables memoization for a function.
% Removes from enabled list but does not clear cached entries.
% Use cache_invalidate/1 to clear entries.

disable_memoization(Fun) :-
    retractall(memo_enabled(Fun)).

%! memo_current_generation(+Fun:atom, +Arity:nonneg, -Gen:integer) is det.
% Gets current generation for a function/arity pair.
% Returns 0 if no generation has been set.

memo_current_generation(Fun, Arity, Gen) :-
    ( metta_memo_generation(Fun, Arity, Found) -> Gen = Found ; Gen = 0 ).

%! bump_metta_memo_generation(+Fun:atom, +Arity:nonneg) is det.
% Bumps generation for a function/arity pair.
% Used to atomically invalidate all cached entries.

bump_metta_memo_generation(Fun, Arity) :-
    memo_current_generation(Fun, Arity, Prev),
    Next is Prev + 1,
    retractall(metta_memo_generation(Fun, Arity, _)),
    assertz(metta_memo_generation(Fun, Arity, Next)).

%! cache_invalidate(+Fun:atom) is det.
% Invalidates all cache entries for a function across all arities.
% Uses per-function mutex for thread safety.
% Also bumps generation to mark stale entries.

cache_invalidate(Fun) :-
    findall(Arity,
        ( arity(Fun, Arity)
        ; metta_memo_generation(Fun, Arity, _)
        ; metta_memo_entry(Fun, Arity, _, _, _)
        ; metta_memo_count(Fun, Arity, _)
        ; metta_memo_head(Fun, Arity, _)
        ; metta_memo_tail(Fun, Arity, _)
        ; metta_memo_q(Fun, Arity, _, _)
        ; current_predicate(Fun/Arity)
        ),
        RawArities),
    sort(RawArities, Arities),
    ( Arities == []
    -> true
    ; forall(member(Arity, Arities),
        with_cache_fun_mutex(Fun, Arity,
            ( bump_metta_memo_generation(Fun, Arity),
              retractall(metta_memo_entry(Fun, Arity, _, _, _)),
              retractall(metta_memo_count(Fun, Arity, _)),
              retractall(metta_memo_head(Fun, Arity, _)),
              retractall(metta_memo_tail(Fun, Arity, _)),
              retractall(metta_memo_q(Fun, Arity, _, _))
            )))
    ).

%! cache_clear is det.
% Clears ALL memoization state globally.
% Removes all entries, generations, and resets CMS.

cache_clear :-
    retractall(metta_memo_entry(_, _, _, _, _)),
    retractall(metta_memo_generation(_, _, _)),
    retractall(metta_memo_count(_, _, _)),
    retractall(metta_memo_head(_, _, _)),
    retractall(metta_memo_tail(_, _, _)),
    retractall(metta_memo_q(_, _, _, _)),
    ( catch(nb_current(metta_cms, _), _, fail) -> nb_delete(metta_cms) ; true ),
    ( catch(nb_current(metta_cms_size, _), _, fail) -> nb_delete(metta_cms_size) ; true ),
    ( catch(nb_current(metta_memo_accesses, _), _, fail) -> nb_delete(metta_memo_accesses) ; true ).

%! 'clear-memoize'(+Result:atom) is det.
% Clears all memoization.
%
% Internal Prolog API with explicit result argument.
% Called from MeTTa as: =!(clear-memoize)=
% Always unifies Result with =true=.
% 
% @param Result Always unified with =true=

'clear-memoize'(true) :-
    cache_clear.

%! 'is-memoized'(+Fun:atom, -Status:atom) is det.
% Checks if a function is currently memoized.
%
% Internal Prolog API with explicit result argument.
% Called from MeTTa as: =!(is-memoized fun)= (returns true/false)
%
% @param Fun Function name
% @param Status =true= or =false=

'is-memoized'(Fun, true) :-
    memo_enabled(Fun), !.
'is-memoized'(_, false).

/* ============================================================================
   Synchronization
   ============================================================================ */

%! cache_fun_mutex_id(+Fun:atom, +Arity:nonneg, -Mutex:atom) is det.
% Generates unique mutex name for a function/arity pair.
% Pattern: metta_cache_fun_Fun_Arity

cache_fun_mutex_id(Fun, Arity, Mutex) :-
    atomic_list_concat(['metta_cache_fun_', Fun, '_', Arity], Mutex).

%! with_cache_fun_mutex(+Fun:atom, +Arity:nonneg, :Goal) is nondet.
% Executes Goal with per-function mutex held.
% Provides fine-grained locking for thread safety.
% @see with_mutex/2

with_cache_fun_mutex(Fun, Arity, Goal) :-
    cache_fun_mutex_id(Fun, Arity, Mutex),
    with_mutex(Mutex, Goal).

%! with_cms_mutex(:Goal) is nondet.
% Executes Goal with global CMS mutex held.
% Used for Count-Min Sketch operations.

with_cms_mutex(Goal) :-
    with_mutex(metta_cache_cms, Goal).

/* ============================================================================
   Frequency - Count-Min Sketch (WTinyLFU)
   ============================================================================

The Count-Min Sketch is a probabilistic data structure for estimating
access frequencies in O(1) space and time. It uses multiple hash functions
and a table of counters. This implementation uses halving for aging.
*/

%! ensure_cms is det.
% Initializes Count-Min Sketch if not present.
% Creates a vector of size min(8192, max_arity).
% Uses nb_setval for global state storage.

ensure_cms :-
    ( catch(nb_current(metta_cms, _), _, fail),
      catch(nb_current(metta_cms_size, _), _, fail)
    -> true
    ; current_prolog_flag(max_arity, MaxArity0),
      ( integer(MaxArity0), MaxArity0 > 0 -> MaxArity = MaxArity0 ; MaxArity = 1024 ),
      SketchSize is min(8192, MaxArity),
      functor(CMS, v, SketchSize),
      forall(between(1, SketchSize, I), nb_setarg(I, CMS, 0)),
      nb_setval(metta_cms, CMS),
      nb_setval(metta_cms_size, SketchSize),
      nb_setval(metta_memo_accesses, 0)
    ).

%! get_freq(+Fun:atom, +Arity:nonneg, +AVs:term, -Freq:integer) is det.
% Gets estimated access frequency from Count-Min Sketch.
% Thread-safe via with_cms_mutex/1.
% Returns 0 if CMS not initialized.

get_freq(Fun, Arity, AVs, Freq) :-
    with_cms_mutex(
        ( catch(nb_current(metta_cms, CMS), _, fail)
        -> ( catch(nb_current(metta_cms_size, SketchSize), _, fail)
            -> true
            ; functor(CMS, _, SketchSize) ),
            term_hash((Fun, Arity, AVs), HashRaw),
            Hash is (abs(HashRaw) mod SketchSize) + 1,
            arg(Hash, CMS, Val),
            ( integer(Val) -> Freq = Val ; Freq = 0 )
        ; Freq = 0 )
        ).

%! record_hit(+Fun:atom, +Arity:nonneg, +AVs:term) is det.
% Records a cache hit by incrementing frequency in CMS.
% Thread-safe via with_cms_mutex/1.

record_hit(Fun, Arity, AVs) :-
    with_cms_mutex(
        ( catch(nb_current(metta_cms, CMS), _, fail)
        -> ( catch(nb_current(metta_cms_size, SketchSize), _, fail)
            -> true
            ; functor(CMS, _, SketchSize) ),
            term_hash((Fun, Arity, AVs), HashRaw),
            Hash is (abs(HashRaw) mod SketchSize) + 1,
            arg(Hash, CMS, Val),
            ( integer(Val) -> NextVal is Val + 1 ; NextVal = 1 ),
            nb_setarg(Hash, CMS, NextVal)
        ; true )
        ).

%! record_miss(+Fun:atom, +Arity:nonneg, +AVs:term) is det.
% Records a cache miss and periodically ages frequencies.
% When accesses exceed sketch size, all frequencies are halved.
% This provides the "Window" in Window-TinyLFU.

record_miss(Fun, Arity, AVs) :-
    with_cms_mutex(
        ( ensure_cms,
          nb_getval(metta_cms_size, SketchSize),
          term_hash((Fun, Arity, AVs), HashRaw),
          Hash is (abs(HashRaw) mod SketchSize) + 1,
          nb_getval(metta_cms, CMS),
          arg(Hash, CMS, Val),
          ( integer(Val) -> NextVal is Val + 1 ; NextVal = 1 ),
          nb_setarg(Hash, CMS, NextVal),
          nb_getval(metta_memo_accesses, Acc),
          NextAcc is Acc + 1,
          nb_setval(metta_memo_accesses, NextAcc),
          ( NextAcc > SketchSize -> halve_cms ; true )
        )).

%! halve_cms is det.
% Ages all frequencies by halving.
% Called when access count exceeds sketch size.
% Implements the "Window" aging in Window-TinyLFU.

halve_cms :-
    nb_setval(metta_memo_accesses, 0),
    nb_getval(metta_cms_size, SketchSize),
    nb_getval(metta_cms, CMS),
    forall(between(1, SketchSize, I),
        ( arg(I, CMS, Val),
          ( integer(Val) -> NewVal is Val // 2 ; NewVal = 0 ),
          nb_setarg(I, CMS, NewVal)
        )).

/* ============================================================================
   Storage - Queue Management
   ============================================================================ */

%! get_memo_queue_state(+Fun:atom, +Arity:nonneg, -Count:integer, -Head:integer, -Tail:integer) is det.
% Gets current queue state for a function/arity.
% Returns 0 for all values if queue not initialized.

get_memo_queue_state(Fun, Arity, Count, Head, Tail) :-
    ( metta_memo_count(Fun, Arity, C) -> Count = C ; Count = 0 ),
    ( metta_memo_head(Fun, Arity, H) -> Head = H ; Head = 0 ),
    ( metta_memo_tail(Fun, Arity, T) -> Tail = T ; Tail = 0 ).

%! set_memo_queue_state(+Fun:atom, +Arity:nonneg, +Count:integer, +Head:integer, +Tail:integer) is det.
% Updates queue state atomically.
% Uses retractall/asserta for atomic update.

set_memo_queue_state(Fun, Arity, Count, Head, Tail) :-
    retractall(metta_memo_count(Fun, Arity, _)),
    retractall(metta_memo_head(Fun, Arity, _)),
    retractall(metta_memo_tail(Fun, Arity, _)),
    asserta(metta_memo_count(Fun, Arity, Count)),
    asserta(metta_memo_head(Fun, Arity, Head)),
    asserta(metta_memo_tail(Fun, Arity, Tail)).

/* ============================================================================
   Storage - Eviction Policies
   ============================================================================

Two eviction strategies are supported:

  * LRU (Least Recently Used): Evicts oldest entry when cache is full.
    Simple and effective for temporal locality.

  * WTinyLFU (Window TinyLFU): Evicts least frequently used entry, but
    only admits new entries if they're more frequent than the victim.
    This prevents "one-hit wonders" from polluting the cache.

*/

%! memo_store(+Fun:atom, +Arity:nonneg, +Gen:integer, +AVs:term, +CachedResults:list) is det.
% Stores a cache entry, applying the configured eviction policy.
%
% When cache is not full: simple append to queue.
% When cache is full:
%   - LRU: evict oldest entry
%   - WTinyLFU: compare frequencies, admit only if new entry
%     is more frequent than victim
%
% Thread-safe via caller (should be called within with_cache_fun_mutex/3).
% @see store_if_current_generation/5

memo_store(Fun, Arity, Gen, AVs, CachedResults) :-
    memo_unique_limit(Max),
    get_memo_queue_state(Fun, Arity, Count, Head, Tail),
    memo_strategy(Strategy),
    ( Count < Max
    -> % Cache not full: simple append
        Count1 is Count + 1,
        Tail1 is Tail + 1,
        assertz(metta_memo_q(Fun, Arity, Tail1, AVs)),
        assertz(metta_memo_entry(Fun, Arity, Gen, AVs, CachedResults)),
        set_memo_queue_state(Fun, Arity, Count1, Head, Tail1)
    ; % Cache full: apply eviction policy
        Head1 is Head + 1,
        ( retract(metta_memo_q(Fun, Arity, Head1, VictimAVs))
        -> ( Strategy == lru
            -> % LRU: Evict oldest, add new
                retractall(metta_memo_entry(Fun, Arity, _, VictimAVs, _)),
                Tail1 is Tail + 1,
                assertz(metta_memo_q(Fun, Arity, Tail1, AVs)),
                assertz(metta_memo_entry(Fun, Arity, Gen, AVs, CachedResults)),
                set_memo_queue_state(Fun, Arity, Count, Head1, Tail1)
            ; % WTinyLFU: Compare frequencies
                get_freq(Fun, Arity, VictimAVs, VictimFreq),
                get_freq(Fun, Arity, AVs, NewFreq),
                ( NewFreq >= VictimFreq
                -> % New entry is more frequent: admit it
                    retractall(metta_memo_entry(Fun, Arity, _, VictimAVs, _)),
                    Tail1 is Tail + 1,
                    assertz(metta_memo_q(Fun, Arity, Tail1, AVs)),
                    assertz(metta_memo_entry(Fun, Arity, Gen, AVs, CachedResults)),
                    set_memo_queue_state(Fun, Arity, Count, Head1, Tail1)
                ; % New entry is less frequent: reject it (keep victim)
                    _ = Gen,
                    Tail1 is Tail + 1,
                    assertz(metta_memo_q(Fun, Arity, Tail1, VictimAVs)),
                    set_memo_queue_state(Fun, Arity, Count, Head1, Tail1)
                )
            )
        ; % Queue inconsistent: recover gracefully
            Tail1 is Tail + 1,
            assertz(metta_memo_q(Fun, Arity, Tail1, AVs)),
            assertz(metta_memo_entry(Fun, Arity, Gen, AVs, CachedResults)),
            Count1 is min(Max, Count + 1),
            set_memo_queue_state(Fun, Arity, Count1, Head1, Tail1)
        )
    ).

%! store_if_current_generation(+Fun:atom, +Arity:nonneg, +ExpectedGen:integer, +AVs:term, +CachedResults:list) is det.
% Stores entry only if generation hasn't changed.
% Prevents stale writes after concurrent invalidation.
% Thread-safe via with_cache_fun_mutex/3.

store_if_current_generation(Fun, Arity, ExpectedGen, AVs, CachedResults) :-
    with_cache_fun_mutex(Fun, Arity,
        ( memo_current_generation(Fun, Arity, CurGen),
          ( CurGen =:= ExpectedGen
          -> memo_store(Fun, Arity, CurGen, AVs, CachedResults)
          ; true )
        )).

/* ============================================================================
   Runtime - Guards & Normalization
   ============================================================================ */

%! memoizable_fun(+Fun:atom, +Arity:nonneg) is semidet.
% Checks if function is eligible for memoization.
% Requirements: enabled, predicate exists, not built-in.

memoizable_fun(Fun, Arity) :-
    memo_enabled(Fun),
    current_predicate(Fun/Arity),
    length(HeadArgs, Arity),
    Head =.. [Fun | HeadArgs],
    \+ predicate_property(Head, built_in).

%! quantize_float(+V:float, -Q:float) is det.
% Quantizes float to fixed precision.
% Prevents cache misses from floating-point noise.
% Uses memo_float_precision/1 for scale.

quantize_float(V, Q) :-
    memo_float_precision(Prec),
    Scale is 10.0 ** Prec,
    Q is round(V * Scale) / Scale.

%! quantize_term(+T:term, -Q:term) is det.
% Recursively quantizes all floats in a term.
% Variables and atoms pass through unchanged.
% Handles nested compound terms via maplist.

quantize_term(T, T) :- var(T), !.
quantize_term(T, Q) :- float(T), !, quantize_float(T, Q).
quantize_term(T, T) :- atomic(T), !.
quantize_term(T, Q) :-
    T =.. [F|Args],
    maplist(quantize_term, Args, QArgs),
    Q =.. [F|QArgs].

%! args_too_complex(+AVs:term) is semidet.
% Checks if arguments exceed size limit.
% Uses term_size/2 and memo_size_limit/1.
% Prevents unbounded memory growth.

args_too_complex(AVs) :-
    memo_size_limit(Limit),
    term_size(AVs, S),
    EstimatedBytes is S * 8,
    EstimatedBytes > Limit.

%! args_worth_caching(+AVs:term) is semidet.
% Checks if arguments are worth caching.
% Succeeds if not too complex.

args_worth_caching(AVs) :-
    \+ args_too_complex(AVs).

/* ============================================================================
   Runtime - Execution Engine
   ============================================================================ */

%! memo_probe_results(+Fun:atom, +AVs:list, -ProbeResults:list) is nondet.
% Probes underlying function to get results.
% Used for non-deterministic predicates.
% Limits to 2 results to detect non-determinism.

memo_probe_results(Fun, AVs, ProbeResults) :-
    append(AVs, [Result], RawArgs),
    RawGoal =.. [Fun | RawArgs],
    findnsols(2, Result, call(RawGoal), ProbeResults).

%! cache_call(+Fun:atom, +AVs:list, -Out:term) is nondet.
% Main memoized call execution.
%
% Flow:
%   1. Check eligibility (ground args, not too complex, memoizable)
%   2. Quantize arguments (normalize floats)
%   3. Check current generation
%   4. If cache hit: record hit, return cached results
%   5. If cache miss: probe function, check non-determinism, store results
%   6. If not eligible: call function directly
%
% Supports non-deterministic predicates via findnsols/2 probing.
% Thread-safe via with_cache_fun_mutex/3.

cache_call(Fun, AVs, Out) :-
    append(AVs, [Out], GoalArgs),
    Goal =.. [Fun | GoalArgs],
    length(AVs, NArgs),
    Arity is NArgs + 1,
    ( ground(AVs),
      args_worth_caching(AVs),
      memoizable_fun(Fun, Arity)
    -> % Eligible for caching
        quantize_term(AVs, KeyAVs),
        memo_current_generation(Fun, Arity, CurGen),
        ( metta_memo_entry(Fun, Arity, CurGen, KeyAVs, CachedResults)
        -> % Cache HIT
            record_hit(Fun, Arity, KeyAVs),
            member(Out, CachedResults)
        ; % Cache MISS
            memo_probe_results(Fun, AVs, ProbeResults),
            ( ProbeResults = [_, _|_]
            -> % Non-deterministic predicate with multiple results: don't cache
                call(Goal)
            ; % Deterministic or single-result: cache it
                CachedResults = ProbeResults,
                store_if_current_generation(Fun, Arity, CurGen, KeyAVs, CachedResults),
                record_miss(Fun, Arity, KeyAVs),
                member(Out, CachedResults)
            )
        )
    ; % Not eligible: call directly
        call(Goal)
    ).

/* ============================================================================
   Public API - memoize!/2
   ============================================================================

These predicates are invoked from MeTTa using the !(...) syntax.
The internal Prolog implementation takes an extra result argument
which is unified with 'true' for success, or bound to an error term.
*/

%! 'memoize!'(+Fun:atom, +Result:term) is det.
% Enables memoization for a MeTTa function.
%
% MeTTa API: =!(memoize my-function)=
% Prolog API: 'memoize'(my_function, Result).
% 
% Validates function exists, temporarily removes and re-adds atoms
% to install memoization hooks, then enables memoization.
%
% @param Fun Function name (atom)
% @param Result Unified with =true= on success, or error term on failure
% @throws error(domain_error(function_symbol, Fun), 'memoize!/2') if Fun invalid
% @throws error(existence_error(function, Fun), 'memoize!/2') if Fun doesn't exist

'memoize'(Fun, 'Empty') :-
    % Validate function exists
    ( atom(Fun), fun(Fun)
    -> true
    ; throw(error(domain_error(function_symbol, Fun), 'memoize!/2'))
    ),
    % Find all translated clauses for this function
    findall(Term,
        (translated_from(_, Term), Term = [=, [Fun|_], _]),
        RawTerms),
    sort(RawTerms, Terms),
    % Temporarily remove atoms (will re-add with memoization hooks)
    forall(member(Term, Terms), 'remove-atom'('&self', Term, _)),
    % Enable memoization
    enable_memoization(Fun),
    % Re-add atoms (now with memoization intercept)
    forall(member(Term, Terms), 'add-atom'('&self', Term, _)).

/* ============================================================================
   END OF LIB_MEMO
   ============================================================================ */
   
