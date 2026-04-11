:- use_module(library(lists)).
:- use_module(library(solution_sequences)).

:- dynamic metta_memo_entry/5.
:- dynamic metta_memo_generation/3.
:- dynamic memo_enabled/1.
:- dynamic memo_disabled_runtime/1.
:- dynamic arity/2.

:- dynamic metta_memo_count/3.
:- dynamic metta_memo_head/3.
:- dynamic metta_memo_tail/3.
:- dynamic metta_memo_q/4.

enable_memoization(Fun) :-
    ( memo_enabled(Fun) -> true ; assertz(memo_enabled(Fun)) ).

disable_memoization(Fun) :-
    retractall(memo_enabled(Fun)),
    retractall(memo_disabled_runtime(Fun)).

runtime_disable(Fun) :-
    ( memo_disabled_runtime(Fun) -> true
    ; assertz(memo_disabled_runtime(Fun))
    ).

memo_current_generation(Fun, Arity, Gen) :-
    ( metta_memo_generation(Fun, Arity, Found) -> Gen = Found ; Gen = 0 ).

bump_metta_memo_generation(Fun, Arity) :-
    memo_current_generation(Fun, Arity, Prev),
    Next is Prev + 1,
    retractall(metta_memo_generation(Fun, Arity, _)),
    assertz(metta_memo_generation(Fun, Arity, Next)).

cache_fun_mutex_id(Fun, Arity, Mutex) :-
    % Per Fun/Arity lock name.
    atomic_list_concat(['metta_cache_fun_', Fun, '_', Arity], Mutex).

with_cache_fun_mutex(Fun, Arity, Goal) :-
    cache_fun_mutex_id(Fun, Arity, Mutex),
    with_mutex(Mutex, Goal).

with_cms_mutex(Goal) :-
    with_mutex(metta_cache_cms, Goal).

cache_invalidate(Fun) :-
    % Discover arities from metadata and cache state.
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
    ( Arities == [] -> true
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

cache_clear :-
    retractall(metta_memo_entry(_, _, _, _, _)),
    retractall(metta_memo_generation(_, _, _)),
    retractall(memo_disabled_runtime(_)),
    retractall(metta_memo_count(_, _, _)),
    retractall(metta_memo_head(_, _, _)),
    retractall(metta_memo_tail(_, _, _)),
    retractall(metta_memo_q(_, _, _, _)),
    ( catch(nb_current(metta_cms, _), _, fail) -> nb_delete(metta_cms) ; true ),
    ( catch(nb_current(metta_cms_size, _), _, fail) -> nb_delete(metta_cms_size) ; true ),
    ( catch(nb_current(metta_memo_accesses, _), _, fail) -> nb_delete(metta_memo_accesses) ; true ).

ensure_cms :-
    ( catch(nb_current(metta_cms, _), _, fail),
      catch(nb_current(metta_cms_size, _), _, fail) -> true
    ; current_prolog_flag(max_arity, MaxArity0),
      ( integer(MaxArity0), MaxArity0 > 0 -> MaxArity = MaxArity0 ; MaxArity = 1024 ),
      SketchSize is min(8192, MaxArity),
      functor(CMS, v, SketchSize),
      forall(between(1, SketchSize, I), nb_setarg(I, CMS, 0)),
      nb_setval(metta_cms, CMS),
      nb_setval(metta_cms_size, SketchSize),
      nb_setval(metta_memo_accesses, 0)
    ).

get_freq(Fun, Arity, AVs, Freq) :-
    % Key by Fun/Arity/AVs
    with_cms_mutex(
        ( catch(nb_current(metta_cms, CMS), _, fail) ->
            ( catch(nb_current(metta_cms_size, SketchSize), _, fail)
            -> true
            ; functor(CMS, _, SketchSize) ),
            term_hash((Fun, Arity, AVs), HashRaw),
            Hash is (abs(HashRaw) mod SketchSize) + 1,
            arg(Hash, CMS, Val),
            ( integer(Val) -> Freq = Val ; Freq = 0 )
        ; Freq = 0 )).

record_hit(Fun, Arity, AVs) :-
    with_cms_mutex(
        ( catch(nb_current(metta_cms, CMS), _, fail) ->
            ( catch(nb_current(metta_cms_size, SketchSize), _, fail)
            -> true
            ; functor(CMS, _, SketchSize) ),
            term_hash((Fun, Arity, AVs), HashRaw),
            Hash is (abs(HashRaw) mod SketchSize) + 1,
            arg(Hash, CMS, Val),
            ( integer(Val) -> NextVal is Val + 1 ; NextVal = 1 ),
            nb_setarg(Hash, CMS, NextVal)
        ; true )).

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
          ( NextAcc > SketchSize -> halve_cms ; true ) )).

halve_cms :-
    nb_setval(metta_memo_accesses, 0),
    nb_getval(metta_cms_size, SketchSize),
    nb_getval(metta_cms, CMS),
    forall(between(1, SketchSize, I),
        ( arg(I, CMS, Val),
          ( integer(Val) -> NewVal is Val // 2 ; NewVal = 0 ),
          nb_setarg(I, CMS, NewVal)
        )).

:- dynamic cache_unique_threshold/1.
cache_unique_threshold(100).

get_memo_queue_state(Fun, Arity, Count, Head, Tail) :-
    ( metta_memo_count(Fun, Arity, C) -> Count = C ; Count = 0 ),
    ( metta_memo_head(Fun, Arity, H) -> Head = H ; Head = 0 ),
    ( metta_memo_tail(Fun, Arity, T) -> Tail = T ; Tail = 0 ).

set_memo_queue_state(Fun, Arity, Count, Head, Tail) :-
    retractall(metta_memo_count(Fun, Arity, _)),
    retractall(metta_memo_head(Fun, Arity, _)),
    retractall(metta_memo_tail(Fun, Arity, _)),
    asserta(metta_memo_count(Fun, Arity, Count)),
    asserta(metta_memo_head(Fun, Arity, Head)),
    asserta(metta_memo_tail(Fun, Arity, Tail)).

memo_store(Fun, Arity, Gen, AVs, CachedResults) :-
    cache_unique_threshold(Max),
    get_memo_queue_state(Fun, Arity, Count, Head, Tail),
    ( Count < Max ->
        Count1 is Count + 1,
        Tail1 is Tail + 1,
        assertz(metta_memo_q(Fun, Arity, Tail1, AVs)),
        assertz(metta_memo_entry(Fun, Arity, Gen, AVs, CachedResults)),
        set_memo_queue_state(Fun, Arity, Count1, Head, Tail1)
    ;
        % Cache full: W-TinyLFU admission & eviction
        Head1 is Head + 1,
        ( retract(metta_memo_q(Fun, Arity, Head1, VictimAVs)) ->
            get_freq(Fun, Arity, VictimAVs, VictimFreq),
            get_freq(Fun, Arity, AVs, NewFreq),
            ( NewFreq >= VictimFreq ->
                % Evict victim
                retractall(metta_memo_entry(Fun, Arity, _, VictimAVs, _)),
                % Admit new item
                Tail1 is Tail + 1,
                assertz(metta_memo_q(Fun, Arity, Tail1, AVs)),
                assertz(metta_memo_entry(Fun, Arity, Gen, AVs, CachedResults)),
                set_memo_queue_state(Fun, Arity, Count, Head1, Tail1)
            ;
                % Reject new item, give Victim a Second Chance
                _ = Gen,
                Tail1 is Tail + 1,
                assertz(metta_memo_q(Fun, Arity, Tail1, VictimAVs)),
                set_memo_queue_state(Fun, Arity, Count, Head1, Tail1)
            )
        ;   % Failsafe if queue is out of sync
            Tail1 is Tail + 1,
            assertz(metta_memo_q(Fun, Arity, Tail1, AVs)),
            assertz(metta_memo_entry(Fun, Arity, Gen, AVs, CachedResults)),
            Count1 is min(Max, Count + 1),
            set_memo_queue_state(Fun, Arity, Count1, Head1, Tail1)
        )
    ).

store_if_current_generation(Fun, Arity, ExpectedGen, AVs, CachedResults) :-
    % Re-check generation inside lock
    with_cache_fun_mutex(Fun, Arity,
        ( memo_current_generation(Fun, Arity, CurGen),
          ( CurGen =:= ExpectedGen
          -> memo_store(Fun, Arity, CurGen, AVs, CachedResults)
          ; true ) )).

memoizable_fun(Fun, Arity) :-
    memo_enabled(Fun),
    \+ memo_disabled_runtime(Fun),
    current_predicate(Fun/Arity),
    length(HeadArgs, Arity),
    Head =.. [Fun | HeadArgs],
    \+ predicate_property(Head, built_in).

memo_arg_size_limit(200).

args_contain_float(AVs) :-
    sub_term(X, AVs),
    float(X), !.

args_too_complex(AVs) :-
    memo_arg_size_limit(Limit),
    term_size(AVs, S),
    S > Limit.

args_worth_caching(AVs) :-
    \+ args_contain_float(AVs),
    \+ args_too_complex(AVs).

memo_probe_results(Fun, AVs, ProbeResults) :-
    % Probe up to 2 results (deterministic-only memo)
    append(AVs, [Result], RawArgs),
    RawGoal =.. [Fun | RawArgs],
    findnsols(2, Result, call(RawGoal), ProbeResults).

cache_call(Fun, AVs, Out) :-
    append(AVs, [Out], GoalArgs),
    Goal =.. [Fun | GoalArgs],
    length(AVs, NArgs),
    Arity is NArgs + 1,
    (   \+ memo_disabled_runtime(Fun),
        ground(AVs),
        args_worth_caching(AVs),
        memoizable_fun(Fun, Arity)
    ->  memo_current_generation(Fun, Arity, CurGen),
        ( metta_memo_entry(Fun, Arity, CurGen, AVs, CachedResults)
        ->  % O(1) hit
            record_hit(Fun, Arity, AVs),
            member(Out, CachedResults)
        ;   % Cache miss
            memo_probe_results(Fun, AVs, ProbeResults),
            ( ProbeResults = [_, _|_]
            % Non-deterministic bypass memo
            -> call(Goal)
            ; CachedResults = ProbeResults,
              store_if_current_generation(Fun, Arity, CurGen, AVs, CachedResults),
              record_miss(Fun, Arity, AVs),
              member(Out, CachedResults)
            )
        )
    ;   % Fallback
        call(Goal)
    ).
