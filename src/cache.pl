:- use_module(library(lists)).

:- dynamic metta_memo_entry/6.
:- dynamic metta_memo_generation/3.
:- dynamic metta_call_freq/4.

:- dynamic memo_enabled/1.

enable_memoization(Fun) :-
    ( memo_enabled(Fun) -> true ; assertz(memo_enabled(Fun)) ).

disable_memoization(Fun) :-
    retractall(memo_enabled(Fun)),
    retractall(memo_disabled_runtime(Fun)).

:- dynamic memo_disabled_runtime/1.

runtime_disable(Fun) :-
    ( memo_disabled_runtime(Fun) -> true
    ; assertz(memo_disabled_runtime(Fun))
    ).


ensure_metta_memo_counters :-
    ( catch(nb_current(metta_memo_seq, _), _, fail) -> true ; nb_setval(metta_memo_seq, 0) ),
    ( catch(nb_current(metta_memo_size, _), _, fail) -> true ; nb_setval(metta_memo_size, 0) ),
    ( catch(nb_current(metta_memo_oldest, _), _, fail) -> true ; nb_setval(metta_memo_oldest, 1) ).

memo_next_seq(Seq) :-
    ensure_metta_memo_counters,
    nb_getval(metta_memo_seq, Prev),
    Seq is Prev + 1,
    nb_setval(metta_memo_seq, Seq).

memo_size_inc :-
    ensure_metta_memo_counters,
    nb_getval(metta_memo_size, Prev),
    Next is Prev + 1,
    nb_setval(metta_memo_size, Next).

memo_size_dec :-
    ensure_metta_memo_counters,
    nb_getval(metta_memo_size, Prev),
    Next is max(0, Prev - 1),
    nb_setval(metta_memo_size, Next).

memo_current_generation(Fun, Arity, Gen) :-
    ( metta_memo_generation(Fun, Arity, Found) -> Gen = Found ; Gen = 0 ).

bump_metta_memo_generation(Fun, Arity) :-
    memo_current_generation(Fun, Arity, Prev),
    Next is Prev + 1,
    retractall(metta_memo_generation(Fun, Arity, _)),
    assertz(metta_memo_generation(Fun, Arity, Next)).


cache_invalidate(Fun) :-
    findall(Arity, arity(Fun, Arity), RawArities),
    sort(RawArities, Arities),
    ( Arities == [] -> true
    ; forall(member(Arity, Arities),
        ( bump_metta_memo_generation(Fun, Arity),
          retractall(metta_call_freq(Fun, Arity, _, _))
        ))
    ).

cache_clear :-
    retractall(metta_memo_entry(_, _, _, _, _, _)),
    retractall(metta_memo_generation(_, _, _)),
    retractall(metta_call_freq(_, _, _, _)),
    retractall(memo_disabled_runtime(_)),
    nb_setval(metta_memo_seq, 0),
    nb_setval(metta_memo_size, 0),
    nb_setval(metta_memo_oldest, 1).

memo_evict_one_oldest :-
    ensure_metta_memo_counters,
    nb_getval(metta_memo_oldest, Oldest),
    ( retract(metta_memo_entry(_, _, _, _, Oldest, _))
    -> memo_size_dec
    ; true
    ),
    NextOldest is Oldest + 1,
    nb_setval(metta_memo_oldest, NextOldest).

memo_evict_if_needed :-
    metta_memo_limit(Limit),
    ensure_metta_memo_counters,
    nb_getval(metta_memo_size, Size),
    ( Size =< Limit -> true
    ; memo_evict_one_oldest,
      memo_evict_if_needed
    ).

metta_memo_limit(10000).

memo_store(Fun, Arity, Gen, AVs, CachedResults) :-
    memo_next_seq(Seq),
    assertz(metta_memo_entry(Fun, Arity, Gen, AVs, Seq, CachedResults)),
    memo_size_inc,
    memo_evict_if_needed.

memoizable_fun(Fun, Arity) :-
    memo_enabled(Fun),
    \+ memo_disabled_runtime(Fun),          % permanently ruled out at runtime
    arity(Fun, Arity),
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


:- dynamic cache_unique_threshold/1.
cache_unique_threshold(100).

should_cache(Fun, Arity, AVs) :-
    metta_memo_entry(Fun, Arity, _, AVs, _, _), !.

should_cache(Fun, Arity, _AVs) :-
    aggregate_all(count, metta_memo_entry(Fun, Arity, _, _, _, _), Count),
    cache_unique_threshold(Max),
    Count < Max.


cache_call(Fun, AVs, Out) :-
    append(AVs, [Out], GoalArgs),
    Goal =.. [Fun | GoalArgs],
    length(AVs, NArgs),
    Arity is NArgs + 1,
        \+ memo_disabled_runtime(Fun),
        ground(AVs),
        args_worth_caching(AVs),            % rejects floats and oversized terms
        memoizable_fun(Fun, Arity),
        should_cache(Fun, Arity, AVs)       % threshold checked BEFORE findall
    ->  memo_current_generation(Fun, Arity, Gen),
        ( metta_memo_entry(Fun, Arity, Gen, AVs, _, CachedResults)
        ->  member(Out, CachedResults)
        ;   findall(Result,
                ( append(AVs, [Result], RawArgs),
                  RawGoal =.. [Fun | RawArgs],
                  call(RawGoal) ),
                RawResults),
            list_to_set(RawResults, CachedResults),
            memo_store(Fun, Arity, Gen, AVs, CachedResults),
            member(Out, CachedResults)
        )
        ( memo_enabled(Fun),
          \+ memo_disabled_runtime(Fun),
          ground(AVs),
          \+ args_worth_caching(AVs)
        ->  runtime_disable(Fun)
        ;   true
        ),
        call(Goal)
    ).
