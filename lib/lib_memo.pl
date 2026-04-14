:- ensure_loaded('../src/cache.pl').

:- multifile memo_call/3.
:- multifile memo_enabled_fun/1.
:- multifile memo_invalidate/1.
:- multifile memo_disable/1.

memo_call(Fun, Args, Out) :-
    cache_call(Fun, Args, Out).

memo_enabled_fun(Fun) :-
    memo_enabled(Fun).

memo_invalidate(Fun) :-
    cache_invalidate(Fun).

memo_disable(Fun) :-
    disable_memoization(Fun).

'memoize!'(Fun, 'Empty') :-
    ( atom(Fun), fun(Fun)
    -> true
    ; throw(error(domain_error(function_symbol, Fun), 'memoize!/2'))
    ),
    findall(Term, (translated_from(_, Term), Term = [=, [Fun|_], _]), RawTerms),
    sort(RawTerms, Terms),
    forall(member(Term, Terms), 'remove-atom'('&self', Term, _)),
    enable_memoization(Fun),
    forall(member(Term, Terms), 'add-atom'('&self', Term, _)).
