maybe_enable_silent(true) :- !.
maybe_enable_silent(false) :- assertz(silent(true)).

run_metta_helper(Verbose, Predicate, Arg, ResultsR) :-
    maybe_enable_silent(Verbose),
    call(Predicate, Arg, Results),
    maplist(swrite,Results,ResultsR).
