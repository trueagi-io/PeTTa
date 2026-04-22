goals_list_to_conj([], true) :- !.
goals_list_to_conj([G], G) :- !.
goals_list_to_conj([G|Gs], (G,R)) :- goals_list_to_conj(Gs, R).

memberchk_eq(V, [H|_]) :- V == H, !.
memberchk_eq(V, [_|T]) :- memberchk_eq(V, T).

maybe_print_compiled_clause(_, _, _) :- silent(true), !.
maybe_print_compiled_clause(Label, FormTerm, Clause) :-
    swrite(FormTerm, FormStr),
    format("\e[33m-->  ~w  -->~n\e[36m~w~n\e[33m--> prolog clause -->~n\e[32m", [Label, FormStr]),
    portray_clause(current_output, Clause),
    format("\e[33m^^^^^^^^^^^^^^^^^^^^^~n\e[0m").

%Register a function and assert its arity (used across spaces.pl, filereader.pl, translator.pl, specializer.pl):
register_fun_arity(F, Args) :-
    register_fun(F),
    length(Args, N),
    Arity is N + 1,
    assertz(arity(F, Arity)).

%Assert a clause with tracking its source term (used across spaces.pl, filereader.pl, specializer.pl):
%Returns Ref for further use (e.g., printing).
assert_clause_with_tracking(Clause, Source) :-
    assertz(Clause, Ref),
    assertz(translated_from(Ref, Source)).

%Version that also returns Ref explicitly (for callers that need it):
assert_clause_with_tracking(Clause, Source, Ref) :-
    assertz(Clause, Ref),
    assertz(translated_from(Ref, Source)).