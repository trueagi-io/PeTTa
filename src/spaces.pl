%Since both normal add-atom call and function additions needs to add the S-expression:
add_sexp(Space, [Rel|Args]) :- Term =.. [Space, Rel | Args],
                               assertz(Term).

%Same but for removal:
remove_sexp(Space, [Rel|Args]) :- Term =.. [Space, Rel | Args],
                                  retractall(Term).

%Add a function atom:
'add-atom'(Space, Term, true) :- Term = [=,[FAtom|W],_], !,
                                 add_sexp(Space, Term),
                                 register_fun(FAtom),
                                 length(W, N),
                                 Arity is N + 1,
                                 assertz(arity(FAtom,Arity)),
                                 once(translate_clause(Term, Clause)),
                                 assertz(Clause, Ref),
                                 assertz(translated_from(Ref, Term)),
                                 invalidate_specializations(FAtom),
                                 invalidate_type_cache,
                                 maybe_print_compiled_clause("added function", Term, Clause).

%Add an atom to the space:
'add-atom'(Space, Term, true) :- add_sexp(Space, Term).

%%Remove a function atom:
'remove-atom'('&self', Term, Removed) :- Term = [=,[F|Args],Body], !,
                                         remove_sexp('&self', Term),
                                         catch(nb_getval(F, Prev), _, Prev = []),
                                         (   select(fun_meta(Args, Body), Prev, Rest)
                                             -> ( Rest == [] -> nb_delete(F)
                                                              ; nb_setval(F, Rest) ) ; true ),
                                         findall(Ref, translated_from(Ref, Term), Refs),
                                         forall(member(Ref, Refs), erase(Ref)),
                                         retractall(translated_from(_, Term)),
                                         invalidate_specializations(F),
                                         invalidate_type_cache,
                                         ( \+ ( current_predicate(F/A), functor(H2, F, A), clause(H2, _, _) )
                                           -> retractall(fun(F)) ; true ),
                                         ( Refs = [] -> Removed = false ; Removed = true ).

%Remove all same atoms:
'remove-atom'(Space, Term, true) :- remove_sexp(Space, Term).

%Match for conjunctive pattern
match(_, LComma, OutPattern, Result) :- LComma == [','], !,
                                        Result = OutPattern.
match(Space, [Comma|[Head|Tail]], OutPattern, Result) :- Comma == ',', !,
                                                         match_one(Space, Head, OutPattern, _),
                                                         match(Space, [','|Tail], OutPattern, Result).

% When the pattern list itself is a variable -> enumerate all atoms
match(Space, PatternVar, OutPattern, Result) :- var(PatternVar), !,
                                                'get-atoms'(Space, PatternVar),
                                                Result = OutPattern.

%Match for pattern:
match(Space, [Rel|PatArgs], OutPattern, Result) :- match_one(Space, [Rel|PatArgs], OutPattern, Result).

%Optimized single-pattern match using direct term construction:
match_one(Space, [Rel|PatArgs], OutPattern, Result) :-
    callable_term(Space, Rel, PatArgs, Term),
    catch(Term, _, fail),
    Result = OutPattern.

%Build callable term efficiently, avoiding =.. where possible:
callable_term(Space, Rel, [A1], Term) :- !, Term =.. [Space, Rel, A1].
callable_term(Space, Rel, [A1,A2], Term) :- !, Term =.. [Space, Rel, A1, A2].
callable_term(Space, Rel, [A1,A2,A3], Term) :- !, Term =.. [Space, Rel, A1, A2, A3].
callable_term(Space, Rel, [A1,A2,A3,A4], Term) :- !, Term =.. [Space, Rel, A1, A2, A3, A4].
callable_term(Space, Rel, [A1,A2,A3,A4,A5], Term) :- !, Term =.. [Space, Rel, A1, A2, A3, A4, A5].
callable_term(Space, Rel, Args, Term) :- Term =.. [Space, Rel | Args].

%Get all atoms in space, regardless of arity:
'get-atoms'(Space, Pattern) :- current_predicate(Space/Arity),
                               functor(Head, Space, Arity),
                               clause(Head, true),
                               Head =.. [Space | Pattern].
