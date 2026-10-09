%Since both normal add-attom call and function additions needs to add the S-expression:
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
                                 metta_on_function_changed(FAtom),
                                 invalidate_specializations(FAtom),
                                 maybe_print_compiled_clause("added function", Term, Clause).

%Add an arrow declaration, which changes how calls to its function are translated:
'add-atom'(Space, Term, true) :- is_arrow_declaration(Space, Term, Fun), !,
                                 revise_arrow_declarations([Fun], add_sexp(Space, Term)).

%Add an atom to the space:
'add-atom'(Space, Term, true) :- add_sexp(Space, Term).

%%Remove a function atom:
'remove-atom'(Space, Term, Removed) :- Term = [=,[F|Args],Body], !,
                                       remove_sexp(Space, Term),
                                       ( retract(function_metadata(F, fun_meta(Args, Body))) -> true ; true ),
                                       findall(Ref, translated_from(Ref, Term), Refs),
                                       forall(member(Ref, Refs), erase(Ref)),
                                       retractall(translated_from(_, Term)),
                                       metta_on_function_changed(F),
                                       invalidate_specializations(F),
                                       ( \+ ( current_predicate(F/A), functor(H2, F, A), clause(H2, _, _) )
                                         -> retractall(fun(F)), metta_on_function_removed(F)
                                         ; true ),
                                       ( Refs = [] -> Removed = false ; Removed = true ).

%Remove the type atoms matching a pattern, which may be arrow declarations of several functions:
'remove-atom'(Space, Term, true) :- Space == '&self', nonvar(Term), Term = [Colon, _, _], Colon == ':', !,
                                    findall(Fun, ( copy_term(Term, [_, Fun, TypeChain]),
                                                   catch('&self'(:, Fun, TypeChain), _, fail),
                                                   is_arrow_declaration(Space, [':', Fun, TypeChain], Fun) ), Funs0),
                                    sort(Funs0, Funs),
                                    ( Funs == [] -> remove_sexp(Space, Term)
                                                  ; revise_arrow_declarations(Funs, remove_sexp(Space, Term)) ).

%Remove all same atoms:
'remove-atom'(Space, Term, true) :- remove_sexp(Space, Term).

%Recognize (: Fun (-> ...)) in the space whose declarations the translator reads:
is_arrow_declaration(Space, Term, Fun) :- Space == '&self',
                                          nonvar(Term), Term = [Colon, Fun, TypeChain], Colon == ':',
                                          atom(Fun),
                                          nonvar(TypeChain), TypeChain = [Arrow|_], Arrow == '->'.

%Match for conjunctive pattern
match(_, LComma, OutPattern, Result) :- LComma == [','], !,
                                        Result = OutPattern.
match(Space, [Comma|[Head|Tail]], OutPattern, Result) :- Comma == ',', !,
                                                         append([Space], Head, List),
                                                         Term =.. List,
                                                         catch(Term, _, fail),
                                                         \+ cyclic_term(OutPattern),
                                                         match(Space, [','|Tail], OutPattern, Result).

% When the pattern list itself is a variable -> enumerate all atoms
match(Space, PatternVar, OutPattern, Result) :- var(PatternVar), !,
                                                'get-atoms'(Space, PatternVar),
                                                \+ cyclic_term(OutPattern),
                                                Result = OutPattern.

%Match for pattern:
match(Space, [Rel|PatArgs], OutPattern, Result) :- Term =.. [Space, Rel | PatArgs],
                                                   catch(Term, _, fail),
                                                   \+ cyclic_term(OutPattern),
                                                   Result = OutPattern.

%Get all atoms in space, irregard of arity:
'get-atoms'(Space, Pattern) :- current_predicate(Space/Arity),
                               functor(Head, Space, Arity),
                               clause(Head, true),
                               Head =.. [Space | Pattern].
