:- dynamic space_primitive/2.

%Nonempty expressions keep their indexed rows; other atoms are stored separately.
%An unbound atom still raises an instantiation error:
add_sexp(Space, Atom) :- ( var(Atom) -> instantiation_error(Atom)
                        ; Atom = [Rel|Args] -> Term =.. [Space, Rel | Args], assertz(Term)
                        ; must_be(atom, Space), assertz(space_primitive(Space, Atom)) ).

%Same but for removal:
remove_sexp(Space, Atom) :- ( var(Atom) -> instantiation_error(Atom)
                           ; Atom = [Rel|Args] -> Term =.. [Space, Rel | Args], retractall(Term)
                           ; must_be(atom, Space), retractall(space_primitive(Space, Atom)) ).

%Add a function atom:
'add-atom'(Space, Term, true) :- Term = [=,[FAtom|_],_], atom(FAtom), !,
                                 install_definition(Space, Term, runtime, Clause),
                                 maybe_print_compiled_clause("added function", Term, Clause).

%Add an atom to the space:
'add-atom'(Space, Term, true) :- add_sexp(Space, Term).

%%A removal with an unknown function name must not erase program rows:
'remove-atom'(_, Term, _) :- Term = [=,[F|_],_], var(F), !, instantiation_error(F).

%%Remove a function atom:
'remove-atom'(Space, Term, Removed) :- Term = [=,[F|_],_], atom(F), !,
                                       with_mutex(petta_definitions, transaction((
                                       remove_sexp(Space, Term),
                                       findall(Ref-A, ( translated_from(Ref, Term), clause(H, _, Ref), functor(H, F, A) ), Refs),
                                       ( Refs = [] -> Removed = false
                                       ; removed_equations(F, Refs), Removed = true ) ))).

%Remove all same atoms:
'remove-atom'(Space, Term, true) :- remove_sexp(Space, Term).

% A space is a predicate per row arity, created by the first add-atom. Reading
% a space before it is written calls an undefined predicate, and every such
% call would search the autoloader before failing. The first call declares the
% space's predicate dynamic instead, so later reads are plain calls on an empty
% predicate; reads of existing spaces pay nothing extra.
space_call(Term) :- catch(Term, E, space_missing(E, Term)).

space_missing(error(existence_error(procedure, Space/Arity), _), Term) :-
    functor(Term, Space, Arity), !,
    dynamic(Space/Arity),
    fail.

%Match for conjunctive pattern
match(_, LComma, OutPattern, Result) :- LComma == [','], !,
                                        Result = OutPattern.
match(Space, [Comma|[Head|Tail]], OutPattern, Result) :- Comma == ',', !,
                                                         ( var(Head) -> 'get-atoms'(Space, Head)
                                                         ; Head = [_|_] -> Term =.. [Space|Head], space_call(Term)
                                                         ; must_be(atom, Space), space_primitive(Space, Head) ),
                                                         \+ cyclic_term(OutPattern),
                                                         match(Space, [','|Tail], OutPattern, Result).

% When the pattern list itself is a variable -> enumerate all atoms
match(Space, PatternVar, OutPattern, Result) :- var(PatternVar), !,
                                                'get-atoms'(Space, PatternVar),
                                                \+ cyclic_term(OutPattern),
                                                Result = OutPattern.

%Match for pattern:
match(Space, Pattern, OutPattern, Result) :- ( Pattern = [Rel|PatArgs]
                                             -> Term =.. [Space, Rel | PatArgs], space_call(Term)
                                              ; must_be(atom, Space), space_primitive(Space, Pattern) ),
                                             \+ cyclic_term(OutPattern),
                                             Result = OutPattern.

%Get all atoms in space, irregard of arity:
'get-atoms'(Space, Pattern) :- space_primitive(Space, Pattern).
'get-atoms'(Space, Pattern) :- current_predicate(Space/Arity),
                               functor(Head, Space, Arity),
                               clause(Head, true),
                               Head =.. [Space | Pattern].
