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
'add-atom'(Space, Term, true) :- equation_name(Term, _), !,
                                 install_definition(Space, Term, runtime, Clause),
                                 maybe_print_compiled_clause("added function", Term, Clause).

%Add a declaration: the code translated with its name's declarations is translated again:
'add-atom'(Space, Term, true) :- Space == '&self', declaration_name(Term, F), !,
                                 with_mutex(petta_definitions, transaction((
                                 add_sexp(Space, Term), program_changed([F]) ))).

%Add an atom to the space:
'add-atom'(Space, Term, true) :- add_sexp(Space, Term).

%%A removal that could match equation rows with an unknown function name must not erase program rows:
'remove-atom'(_, Term, _) :- equation_removal_unknown_head(Term, Bad), !, instantiation_error(Bad).

%%Remove a function atom:
'remove-atom'(Space, Term, Removed) :- equation_name(Term, F), !,
                                       with_mutex(petta_definitions, transaction((
                                       matching_equations(Space, Term, F, Refs),
                                       remove_sexp(Space, Term),
                                       ( Refs == [] -> Removed = false
                                       ; removed_equations(F, Refs), Removed = true ) ))).

%Remove the rows of a three-element pattern headed by a variable or the colon.
%The equations and declarations among them leave the program:
'remove-atom'(Space, Term, true) :- is_list(Term), Term = [Tag, Head, Body], ( var(Tag) ; Tag == (:) ), !,
                                    with_mutex(petta_definitions, transaction((
                                    ( Space == '&self' -> findall(F, removed_declaration(Term, F), Fs) ; Fs = [] ),
                                    ( var(Tag), nonvar(Head), Head = [G|_], atom(G)
                                      -> matching_equations(Space, [=, Head, Body], G, Refs)
                                    ; Refs = [] ),
                                    remove_sexp(Space, Term),
                                    ( Refs == [] -> true ; removed_equations(G, Refs) ),
                                    program_changed(Fs) ))).

%Remove all same atoms:
'remove-atom'(Space, Term, true) :- remove_sexp(Space, Term).

matching_equations(Space, Equation, F, Refs) :-
    ( copy_term(Equation, Pattern), Probe =.. [Space|Pattern],
      clause(Probe, true, RowRef),
      clause(Stored, true, RowRef), Stored =.. [Space|Row],
      equation_name(Row, StoredF), StoredF == F
      -> findall(Ref-A, ( translated_from(Ref, Equation), equation_arity(F, A, Ref) ), Refs)
    ; Refs = [] ).

%An equation is a proper list (= (F ...) Body) and a declaration a proper list
%(: F Type), each with a symbol F. The spine is checked before the fields, so an
%open tail is never bound:
equation_name(Term, F) :- nonvar(Term), Term = [Eq|_], Eq == (=), is_list(Term),
                          Term = [_, Head, _], nonvar(Head), Head = [F|_], atom(F).
declaration_name(Term, F) :- nonvar(Term), Term = [Colon|_], Colon == (:), is_list(Term),
                             Term = [_, F, _], atom(F).

equation_removal_unknown_head(Term, Bad) :-
    is_list(Term), Term = [Eq, Head, _],
    ( Eq == (=) ; var(Eq) ),
    ( var(Head) -> Bad = Head
    ; nonvar(Head), Head = [F|_], var(F) -> Bad = F ).

%A declaration is a stored row (: Target Type) with the colon and a nonvariable target;
%a row with a variable in either place is data. The row is read by reference, so the
%lookup's unification cannot make a variable look like the colon or a target:
declared_row(Target, Type) :-
    current_predicate('&self'/3),
    copy_term_nat(Target-Type, IndexTarget-IndexType),
    clause('&self'(':', IndexTarget, IndexType), true, Ref),
    literal_declaration(Ref, StoredTarget, StoredType),
    Target-Type = StoredTarget-StoredType.

literal_declaration(Ref, Target, Type) :-
    clause('&self'(Colon, Target, Type), true, Ref), Colon == (:), nonvar(Target).

%The declarations a pattern removes, read by reference so its variables stay unbound:
removed_declaration(Pattern, F) :- current_predicate('&self'/3),
                                   copy_term(Pattern, [Tag, Name, Type]),
                                   clause('&self'(Tag, Name, Type), true, Ref),
                                   literal_declaration(Ref, F, _), atom(F).

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
