:- dynamic space_primitive/2.

%Since both normal add-attom call and function additions needs to add the S-expression:
%Nonempty expressions keep their indexed rows; other atoms are stored separately, so
%no expression value is reserved as a marker. An unbound atom raises an instantiation error.
add_sexp(Space, TermIn) :-
    ( var(TermIn) ->
        instantiation_error(TermIn)
    ; is_list(TermIn), TermIn = [Rel|Args] ->
        Term =.. [Space, Rel | Args],
        assertz(Term),
        maybe_cache_type_decl(Space, [Rel|Args])
    ;
        must_be(atom, Space),
        assertz(space_primitive(Space, TermIn))
    ).

%Same but for removal:
remove_sexp(Space, TermIn) :-
    ( var(TermIn) ->
        instantiation_error(TermIn)
    ; is_list(TermIn), TermIn = [Rel|Args] ->
        Term =.. [Space, Rel | Args],
        retractall(Term),
        maybe_uncache_type_decl(Space, [Rel|Args])
    ;
        must_be(atom, Space),
        retractall(space_primitive(Space, TermIn))
    ).

%%An unbound atom would match every row, so it is not added:
'add-atom'(_, Term, _) :- var(Term), !, instantiation_error(Term).

%Publish a function and all refreshed dependents together, or roll back.
'add-atom'(Space, Term, true) :- Term = [=,[FAtom|W],_], atom(FAtom), !,
    with_mutex(petta_definitions,
        with_definition_update(runtime_add_function_tx(Space, Term, FAtom, W))).

%Add an atom to the space:
'add-atom'(Space, Term, true) :-
    typed_space_runtime_value_ok(Space, Term),
    add_sexp(Space, Term).

runtime_add_function_tx(Space, Term, FAtom, W) :-
    add_sexp(Space, Term),
    length(W, N), Arity is N + 1,
    register_signature(runtime, FAtom, Arity),
    unified_checker_invalidate_event(clause_changed(FAtom/N, runtime_preparing)),
    once(compile_equation(Term, Clause, true, Dependencies, Uses)),
    assert_equation(Term, Clause, Uses, ClauseRef),
    record_compiled_dependencies(ClauseRef, FAtom/N, Dependencies),
    once(retract(pending_signature(FAtom, Arity, runtime))),
    sync_function_arities(FAtom),
    notify_mutation(clause_changed(FAtom/N, runtime)),
    refresh_users(FAtom, []),
    forall(metta_on_function_changed(FAtom), true),
    invalidate_specializations(FAtom),
    maybe_print_compiled_clause("added function", Term, Clause).

%%A removal with an unknown function name must not erase program rows:
'remove-atom'(_, Term, _) :- Term = [=,[F|_],_], var(F), !, instantiation_error(F).

%%Remove a function atom:
'remove-atom'(Space, Term, Removed) :-
    Term = [=,[F|Args],Body], atom(F), !,
    with_mutex(petta_definitions, with_definition_update(
        ( remove_sexp(Space, Term),
          remember_function_metas(F),
          ( atom(F), nb_current(F, Prev) -> true ; Prev = [] ),
          (   select(Meta, Prev, Rest),
              Meta = fun_meta(Args0, Body0, _),
              Args0 =@= Args,
              Body0 =@= Body
              -> ( Rest == [] -> nb_delete(F)
                               ; nb_setval(F, Rest) ) ; true ),
          findall(Ref-A,
                  ( translated_from(Ref, Term),
                    ( equation_arity(F, A, Ref) -> true
                    ; clause(H, _, Ref), functor(H, F, A) ) ),
                  Refs),
          forall(member(Ref-A, Refs),
                 ( forget_compiled_dependencies(Ref),
                   forget_translation(Ref),
                   ignore(catch(erase(Ref), _, fail)),
                   no_equations_answer(F, A) )),
          sync_function_arities(F),
          invalidate_specializations(F),
          length(Args, N),
          ( \+ ( equation_arity(F, RemA, _), RemA is N + 1 )
            -> retractall(det_bound_proviso(F, N, _, _)),
               forall(( inferred_fn_type(F, InfATs, InfOT), length(InfATs, N) ),
                      retractall(inferred_fn_type(F, InfATs, InfOT)))
          ; true ),
          % Callable classification is part of the unified call contract.
          % sync_function_arities/1 drops the last fun/1 marker before
          % rebuilding callers so they see data syntax, not a stale call.
          notify_mutation(clause_changed(F/N, runtime)),
          refresh_users(F, []),
          forall(metta_on_function_changed(F), true),
          ( fun(F)
            -> true
            ; forall(metta_on_function_removed(F), true) ),
          ( Refs = [] -> Removed = false ; Removed = true ) ))).

%Remove all same atoms:
'remove-atom'(Space, Term, true) :-
    typed_space_runtime_value_ok(Space, Term),
    remove_sexp(Space, Term).

%Updates whose row the translator proved against the space's declared schema
%(typed_space_update_goal/4). A function atom keeps its own clause.
'add-atom-proven'(Space, Term, true) :- ( var(Term) -> true ; Term = [=,_,_] ), !,
                                        'add-atom'(Space, Term, true).
'add-atom-proven'(Space, Term, true) :- add_sexp(Space, Term).

'remove-atom-proven'(Space, Term, Removed) :- Term = [=,_,_], !,
                                              'remove-atom'(Space, Term, Removed).
'remove-atom-proven'(Space, Term, true) :- remove_sexp(Space, Term).

%Typed spaces accept open payloads and removal patterns.  At the operation
%boundary reject only a value that has become a definite contradiction; this
%is the runtime counterpart of translator.pl's check_typed_space_value/2, not
%a residual type guard, and it never constrains unresolved fields.
typed_space_runtime_value_ok(Space, Value) :-
    ( atom(Space), declared_space_type(Space, RowT),
      value_definitely_mismatch(Value, RowT)
      -> throw(error(literal_type_mismatch(Value, RowT), typecheck))
    ; true ).

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
                                                         ; is_list(Head), Head = [_|_] -> append([Space], Head, List), Term =.. List,
                                                                                          space_call(Term)
                                                         ; must_be(atom, Space), space_primitive(Space, Head) ),
                                                         \+ cyclic_term(OutPattern),
                                                         match(Space, [','|Tail], OutPattern, Result).

% When the pattern list itself is a variable -> enumerate all atoms
match(Space, PatternVar, OutPattern, Result) :- var(PatternVar), !,
                                                'get-atoms'(Space, PatternVar),
                                                \+ cyclic_term(OutPattern),
                                                Result = OutPattern.

%Match for pattern:
match(Space, Pattern, OutPattern, Result) :-
    ( is_list(Pattern), Pattern = [Rel|PatArgs] ->
        Term =.. [Space, Rel | PatArgs],
        space_call(Term)
    ;
        must_be(atom, Space),
        space_primitive(Space, Pattern)
    ),
    \+ cyclic_term(OutPattern),
    Result = OutPattern.

%Get all atoms in space, irregard of arity:
'get-atoms'(Space, Pattern) :- space_primitive(Space, Pattern).
'get-atoms'(Space, Pattern) :- current_predicate(Space/Arity),
                               functor(Head, Space, Arity),
                               clause(Head, true),
                               Head =.. [Space | Pattern].
