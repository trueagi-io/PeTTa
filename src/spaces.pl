%Runtime function add-atom cleans up its own staged raw atom, registration,
%metadata and compiled-dependency state on failure or exception. This remains
%a bounded transaction: an embedder observing concurrent mutation can still
%see the short interval between staging and cleanup, and a multi-function
%dependency cascade is not atomically rolled back as one unit.
:- discontiguous 'add-atom'/3.
%
%Since both normal add-attom call and function additions needs to add the S-expression:
add_sexp(Space, TermIn) :-
    ( is_list(TermIn), TermIn = [Rel|Args] ->
        Term =.. [Space, Rel | Args],
        assertz(Term),
        maybe_cache_type_decl(Space, [Rel|Args])
    ;
        Term =.. [Space, '#primitive', TermIn],
        assertz(Term)
    ).

%Same but for removal:
remove_sexp(Space, TermIn) :-
    ( is_list(TermIn), TermIn = [Rel|Args] ->
        Term =.. [Space, Rel | Args],
        retractall(Term),
        maybe_uncache_type_decl(Space, [Rel|Args])
    ;
        Term =.. [Space, '#primitive', TermIn],
        retractall(Term)
    ).

%Add a function atom:
'add-atom'(Space, Term, true) :- Term = [=,[FAtom|W],_], !,
                                 length(W, SourceArity),
                                 snapshot_runtime_function_add(
                                     FAtom, SourceArity, Snapshot),
                                 catch(
                                     ( runtime_add_function_tx(
                                           Space, Term, FAtom, W, Snapshot,
                                           RawRef, ClauseRef)
                                       -> true
                                       ; cleanup_runtime_function_add(
                                           FAtom, RawRef, ClauseRef,
                                           Snapshot),
                                       fail ),
                                     Error,
                                     ( cleanup_runtime_function_add(
                                           FAtom, RawRef, ClauseRef,
                                           Snapshot),
                                       throw(Error) )),
                                 nb_setval('$runtime_add_raw_ref', none),
                                 nb_setval('$runtime_add_clause_ref', none).

runtime_add_function(Space, Term, FAtom, W, RawRef, ClauseRef) :-
    runtime_add_function_tx(
        Space, Term, FAtom, W, no_runtime_snapshot, RawRef, ClauseRef).

runtime_add_function_tx(Space, Term, FAtom, W, Snapshot, RawRef, ClauseRef) :-
    Term = [=, [FAtom|W], TermBody],
    RawTerm =.. [Space, '=', [FAtom|W], TermBody],
    assertz(RawTerm, RawRef),
    % Publish the exact staged ref before any later goal can throw.  Ordinary
    % Prolog output arguments are undone during exception unwinding; storing
    % it non-backtrackably lets the transaction cleanup erase exactly this raw
    % clause without variant-matching older atoms.
    remember_runtime_add_ref(Snapshot, raw, RawRef),
    nb_setval('$runtime_add_raw_ref', RawRef),
    maybe_cache_type_decl(Space, Term),
    register_fun(FAtom),
    length(W, N),
    Arity is N + 1,
    assertz(arity(FAtom, Arity)),
    % A runtime clause is translated before its normal mutation notification.
    % Drop any old unified self-summary now so recursive validation cannot
    % consume the pre-add contract from an enclosing batch scope.
    unified_checker_invalidate_event(
        clause_changed(FAtom/N, runtime_preparing)),
    once(translate_clause(Term, Clause, true, Dependencies)),
    assertz(Clause, ClauseRef),
    remember_runtime_add_ref(Snapshot, clause, ClauseRef),
    nb_setval('$runtime_add_clause_ref', ClauseRef),
    assertz(translated_from(ClauseRef, Term)),
    record_compiled_dependencies(ClauseRef, FAtom/N, Dependencies),
    notify_mutation(clause_changed(FAtom/N, runtime)),
    metta_on_function_changed(FAtom),
    invalidate_specializations(FAtom),
    maybe_print_compiled_clause("added function", Term, Clause).

snapshot_runtime_function_add(F, N,
        runtime_add_snapshot(N, FunFacts, Arities, Recompile,
                             CompiledClauses, CacheEntries,
                             runtime_add_refs(none, none))) :-
    findall(true, fun(F), FunFacts),
    findall(A, arity(F, A), Arities),
    snapshot_recompile_state(F, Recompile),
    snapshot_runtime_function_clauses(F, CompiledClauses),
    unified_checker_cache:unified_summary_cache_snapshot_event(
        clause_changed(F/N, runtime_preparing), CacheEntries).

cleanup_runtime_function_add(F, RawRef, ClauseRef,
        runtime_add_snapshot(N, FunFacts, Arities, Recompile,
                             CompiledClauses, CacheEntries, Refs)) :-
    runtime_add_clause_ref(ClauseRef, Refs, StagedClauseRef),
    ( nonvar(StagedClauseRef)
      -> forget_compiled_dependencies(StagedClauseRef),
         retractall(translated_from(StagedClauseRef, _)),
         ignore(catch(erase(StagedClauseRef), _, fail))
    ; true ),
    runtime_add_raw_ref(RawRef, Refs, StagedRawRef),
    ( nonvar(StagedRawRef)
      -> ignore(catch(erase(StagedRawRef), _, fail))
    ; true ),
    nb_setval('$runtime_add_raw_ref', none),
    nb_setval('$runtime_add_clause_ref', none),
    restore_runtime_function_clauses(F, CompiledClauses),
    restore_recompile_state(F, Recompile),
    retractall(fun(F)),
    forall(member(true, FunFacts), assertz(fun(F))),
    retractall(arity(F, _)),
    forall(member(A, Arities), assertz(arity(F, A))),
    % notify_mutation/1 may already have swapped the staged clause and rebuilt
    % callers before a later hook throws.  Replaying the mutation after the
    % exact pre-add source clauses are back brings that dependent cascade into
    % agreement with the restored program.  The cache snapshot is installed
    % afterwards so the original closed contracts survive the failed add.
    ignore(catch(notify_mutation(clause_changed(F/N, runtime)), _, fail)),
    % Recompile analysis may have staged post-add summaries before a later
    % operation failed.  The rollback restored the old program state, so make
    % that failure boundary explicit and conservative.
    unified_checker_cache:unified_summary_cache_restore(CacheEntries).

snapshot_runtime_function_clauses(F, Clauses) :-
    findall(runtime_compiled_clause(Source, Head, Body, Origin, Dependencies),
            ( translated_from(Ref, Source),
              runtime_source_key(Source, F/_),
              clause(Head, Body, Ref),
              compiled_dependency_origin(Ref, Origin),
              ( compiled_deps(Ref, _, _, StoredDependencies)
                -> Dependencies = StoredDependencies
              ; Dependencies = [] ) ),
            Clauses).

restore_runtime_function_clauses(F, Clauses) :-
    findall(Ref,
            ( translated_from(Ref, Source),
              runtime_source_key(Source, F/_) ),
            CurrentRefs),
    forall(member(Ref, CurrentRefs),
           ( forget_compiled_dependencies(Ref),
             retractall(translated_from(Ref, _)),
             ignore(catch(erase(Ref), _, fail)) )),
    restore_runtime_compiled_clauses(Clauses).

restore_runtime_compiled_clauses([]).
restore_runtime_compiled_clauses(
        [runtime_compiled_clause(Source, Head, Body, Origin, Dependencies)
         |Clauses]) :-
    assertz((Head :- Body), Ref),
    assertz(translated_from(Ref, Source)),
    runtime_source_key(Source, Key),
    record_compiled_dependencies(Ref, Key, Origin, Dependencies),
    restore_runtime_compiled_clauses(Clauses).

runtime_source_key([Eq, [F|Args], _], F/N) :-
    Eq == (=), atom(F), is_list(Args), length(Args, N).

remember_runtime_add_ref(
        runtime_add_snapshot(_, _, _, _, _, _, Refs), raw, Ref) :- !,
    nb_setarg(1, Refs, Ref).
remember_runtime_add_ref(
        runtime_add_snapshot(_, _, _, _, _, _, Refs), clause, Ref) :- !,
    nb_setarg(2, Refs, Ref).
remember_runtime_add_ref(_, _, _).

runtime_add_raw_ref(RawRef, _, RawRef) :- nonvar(RawRef), !.
runtime_add_raw_ref(_, runtime_add_refs(Stored, _), Stored) :-
    Stored \== none, !.
runtime_add_raw_ref(_, _, Stored) :-
    catch(nb_getval('$runtime_add_raw_ref', Value), _, fail),
    Value \== none, !,
    Stored = Value.
runtime_add_raw_ref(_, _, _).

runtime_add_clause_ref(ClauseRef, _, ClauseRef) :- nonvar(ClauseRef), !.
runtime_add_clause_ref(_, runtime_add_refs(_, Stored), Stored) :-
    Stored \== none, !.
runtime_add_clause_ref(_, _, Stored) :-
    catch(nb_getval('$runtime_add_clause_ref', Value), _, fail),
    Value \== none, !,
    Stored = Value.
runtime_add_clause_ref(_, _, _).

%Add an atom to the space:
'add-atom'(Space, Term, true) :-
    typed_space_runtime_value_ok(Space, Term),
    add_sexp(Space, Term).

%%Remove a function atom:
'remove-atom'(Space, Term, Removed) :- Term = [=,[F|Args],Body], !,
                                       remove_sexp(Space, Term),
                                       catch(nb_getval(F, Prev), _, Prev = []),
                                       (   select(Meta, Prev, Rest),
                                           fun_meta_parts(Meta, Args0, Body0, _),
                                           Args0 =@= Args,
                                           Body0 =@= Body
                                           -> ( Rest == [] -> nb_delete(F)
                                                            ; nb_setval(F, Rest) ) ; true ),
                                       findall(Ref, translated_from(Ref, Term), Refs),
                                       forall(member(Ref, Refs),
                                              ( forget_compiled_dependencies(Ref),
                                                erase(Ref) )),
                                       retractall(translated_from(_, Term)),
                                       metta_on_function_changed(F),
                                       invalidate_specializations(F),
                                       length(Args, N),
                                       ( \+ ( current_predicate(F/A), functor(H2, F, A), clause(H2, _, _) )
                                         -> retractall(fun(F)), metta_on_function_removed(F)
                                         ; true ),
                                       % Callable classification is part of the
                                       % unified call contract.  Drop the last
                                       % fun/1 marker before rebuilding callers
                                       % so they see data syntax, not a stale
                                       % unknown function call.
                                       notify_mutation(clause_changed(F/N, runtime)),
                                       ( Refs = [] -> Removed = false ; Removed = true ).

%Remove all same atoms:
'remove-atom'(Space, Term, true) :-
    typed_space_runtime_value_ok(Space, Term),
    remove_sexp(Space, Term).

%Updates whose row the translator proved against the space's declared schema
%(typed_space_update_goal/4). A function atom keeps its own clause.
'add-atom-proven'(Space, Term, true) :- Term = [=,_,_], !,
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
                                                         ( is_list(Head) -> append([Space], Head, List), Term =.. List
                                                                          ; Term =.. [Space, '#primitive', Head] ),
                                                         space_call(Term),
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
        Term =.. [Space, Rel | PatArgs]
    ;
        Term =.. [Space, '#primitive', Pattern]
    ),
    space_call(Term),
    \+ cyclic_term(OutPattern),
    Result = OutPattern.

%Get all atoms in space, irregard of arity:
'get-atoms'(Space, Pattern) :- current_predicate(Space/Arity),
                               functor(Head, Space, Arity),
                               clause(Head, true),
                               Head =.. [Space | Args],
                               ( Args = ['#primitive', P] -> Pattern = P ; Pattern = Args ).
