%%% Compiled-analysis dependency graph: the only publication and mutation
%%% boundary for proof dependencies. A compiled clause is keyed by its real
%%% clause reference, so removal and specialization cannot leave a live edge.

:- dynamic compiled_deps/4.     % compiled_deps(ClauseRef, F/N, SourceFile, Dependencies)
:- dynamic compiled_dep_edge/3. % compiled_dep_edge(Dependency, ClauseRef, F/N)
:- dynamic validation_dep_edge/2. % validation_dep_edge(Dependency, ValidationKey)
:- dynamic constructor_dependency_owner/2. % constructor_dependency_owner(Type, Owner)
:- dynamic constructor_dependency_type/1.  % exact active set, one fact per Type

record_compiled_dependencies(Ref, F/N, Dependencies) :-
    current_metta_file(File),
    record_compiled_dependencies(Ref, F/N, File, Dependencies).

record_compiled_dependencies(Ref, F/N, File, Dependencies) :-
    retractall(compiled_deps(Ref, _, _, _)),
    retractall(compiled_dep_edge(_, Ref, _)),
    forget_constructor_dependency_owner(compiled(Ref)),
    ( ground(Dependencies)
      -> Copy = Dependencies
      ; copy_term_nat(Dependencies, Copy) ),
    sort(Copy, Deps),
    assertz(compiled_deps(Ref, F/N, File, Deps)),
    forall(member(Dependency, Deps),
           assertz(compiled_dep_edge(Dependency, Ref, F/N))),
    record_constructor_dependency_owner(compiled(Ref), Deps).

forget_compiled_dependencies(Ref) :-
    retractall(compiled_deps(Ref, _, _, _)),
    retractall(compiled_dep_edge(_, Ref, _)),
    forget_constructor_dependency_owner(compiled(Ref)).

compiled_dependency_origin(Ref, File) :-
    compiled_deps(Ref, _, File, _), !.
compiled_dependency_origin(_, File) :-
    current_metta_file(File).

recorded_constructor_dependency_types(Types) :-
    findall(T, constructor_dependency_type(T), Types).

record_constructor_dependency_owner(Owner, Dependencies) :-
    forall(( member(ctor_set(Type), Dependencies), atom(Type) ),
           ( assertz(constructor_dependency_owner(Type, Owner)),
             ( constructor_dependency_type(Type)
               -> true
               ; assertz(constructor_dependency_type(Type)) ) )).

forget_constructor_dependency_owner(Owner) :-
    findall(Type, constructor_dependency_owner(Type, Owner), Types0),
    sort(Types0, Types),
    retractall(constructor_dependency_owner(_, Owner)),
    forall(member(Type, Types),
           ( constructor_dependency_owner(Type, _)
             -> true
             ; retractall(constructor_dependency_type(Type)) )).

record_validation_dependencies(Key, Dependencies) :-
    retractall(validation_dep_edge(_, Key)),
    forget_constructor_dependency_owner(validation(Key)),
    ( ground(Dependencies)
      -> Copy = Dependencies
      ; copy_term_nat(Dependencies, Copy) ),
    sort(Copy, Deps),
    forall(member(Dependency, Deps),
           assertz(validation_dep_edge(Dependency, Key))),
    record_constructor_dependency_owner(validation(Key), Deps).

forget_validation_dependencies(Key) :-
    retractall(validation_dep_edge(_, Key)),
    forget_constructor_dependency_owner(validation(Key)).

% Mutation vocabulary:
%   clause_changed(F/N, prevalidated|runtime|derived)
%   declaration_changed(F/N, added|removed)
%   declaration_changed(Kind, Name, added|removed)
%   constructor_set_changed(Type, IntroducedOrRemovedSymbol)
%   callable_changed(F)
notify_mutation(Event) :-
    notify_mutations([Event]).

notify_mutations(Events) :-
    mutation_seed_functions_all(Events, Seed),
    State0 = graph_state(Seed, []),
    maplist(invalidate_mutation_event, Events),
    planned_recompile_functions(Events, State0, Functions),
    with_unified_recompile_analysis(
        Functions,
        notify_mutation_queue(Events, State0, _)).

mutation_seed_functions_all(Events, Seed) :-
    findall(F,
            ( member(Event, Events),
              mutation_seed_functions(Event, Functions),
              member(F, Functions) ),
            Seed0),
    sort(Seed0, Seed).

notify_mutation_queue([], State, State) :- !.
notify_mutation_queue(Events, State0, State) :-
    affected_compiled_functions_all(Events, Functions),
    pending_recompile_functions(Functions, State0, PendingFunctions),
    recompile_affected_functions(
        PendingFunctions, event_batch(Events), State0, State1, MoreEvents),
    foldl(revalidate_affected_consumers, Events, State1, State2),
    maplist(invalidate_mutation_event, MoreEvents),
    notify_mutation_queue(MoreEvents, State2, State).

% Every rebuilt function publishes a derived clause-set event that wakes its
% callers. Walk that cascade before compiling so the unified checker can solve
% the union once; the queue below still recompiles in its own order and emits
% every notification immediately.
planned_recompile_functions(Events, State0, Functions) :-
    planned_recompile_functions_(Events, State0, [], Reversed),
    reverse(Reversed, Functions).

planned_recompile_functions_([], _, Functions, Functions) :- !.
planned_recompile_functions_(Events, State0, Acc0, Functions) :-
    affected_compiled_functions_all(Events, Affected),
    pending_recompile_functions(Affected, State0, Pending),
    planned_function_events(Pending, MoreEvents),
    State0 = graph_state(Visited0, Validated),
    append(Pending, Visited0, Visited),
    reverse(Pending, PendingReversed),
    append(PendingReversed, Acc0, Acc),
    planned_recompile_functions_(
        MoreEvents, graph_state(Visited, Validated), Acc, Functions).

planned_function_events(Functions, Events) :-
    findall(clause_changed(F/N, derived),
            ( member(F, Functions),
              compiled_function_arities(F, Arities),
              member(N, Arities) ),
            Events).

invalidate_mutation_event(Event) :-
    unified_checker_invalidate_event(Event),
    analysis_cache_invalidate_event(Event).

%Consumers of the batch, with the functions the events themselves change
%(a redeclared or runtime-edited function) recompiled first.
affected_compiled_functions_all(Events, Functions) :-
    findall(F,
            ( member(Event, Events),
              mutation_candidate_dependency(Event, Dependency),
              compiled_dep_edge(Dependency, Ref, F/_),
              compiled_deps(Ref, _, File, _),
              clause(_, _, Ref),
              mutation_consumer_file_relevant(Event, File) ),
            Functions0),
    sort(Functions0, Sorted),
    findall(F,
            ( member(Event, Events),
              ( Event = declaration_changed(F/_, _)
              ; Event = clause_changed(F/_, runtime) ) ),
            Owners0),
    list_to_set(Owners0, Owners),
    intersection(Owners, Sorted, Prioritized),
    subtract(Sorted, Prioritized, Rest),
    append(Prioritized, Rest, Functions).

pending_recompile_functions(Functions, graph_state(Visited, _), Pending) :-
    subtract(Functions, Visited, Pending).

%A source-load clause was validated against the file's complete prepass, and
%a derived event names a function the graph just rebuilt. A runtime add/remove
%must rebuild the changed function itself too, because boundness provisos and
%commitment checks are clause-set unions emitted into every clause.
mutation_seed_functions(clause_changed(F/_, prevalidated), [F]) :- !.
mutation_seed_functions(clause_changed(F/_, derived), [F]) :- !.
mutation_seed_functions(_, []).

%The per-file prepass already made every definition in the same file visible,
%and recompiling earlier clauses after each later one is redundant and
%observably different once specializations exist. A source clause still wakes
%consumers compiled in older files.
mutation_consumer_file_relevant(clause_changed(_, prevalidated), File) :- !,
    current_metta_file(Current),
    File \== Current.
mutation_consumer_file_relevant(_, _).

%A source-loaded (prevalidated) clause only resolves late references to it;
%any other clause change invalidates everything that read its function.
mutation_candidate_dependency(clause_changed(F/N, Mode), D) :-
    ( Mode == prevalidated
      -> member(D, [late_call(F/N), late_symbol(F)])
    ; member(D, [effect(F/N), decl(F/N), clause_set(F/N), output_cert(_, F/N),
                 late_call(F/N), late_symbol(F)]) ).
mutation_candidate_dependency(declaration_changed(F/N, _), D) :-
    member(D, [effect(F/N), decl(F/N), clause_set(F/N)]).
mutation_candidate_dependency(declaration_changed(Kind, Name, _),
                              declaration(Kind, Name)).
%A late alias/newtype also wakes readers that tracked its spelling as a
%nominal constructor set.
mutation_candidate_dependency(declaration_changed(alias, Name, added),
                              ctor_set(Name)).
mutation_candidate_dependency(declaration_changed(newtype, Name, added),
                              ctor_set(Name)).
mutation_candidate_dependency(constructor_set_changed(Type, _), ctor_set(Type)).
mutation_candidate_dependency(callable_changed(F), late_symbol(F)).
mutation_candidate_dependency(callable_changed(F), late_call(F/_)).

recompile_affected_functions([], _, State, State, []).
recompile_affected_functions([F|Fs], Event,
                             graph_state(Visited0, Validated),
                             State, Events) :-
    ( memberchk(F, Visited0)
      -> recompile_affected_functions(Fs, Event,
                                      graph_state(Visited0, Validated),
                                      State, Events)
    ; mutation_recompile_diagnostic(Event, F),
      compiled_function_arities(F, BeforeArities),
      recompile_function_clauses(F),
      compiled_function_arities(F, AfterArities),
      append(BeforeArities, AfterArities, Ns0),
      sort(Ns0, Ns),
      findall(clause_changed(F/N, derived), member(N, Ns), FEvents),
      recompile_affected_functions(Fs, Event,
                                   graph_state([F|Visited0], Validated),
                                   State1, RestEvents),
      append(FEvents, RestEvents, Events),
      State = State1
    ).

compiled_function_arities(F, Arities) :-
    findall(N,
            ( compiled_deps(Ref, F/N, _, _),
              clause(_, _, Ref) ),
            Ns0),
    sort(Ns0, Arities).

revalidate_affected_consumers(Event,
                              graph_state(Fs, Validated0),
                              graph_state(Fs, Validated)) :-
    Event = clause_changed(_, prevalidated), !,
    Validated = Validated0.
revalidate_affected_consumers(Event,
                              graph_state(Fs, Validated0),
                              graph_state(Fs, Validated)) :-
    findall(Key,
            ( mutation_candidate_dependency(Event, Dependency),
              validation_dep_edge(Dependency, Key),
              \+ memberchk(Key, Validated0) ),
            Keys0),
    sort(Keys0, Keys),
    forall(member(Key, Keys), revalidate_dependency_consumer(Key, Event)),
    append(Keys, Validated0, Vs0),
    sort(Vs0, Validated).

mutation_recompile_diagnostic(event_batch(Events), F) :- !,
    forall(member(Event, Events),
           mutation_recompile_diagnostic(Event, F)).
mutation_recompile_diagnostic(constructor_set_changed(_, _), F) :- !,
    format(user_error,
           "Warning: a constructor declared after ~w was compiled changes a type it matched on; recompiling ~w~n",
           [F, F]).
mutation_recompile_diagnostic(declaration_changed(alias, Alias, added), F) :- !,
    format(user_error,
           "Warning: type alias ~w arrives after declarations using it; recompiling ~w against the expanded type~n",
           [Alias, F]).
mutation_recompile_diagnostic(declaration_changed(F/N, added), F) :-
    compiled_function_arities(F, Ns),
    memberchk(N, Ns), !,
    format(user_error,
           "Warning: type declaration for ~w arrives after its definition; its clauses are being recompiled against it~n",
           [F]).
mutation_recompile_diagnostic(_, _).
