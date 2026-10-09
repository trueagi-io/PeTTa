%%% Compiled-analysis dependency graph %%%
%
% Proof producers return dependencies; this file is the only publication and
% mutation boundary.  A compiled clause is keyed by its real Prolog clause
% reference, so removal and specialization invalidation cannot leave a live
% edge.  Whole-function queries are derived by unioning those clause records.
%
% Owns compiled/validation dependency stores and notify_mutation/1. Consumes
% proof-cache invalidation, canonical declaration queries, translator
% recompile/specialization interfaces, and subject-specific revalidators.
% Boundary: no other unit asserts the stores below.

:- dynamic compiled_deps/4.     % compiled_deps(ClauseRef, F/N, SourceFile, Dependencies)
:- dynamic compiled_dep_edge/3. % compiled_dep_edge(Dependency, ClauseRef, F/N)
:- dynamic validation_deps/2.   % validation_deps(ValidationKey, Dependencies)
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

compiled_function_dependencies(F, Dependencies) :-
    findall(D,
            ( compiled_deps(Ref, F/_, _, Ds),
              clause(_, _, Ref),
              member(D, Ds) ),
            Ds0),
    sort(Ds0, Dependencies).

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
    retractall(validation_deps(Key, _)),
    retractall(validation_dep_edge(_, Key)),
    forget_constructor_dependency_owner(validation(Key)),
    ( ground(Dependencies)
      -> Copy = Dependencies
      ; copy_term_nat(Dependencies, Copy) ),
    sort(Copy, Deps),
    assertz(validation_deps(Key, Deps)),
    forall(member(Dependency, Deps),
           assertz(validation_dep_edge(Dependency, Key))),
    record_constructor_dependency_owner(validation(Key), Deps).

forget_validation_dependencies(Key) :-
    retractall(validation_deps(Key, _)),
    retractall(validation_dep_edge(_, Key)),
    forget_constructor_dependency_owner(validation(Key)).

% Mutation vocabulary:
%   clause_changed(F/N, prevalidated|runtime|derived)
%   declaration_changed(F/N, added|removed)
%   declaration_changed(Kind, Name, added|removed)
%   constructor_set_changed(Type, IntroducedOrRemovedSymbol)
%   broad_mutation(Reason)                 % documented correctness fallback
%
% The broad form is a documented correctness fallback for a future mutation
% that cannot be mapped to a nominal or alias key. No current event needs it.
notify_mutation(clause_changed(FN)) :- !,
    notify_mutation(clause_changed(FN, runtime)).
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
    revalidate_mutation_events(Events, State1, State2),
    maplist(invalidate_mutation_event, MoreEvents),
    notify_mutation_queue(MoreEvents, State2, State).

% Recompilation does not change MeTTa source clauses, but every rebuilt
% function publishes a derived clause-set event which wakes its callers.  Walk
% that reverse-dependency cascade before compiling anything so the unified
% checker can solve the union once.  The real queue below still recompiles in
% its established order and emits every notification immediately; this plan is
% used only to size the ephemeral analysis scope.
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

planned_function_events([], []).
planned_function_events([F|Functions], Events) :-
    compiled_function_arities(F, Arities),
    findall(clause_changed(F/N, derived), member(N, Arities), Here),
    planned_function_events(Functions, Rest),
    append(Here, Rest, Events).

invalidate_mutation_event(Event) :-
    unified_checker_invalidate_event(Event),
    analysis_cache_invalidate_event(Event).

affected_compiled_functions_all(Events, Functions) :-
    findall(F,
            ( member(Event, Events),
              affected_compiled_functions(Event, Affected),
              member(F, Affected) ),
            Functions0),
    sort(Functions0, Sorted),
    mutation_owner_functions(Events, Owners),
    include(member_of(Sorted), Owners, Prioritized),
    subtract(Sorted, Prioritized, Rest),
    append(Prioritized, Rest, Functions).

mutation_owner_functions(Events, Owners) :-
    findall(F,
            ( member(Event, Events), mutation_owner_function(Event, F) ),
            Owners0),
    list_to_set(Owners0, Owners).

mutation_owner_function(declaration_changed(F/_, _), F).
mutation_owner_function(clause_changed(F/_, runtime), F).

member_of(List, Value) :- memberchk(Value, List).

revalidate_mutation_events([], State, State).
revalidate_mutation_events([Event|Events], State0, State) :-
    revalidate_affected_consumers(Event, State0, State1),
    revalidate_mutation_events(Events, State1, State).

pending_recompile_functions(Functions, graph_state(Visited, _), Pending) :-
    exclude(already_visited_function(Visited), Functions, Pending).

already_visited_function(Visited, Function) :-
    memberchk(Function, Visited).

%A source-load clause has already been validated against the file's complete
%prepass, and a derived event names the function the graph just rebuilt.  A
%runtime add/remove, however, must rebuild the changed function itself too:
%boundness provisos and commitment checks are clause-set unions whose emitted
%checks live in every clause.
mutation_seed_functions(clause_changed(F/_, prevalidated), [F]) :- !.
mutation_seed_functions(clause_changed(F/_, derived), [F]) :- !.
mutation_seed_functions(_, []).

affected_compiled_functions(Event, Functions) :-
    findall(F,
            ( mutation_candidate_dependency(Event, Dependency),
              compiled_dep_edge(Dependency, Ref, F/_),
              compiled_deps(Ref, _, File, _),
              clause(_, _, Ref),
              mutation_consumer_file_relevant(Event, File) ),
            Fs0),
    sort(Fs0, Sorted),
    prioritize_mutation_owner(Event, Sorted, Functions).

%The per-file prepass already made every definition and pending body in that
%same file visible. Recompiling its earlier clauses after each later clause is
%both redundant and observably different once specializations exist. A source
%clause still wakes consumers compiled in older files, which is the former
%late-symbol lifecycle.
mutation_consumer_file_relevant(clause_changed(_, prevalidated), File) :- !,
    current_metta_file(Current),
    File \== Current.
mutation_consumer_file_relevant(_, _).

mutation_candidate_dependency(clause_changed(F/N, prevalidated), late_call(F/N)).
mutation_candidate_dependency(clause_changed(F/_, prevalidated), late_symbol(F)).
mutation_candidate_dependency(clause_changed(F/N, Mode), effect(F/N)) :-
    Mode \== prevalidated.
mutation_candidate_dependency(clause_changed(F/N, Mode), decl(F/N)) :-
    Mode \== prevalidated.
mutation_candidate_dependency(clause_changed(F/N, Mode), clause_set(F/N)) :-
    Mode \== prevalidated.
mutation_candidate_dependency(clause_changed(F/N, Mode), output_cert(_, F/N)) :-
    Mode \== prevalidated.
mutation_candidate_dependency(clause_changed(F/N, Mode), late_call(F/N)) :-
    Mode \== prevalidated.
mutation_candidate_dependency(clause_changed(F/_, Mode), late_symbol(F)) :-
    Mode \== prevalidated.
mutation_candidate_dependency(declaration_changed(F/N, _), effect(F/N)).
mutation_candidate_dependency(declaration_changed(F/N, _), decl(F/N)).
mutation_candidate_dependency(declaration_changed(F/N, _), clause_set(F/N)).
mutation_candidate_dependency(declaration_changed(Kind, Name, _),
                              declaration(Kind, Name)).
%Before a late alias/newtype existed, its spelling was conservatively tracked
%as a nominal constructor set. Preserve that late-add direction while all
%positive reads use the generic declaration key.
mutation_candidate_dependency(declaration_changed(alias, Name, added),
                              ctor_set(Name)).
mutation_candidate_dependency(declaration_changed(newtype, Name, added),
                              ctor_set(Name)).
mutation_candidate_dependency(constructor_set_changed(Type, _), ctor_set(Type)).
mutation_candidate_dependency(callable_changed(F), late_symbol(F)).
mutation_candidate_dependency(callable_changed(F), late_call(F/_)).
mutation_candidate_dependency(broad_mutation(_), _).

prioritize_mutation_owner(declaration_changed(F/_, _), Functions, [F|Rest]) :-
    select(F, Functions, Rest), !.
prioritize_mutation_owner(clause_changed(F/_, runtime), Functions, [F|Rest]) :-
    select(F, Functions, Rest), !.
prioritize_mutation_owner(_, Functions, Functions).

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

% One matcher is shared by compiled consumers and memo proofs.
mutation_dependency_matches(clause_changed(F/N, prevalidated), late_call(F/N)).
mutation_dependency_matches(clause_changed(F/_, prevalidated), late_symbol(F)).
mutation_dependency_matches(clause_changed(F/N, Mode), effect(F/N)) :-
    Mode \== prevalidated.
mutation_dependency_matches(clause_changed(F/N, Mode), decl(F/N)) :-
    Mode \== prevalidated.
mutation_dependency_matches(clause_changed(F/N, Mode), clause_set(F/N)) :-
    Mode \== prevalidated.
mutation_dependency_matches(clause_changed(F/N, Mode), output_cert(_, F/N)) :-
    Mode \== prevalidated.
mutation_dependency_matches(clause_changed(F/N, Mode), late_call(F/N)) :-
    Mode \== prevalidated.
mutation_dependency_matches(clause_changed(F/_, Mode), late_symbol(F)) :-
    Mode \== prevalidated.
mutation_dependency_matches(declaration_changed(F/N, _), effect(F/N)).
mutation_dependency_matches(declaration_changed(F/N, _), decl(F/N)).
mutation_dependency_matches(declaration_changed(F/N, _), clause_set(F/N)).
mutation_dependency_matches(declaration_changed(Kind, Name, _),
                            declaration(Kind, Name)).
mutation_dependency_matches(declaration_changed(alias, Name, added),
                            ctor_set(Name)).
mutation_dependency_matches(declaration_changed(newtype, Name, added),
                            ctor_set(Name)).
mutation_dependency_matches(constructor_set_changed(Type, _), ctor_set(Type)).
mutation_dependency_matches(callable_changed(F), late_symbol(F)).
mutation_dependency_matches(callable_changed(F), late_call(F/_)).
mutation_dependency_matches(broad_mutation(_), _).

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
