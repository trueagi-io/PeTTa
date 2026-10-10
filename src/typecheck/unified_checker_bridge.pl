:- module(unified_checker_bridge,
          [ with_unified_file_analysis/2,
            with_unified_clause_analysis/2,
            with_unified_ad_hoc_clause_analysis/2,
            with_unified_recompile_analysis/2,
            with_unified_edge_facts/3,
            with_unified_preanalyzed_form/1,
            current_unified_source_variable/1,
            unified_builtin_call_card/2,
            unified_function_result_fact/3,
            unified_checker_invalidate_event/1
          ]).

/** <module> Sole adapter between the unified analyzer and the translator

This module may query PeTTa's canonical declaration/function stores and may
project already-computed edge facts into the code generator.  It must not
contain a second recursive source checker.  All control-flow, cardinality and
result-shape decisions come from `ir_analyzer` records.
*/

:- use_module(abstract_domain).
:- use_module(ir_analyzer).
:- use_module(relational_ir).
:- use_module(builtin_registry).
:- use_module(unified_checker_cache).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(ugraphs)).

:- meta_predicate with_unified_file_analysis(+, 0).
:- meta_predicate with_unified_clause_analysis(+, 0).
:- meta_predicate with_unified_ad_hoc_clause_analysis(+, 0).
:- meta_predicate with_unified_recompile_analysis(+, 0).
:- meta_predicate with_unified_edge_facts(+, +, 0).
:- meta_predicate with_unified_preanalyzed_form(0).

with_unified_file_analysis(ParsedForms, Goal) :-
    invalidate_enclosing_file_scope(ParsedForms),
    pending_clause_sources(ParsedForms, Pending),
    ( Pending == []
      -> call(Goal)
    ; solve_file_analysis(
          Pending, Summaries, ClauseRecords, CacheEntries),
      current_bridge_generation(Generation),
      with_b_value('$unified_nested_mutations', [], [],
          ( with_bridge_scope(scope(Generation, Summaries, ClauseRecords),
                call(Goal)),
            b_getval('$unified_nested_mutations', Events0),
            sort(Events0, Events) )),
      publish_or_refresh_cache_entries(
          Generation, Pending, CacheEntries, Events) ).

% A nested file is solved while its caller's batch summaries are in scope, and
% its clauses can change how the enclosing file's calls are classified.
% Invalidate the outer generation once at the nested boundary, so the outer
% completion recomputes rather than publishes its pre-import analysis.
invalidate_enclosing_file_scope([]) :- !.
invalidate_enclosing_file_scope(_) :-
    raw_bridge_scope(_), !,
    bump_bridge_generation.
invalidate_enclosing_file_scope(_).

% Dependency invalidation may rebuild several functions: analyze the changed
% set together, expanding its direct-call closure only where no valid closed
% summary remains. Closed summaries are published only after the rebuild
% succeeds.
with_unified_recompile_analysis([], Goal) :- !,
    call(Goal).
with_unified_recompile_analysis(Functions, Goal) :-
    recompile_clause_sources(Functions, Universe, Sources),
    ( Sources == []
      -> call(Goal)
    ; solve_source_closure(
          Sources, Universe, Summaries, ClauseResults, Relevant),
      clause_records_from_results(Sources, ClauseResults, Records),
      summary_cache_entries(Summaries, Relevant, CacheEntries),
      current_bridge_generation(Generation),
      with_bridge_scope(scope(Generation, Summaries, Records),
          call(Goal)),
      publish_cache_entries_if_current(Generation, CacheEntries) ).

recompile_clause_sources(Functions, Universe, Sources) :-
    current_stored_clause_sources(Universe),
    include(source_owned_by(Functions), Universe, Sources).

source_owned_by(Functions, clause_source(F, _, _)) :-
    memberchk(F, Functions).

with_unified_clause_analysis(Source, Goal) :-
    ( bridge_scope(scope(_, _, Records)),
      scope_record(Records, Source, Record)
      -> with_current_clause(Record, Goal)
    ; % A nested import or runtime mutation invalidated the batch summaries
      % but not this occurrence's IR, or the batch scope has ended
      % (recompilation): reanalyze against fresh summaries of the current
      % stored call closure.
      ( raw_bridge_scope(scope(_, _, StaleRecords)),
        scope_record(StaleRecords, Source, StaleRecord),
        current_stored_summaries(Source, Summaries, ClauseResults),
        ( clause_record_from_results(Source, ClauseResults, SolverRecord)
          -> FreshRecord = SolverRecord
        ; refresh_clause_record(StaleRecord, Summaries, FreshRecord) )
      ; stored_clause_source(Source),
        current_stored_summaries(Source, Summaries, ClauseResults),
        ( clause_record_from_results(Source, ClauseResults, SolverRecord)
          -> FreshRecord = SolverRecord
        ; fresh_clause_record(Source, Summaries, FreshRecord) ) )
      -> current_bridge_generation(Generation),
         with_bridge_scope(scope(Generation, Summaries, [FreshRecord]),
                           with_current_clause(FreshRecord, Goal))
    ; call(Goal) ).

%An exact occurrence first, else a variant aligned to this source term:
scope_record(Records, Source, Record) :-
    ( record_for_source(Records, Source, Record)
    ; aligned_record_for_source(Records, Source, Record) ).

% Compiler-generated clauses (lambdas) are outside the parsed batch but need
% the same edge isolation: give one an ephemeral record over the active
% batch's summaries, nested under and restoring the enclosing record.
with_unified_ad_hoc_clause_analysis(Source, Goal) :-
    ( current_analysis_summaries(Summaries),
      try_lower_source_clause(Source, lowered(IR, Env, Origins)),
      analyze_lowered_clause(IR, Summaries, Analysis)
      -> Record = clause_record(Source, IR, Env, Origins, Analysis),
         with_current_clause(Record, Goal)
    ; call(Goal) ).

current_analysis_summaries(Summaries) :-
    bridge_scope(scope(_, Summaries, _)), !.
current_analysis_summaries([]).

record_for_source([Record|_], Source, Record) :-
    Record = clause_record(Stored, _, _, _, _),
    Stored == Source, !.
record_for_source([_|Records], Source, Record) :-
    record_for_source(Records, Source, Record).

aligned_record_for_source(Records, Source, Record) :-
    member(StoredRecord, Records),
    StoredRecord = clause_record(Stored, _, _, _, _),
    Stored =@= Source, !,
    copy_term_nat(StoredRecord, Record),
    Record = clause_record(Aligned, _, _, _, _),
    Aligned = Source.

refresh_clause_record(clause_record(Source, IR, Env, Origins, _), Summaries,
                      clause_record(Source, IR, Env, Origins, Analysis)) :-
    analyze_lowered_clause(IR, Summaries, Analysis).

stored_clause_source(Source) :-
    catch(user:translated_from(Ref, Stored), _, fail),
    clause_property(Ref, predicate(_)),
    Stored =@= Source, !.

fresh_clause_record(Source, Summaries,
                    clause_record(Source, IR, Env, Origins, Analysis)) :-
    try_lower_source_clause(Source, lowered(IR, Env, Origins)),
    analyze_lowered_clause(IR, Summaries, Analysis).

with_unified_edge_facts(SourceCondition, Truth, Goal) :-
    ( current_unified_clause_analysis(_, _, Env, Origins, Analysis),
      once(( origin_result_id(Origins, SourceCondition, TestId),
             analysis_node_state(Analysis, TestId, BaseState) ))
      -> ( analysis_edge_state(Analysis, TestId, Truth, EdgeState)
           -> project_edge_types(Env, BaseState, EdgeState, Goal)
         ; isolate_environment(Env, Goal) )
    ; call(Goal) ).

with_unified_preanalyzed_form(Goal) :-
    with_b_value('$unified_source_form', false, true, Goal).

current_unified_clause_analysis(Source, IR, Env, Origins, Analysis) :-
    catch(b_getval('$unified_current_clause', Record), _, fail),
    Record = clause_record(Source, IR, Env, Origins, Analysis).

current_unified_source_variable(Var) :-
    var(Var),
    current_unified_clause_analysis(_, _, Env, _, _),
    env_var_id(Env, Var, _).

% Project the cardinality proved for this source builtin call. Prolog's ==
% cannot tell apart separately built compounds sharing variables, so every
% matching origin must carry the same card. Restricted to registered builtins:
% a user-function summary may contain the declared contract still being
% validated.
unified_builtin_call_card(SourceCall, Card) :-
    nonvar(SourceCall),
    SourceCall = [F|Args], atom(F), is_list(Args),
    length(Args, Arity),
    once(builtin_spec(F/Arity, _, _, _, _, _)),
    current_unified_clause_analysis(_, IR, _, Origins, Analysis),
    % A node-local upgrade must not hide fallibility elsewhere in the clause
    % (a repeated-variable let pattern): the whole body must be total.
    analysis_card(Analysis, card(1,1)),
    findall(Id,
            ( origin_result_id(Origins, SourceCall, Id),
              ir_node(IR, call(CallId, F, IRArgs)),
              CallId == Id,
              length(IRArgs, Arity) ),
            Ids0),
    sort(Ids0, Ids),
    Ids = [FirstId|RestIds],
    analysis_node_card(Analysis, FirstId, Card),
    maplist(node_has_card(Analysis, Card), RestIds).

node_has_card(Analysis, Card, Id) :-
    analysis_node_card(Analysis, Id, StoredCard),
    StoredCard == Card.

unified_function_result_fact(F, N, Fact) :-
    current_unified_clause_analysis(_, _, _, _, _),
    ( bridge_scope(scope(_, Summaries, _)),
      member(function_summary(F, N, _, ScopedFacts, _, _), Summaries)
      -> Facts = ScopedFacts
    ; unified_function_summary(F, N, _, Facts, _, _) ),
    member(Stored, Facts),
    Stored =@= Fact, !.

unified_function_summary(F, N, Card, Facts, Effects, Diagnostics) :-
    unified_summary_cache_lookup(
        F, N,
        function_summary(F, N, Card, Facts, Effects, Diagnostics)).

unified_checker_invalidate_event(Event) :-
    maybe_invalidate_active_scope(Event),
    unified_summary_cache_invalidate_event(Event).

maybe_invalidate_active_scope(clause_changed(_, prevalidated)) :- !.
maybe_invalidate_active_scope(clause_changed(_, derived)) :- !.
maybe_invalidate_active_scope(declaration_changed(F/_, added)) :-
    catch(user:ho_specialization(_, F), _, fail), !.
% Higher-order specializations are compiler artifacts that summaries never
% rely on, so removing one clears dormant cache rows without invalidating an
% in-flight scope.
maybe_invalidate_active_scope(generated_specialization_removed(_)) :- !.
% Ordinary source forms belong to the batch being compiled. Runtime mutation
% from a runnable does not enter this scope and invalidates the generation.
maybe_invalidate_active_scope(_) :-
    catch(b_getval('$unified_source_form', true), _, fail), !.
maybe_invalidate_active_scope(Event) :-
    bump_bridge_generation,
    record_nested_scope_mutation(Event).

record_nested_scope_mutation(Event) :-
    raw_bridge_scope(_), !,
    catch(b_getval('$unified_nested_mutations', Events0), _, Events0 = []),
    b_setval('$unified_nested_mutations', [Event|Events0]).
record_nested_scope_mutation(_).


% -- Batch solve ---------------------------------------------------------

pending_clause_sources(ParsedForms, Pending) :-
    pending_clause_sources_(ParsedForms, Pending).

pending_clause_sources_([], []).
pending_clause_sources_([parsed(function, _, _, Term)|Forms], Pending) :- !,
    ( Term = [Eq, [F|Args], _], Eq == (=), atom(F), is_list(Args)
      -> length(Args, N),
         Pending = [clause_source(F, N, Term)|Rest]
    ; Pending = Rest ),
    pending_clause_sources_(Forms, Rest).
pending_clause_sources_([_|Forms], Pending) :-
    pending_clause_sources_(Forms, Pending).

solve_file_analysis(Pending, Summaries, ClauseRecords, CacheEntries) :-
    source_keys(Pending, Keys),
    maplist(invalidate_pending_summary, Keys),
    clause_universe(Keys, Pending, ClauseSources),
    prepare_clause_universe(ClauseSources, Clauses),
    initial_touched_summaries(Keys, Initial),
    solve_fixed_point(Clauses, Keys, Initial, Solved, ClauseResults),
    include(summary_for_keys(Keys), Solved, Summaries),
    clause_records_from_results(Pending, ClauseResults, ClauseRecords),
    summary_cache_entries(Summaries, Clauses, CacheEntries).

invalidate_pending_summary(Key) :-
    unified_checker_invalidate_event(clause_changed(Key, prevalidated)).

clause_universe(Keys, Pending, Clauses) :-
    current_stored_clause_sources(Stored),
    include(source_record_for_keys(Keys), Stored, Existing),
    append(Existing, Pending, Clauses).

% Recompilation runs outside the file solver: reusing its records after a
% mutation is unsound, and analyzing the consumer alone loses facts from
% unchanged callees. Build a fresh, invocation-local fixed point over the
% target's current direct-call closure (plus the target itself when it is a
% not-yet-stored pending clause).
current_stored_summaries(Source, Summaries, ClauseResults) :-
    source_clause_key(Source, F/N),
    solve_current_source_closure(
        [clause_source(F, N, Source)], Summaries, ClauseResults).

solve_current_source_closure(RootSources, Summaries, ClauseResults) :-
    current_stored_clause_sources(Stored),
    ensure_sources_in_universe(RootSources, Stored, Sources),
    solve_source_closure(RootSources, Sources, Summaries, ClauseResults, _).

solve_source_closure(RootSources, Sources, Summaries, ClauseResults,
                     Relevant) :-
    prepare_cached_source_closure(
        RootSources, Sources, Relevant, Keys),
    initial_touched_summaries(Keys, Initial),
    solve_fixed_point(Relevant, Keys, Initial, Summaries, ClauseResults).

prepare_cached_source_closure(RootSources, Universe, Prepared, Keys) :-
    source_keys(RootSources, RootKeys),
    sources_for_keys(RootKeys, Universe, InitialSources),
    prepare_clause_universe(InitialSources, InitialPrepared),
    expand_uncached_source_closure(
        InitialPrepared, RootKeys, Universe, Prepared, Keys).

expand_uncached_source_closure(Prepared0, Keys0, Universe, Prepared, Keys) :-
    findall(Callee,
            ( member(Clause, Prepared0),
              prepared_clause_call_key(Clause, Callee),
              \+ memberchk(Callee, Keys0),
              \+ cached_summary_key(Callee),
              source_key_in_universe(Universe, Callee) ),
            Missing0),
    sort(Missing0, Missing),
    ( Missing == []
      -> Prepared = Prepared0, Keys = Keys0
    ; sources_for_keys(Missing, Universe, MoreSources),
      prepare_clause_universe(MoreSources, MorePrepared),
      append(Prepared0, MorePrepared, Prepared1),
      append(Keys0, Missing, Keys1a),
      sort(Keys1a, Keys1),
      expand_uncached_source_closure(
          Prepared1, Keys1, Universe, Prepared, Keys) ).

cached_summary_key(F/N) :-
    unified_function_summary(F, N, _, _, _, _).

source_key_in_universe(Universe, Key) :-
    member(Source, Universe),
    Source = clause_source(_, _, _),
    source_clause_record_key(Source, Key), !.

sources_for_keys(Keys, Universe, Sources) :-
    include(source_record_for_keys(Keys), Universe, Sources).

source_record_for_keys(Keys, Source) :-
    source_clause_record_key(Source, Key),
    memberchk(Key, Keys).

source_clause_record_key(clause_source(F, N, _), F/N).

source_keys(Sources, Keys) :-
    findall(F/N, member(clause_source(F, N, _), Sources), Keys0),
    sort(Keys0, Keys).

source_clause_key([Eq, [F|Args], _], F/N) :-
    Eq == (=), atom(F), is_list(Args), length(Args, N).

current_stored_clause_sources(Sources) :-
    findall(clause_source(F, N, Source),
            ( catch(user:translated_from(Ref, Source), _, fail),
              clause_property(Ref, predicate(_)),
              source_clause_key(Source, F/N) ),
            Sources).

ensure_source_in_universe(Source, Root, Stored, Sources) :-
    ( member(clause_source(F, N, Existing), Stored),
      Root = F/N,
      Existing =@= Source
      -> Sources = Stored
    ; clause_source_for_key(Root, Source, Clause),
      Sources = [Clause|Stored] ).

ensure_sources_in_universe([], Sources, Sources).
ensure_sources_in_universe([clause_source(F, N, Source)|Roots], Stored,
                           Sources) :-
    ensure_source_in_universe(Source, F/N, Stored, WithSource),
    ensure_sources_in_universe(Roots, WithSource, Sources).

clause_source_for_key(F/N, Source, clause_source(F, N, Source)).

prepared_clause_key(prepared_clause(F, N, _, _), F/N).

prepared_clause_call_key(
        prepared_clause(_, _, _, lowered(IR, _, _)), F/N) :-
    ir_node(IR, call(_, F, Args)),
    atom(F), is_list(Args), length(Args, N).

prepared_for_keys(Keys, Clause) :-
    prepared_clause_key(Clause, Key),
    memberchk(Key, Keys).

prepare_clause_universe([], []).
prepare_clause_universe([clause_source(F, N, Source)|Sources],
                        [prepared_clause(F, N, Source, Lowered)|Clauses]) :-
    try_lower_source_clause(Source, Lowered),
    prepare_clause_universe(Sources, Clauses).

initial_touched_summaries([], []).
initial_touched_summaries([F/N|Keys],
                          [function_summary(F, N, Card, [], [], [initial])|Rest]) :-
    declared_call_card(F, N, Card), !,
    initial_touched_summaries(Keys, Rest).
initial_touched_summaries([F/N|Keys],
                          [function_summary(F, N, card(0,many), [], [opaque],
                                            [initial])|Rest]) :-
    initial_touched_summaries(Keys, Rest).

% Solve result summaries in dependency order, retaining the clause analyses
% behind the final summaries. Acyclic components are analyzed once, callee
% first; a recursive component uses a key worklist, so a changed summary
% reanalyzes only its callers.
solve_fixed_point(Clauses, Keys, Current, Solved, ClauseResults) :-
    solve_fixed_point_counted(
        Clauses, Keys, Current, Solved, ClauseResults, _).

solve_fixed_point_counted(Clauses, Keys, Current, Solved, ClauseResults,
                          AnalysisCount) :-
    solver_component_order(Keys, Clauses, Components, Graph),
    solve_components(Components, Graph, Clauses, Current, Solved,
                     ClauseResults, 0, AnalysisCount).

solver_component_order(Keys, Clauses, Components, Graph) :-
    solver_call_graph(Keys, Clauses, Graph),
    graph_sccs(Keys, Graph, SCCs),
    order_sccs_callee_first(SCCs, Graph, Components).

solver_call_graph(Keys, Clauses, Graph) :-
    findall(Caller-Callee,
            ( member(Clause, Clauses),
              prepared_clause_key(Clause, Caller),
              memberchk(Caller, Keys),
              prepared_clause_call_key(Clause, Callee),
              memberchk(Callee, Keys) ),
            Edges0),
    sort(Edges0, Edges),
    vertices_edges_to_ugraph(Keys, Edges, Graph).

graph_sccs(Keys, Graph, SCCs) :-
    transpose_ugraph(Graph, Transposed),
    graph_sccs_(Keys, Graph, Transposed, SCCs).

graph_sccs_([], _, _, []).
graph_sccs_([Key|Keys], Graph, Transposed, [SCC|SCCs]) :-
    reachable(Key, Graph, Forward),
    reachable(Key, Transposed, Backward),
    ord_intersection(Forward, Backward, SCC),
    ord_subtract(Keys, SCC, Remaining),
    graph_sccs_(Remaining, Graph, Transposed, SCCs).

order_sccs_callee_first(SCCs, Graph, Ordered) :-
    maplist(wrap_scc, SCCs, Vertices),
    findall(CallerComponent-CalleeComponent,
            ( member(Caller-Neighbors, Graph),
              member(Callee, Neighbors),
              scc_for_key(SCCs, Caller, CallerSCC),
              scc_for_key(SCCs, Callee, CalleeSCC),
              CallerSCC \== CalleeSCC,
              CallerComponent = scc(CallerSCC),
              CalleeComponent = scc(CalleeSCC) ),
            ComponentEdges0),
    sort(ComponentEdges0, ComponentEdges),
    vertices_edges_to_ugraph(Vertices, ComponentEdges, ComponentGraph),
    top_sort(ComponentGraph, CallerFirst),
    reverse(CallerFirst, CalleeFirst),
    maplist(unwrap_scc, CalleeFirst, Ordered).

wrap_scc(SCC, scc(SCC)).
unwrap_scc(scc(SCC), SCC).

scc_for_key([SCC|_], Key, SCC) :- memberchk(Key, SCC), !.
scc_for_key([_|SCCs], Key, SCC) :- scc_for_key(SCCs, Key, SCC).

solve_components([], _, _, Summaries, Summaries, [], Count, Count).
solve_components([SCC|SCCs], Graph, Clauses, Current, Solved,
                 ClauseResults, Count0, Count) :-
    include(prepared_for_keys(SCC), Clauses, ComponentClauses),
    ( recursive_component(SCC, Graph)
      -> solve_recursive_component(ComponentClauses, SCC, Graph,
                                   Current, Next, ComponentResults,
                                   Count0, Count1)
    ; solve_component_once(ComponentClauses, SCC, Current, Next,
                           ComponentResults, Count0, Count1) ),
    solve_components(SCCs, Graph, Clauses, Next, Solved, RestResults,
                     Count1, Count),
    append(ComponentResults, RestResults, ClauseResults).

recursive_component([_,_|_], _) :- !.
recursive_component([Key], Graph) :-
    neighbors(Key, Graph, Neighbors),
    memberchk(Key, Neighbors).

solve_component_once(Clauses, Keys, Current, Next, ClauseResults,
                     Count0, Count) :-
    analyze_clause_universe(Clauses, Current, Analyses),
    summarize_keys(Keys, Analyses, Derived),
    replace_key_summaries(Keys, Current, Derived, Next),
    ClauseResults = Analyses,
    length(Clauses, ClauseCount),
    Count is Count0 + ClauseCount.

solve_recursive_component(Clauses, Keys, Graph, Current, Solved,
                          ClauseResults, Count0, Count) :-
    length(Keys, KeyCount),
    MaxSteps is 32 * max(1, KeyCount),
    solve_recursive_worklist(Keys, Keys, Graph, Clauses,
                             Current, BaseSolved, [], BaseClauseResults,
                             MaxSteps, Count0, Count1),
    infer_recursive_result_facts(Clauses, Keys, BaseSolved,
                                 BaseClauseResults, Solved, ClauseResults,
                                 Count1, Count).

solve_recursive_worklist([], _, _, _, Summaries, Summaries,
                         ClauseResults, ClauseResults, _, Count, Count).
solve_recursive_worklist([Key|Queue0], Keys, Graph, Clauses,
                         Current, Solved, Results0, ClauseResults,
                         Remaining, Count0, Count) :-
    ( Remaining =< 0
      -> throw(error(unified_summary_nonconvergence(Keys), typecheck))
    ; true ),
    include(prepared_for_keys([Key]), Clauses, KeyClauses),
    solve_component_once(KeyClauses, [Key], Current, Next,
                         KeyResults, Count0, Count1),
    replace_key_clause_results(Key, Results0, KeyResults, Results1),
    ( summary_resolver_view_unchanged(Key, Current, Next)
      -> Queue = Queue0
    ; component_callers(Key, Keys, Graph, Callers),
      ord_union(Queue0, Callers, Queue) ),
    More is Remaining - 1,
    solve_recursive_worklist(Queue, Keys, Graph, Clauses,
                             Next, Solved, Results1, ClauseResults,
                             More, Count1, Count).

replace_key_clause_results(Key, Current, Derived, Next) :-
    exclude(clause_result_for_key(Key), Current, External),
    append(External, Derived, Next).

clause_result_for_key(F/N, clause_result(F, N, _, _)).

component_callers(Callee, Keys, Graph, Callers) :-
    findall(Caller,
            ( member(Caller-Neighbors, Graph),
              memberchk(Caller, Keys),
              memberchk(Callee, Neighbors) ),
            Callers0),
    sort(Callers0, Callers).

% Universal result facts such as proper_bool vanish at the first recursive
% join of a least fixed point, so they are proved coinductively, one fact at
% a time: a result declaration proposes the candidate, the fact is assumed for
% calls to the remaining candidate members of the SCC, every clause is
% analyzed, and members that do not rederive it are removed. One fact per pass
% keeps incompatible assumptions from manufacturing vacuous proofs. Only the
% exported facts change; codegen analyses are refreshed once under them.
infer_recursive_result_facts(Clauses, Keys, Base, BaseResults,
                             Solved, ClauseResults, Count0, Count) :-
    findall(Fact, coinductive_result_fact(Fact), Facts),
    prove_recursive_result_facts(Facts, Clauses, Keys, Base,
                                 Proven, Count0, Count1),
    add_proven_result_facts(Proven, Base, Solved),
    finalize_recursive_result_fact_analyses(
        Proven, Clauses, Solved, BaseResults, ClauseResults,
        Count1, Count).

coinductive_result_fact(proper_bool).

prove_recursive_result_facts([], _, _, _, [], Count, Count).
prove_recursive_result_facts([Fact|Facts], Clauses, Keys, Base,
                             Proven, Count0, Count) :-
    include(result_fact_candidate(Fact), Keys, Candidates),
    prove_recursive_result_fact(Candidates, Fact, Clauses, Base,
                                Survivors, Count0, Count1),
    fact_proofs(Survivors, Fact, Here),
    prove_recursive_result_facts(Facts, Clauses, Keys, Base,
                                 Rest, Count1, Count),
    append(Here, Rest, Proven).

% A Bool result proposes proper_bool; an open Bool result still fails the
% preservation proof.
result_fact_candidate(proper_bool, F/N) :-
    unique_declared_result_type(F, N, 'Bool').

unique_declared_result_type(F, N, Expected) :-
    findall(Output,
            declared_signature_candidate(F, N, _, Output),
            Outputs0),
    variant_dedup(Outputs0, Outputs),
    Outputs = [Output],
    Output == Expected.

prove_recursive_result_fact([], _, _, _, [], Count, Count) :- !.
prove_recursive_result_fact(Candidates, Fact, Clauses, Base,
                            Survivors, Count0, Count) :-
    summaries_with_result_hypothesis(Base, Candidates, Fact, Hypotheses),
    analyze_clause_universe(Clauses, Hypotheses, Analyses),
    include(key_preserves_result_fact(Analyses, Fact),
            Candidates, Retained0),
    sort(Retained0, Retained),
    length(Clauses, ClauseCount),
    Count1 is Count0 + ClauseCount,
    ( Retained == Candidates
      -> Survivors = Retained, Count = Count1
    ; prove_recursive_result_fact(Retained, Fact, Clauses, Base,
                                  Survivors, Count1, Count) ).

summaries_with_result_hypothesis([], _, _, []).
summaries_with_result_hypothesis(
        [function_summary(F, N, Card, Facts, Effects, Diagnostics)|Summaries],
        Candidates, Fact,
        [function_summary(F, N, Card, HypothesisFacts,
                          Effects, Diagnostics)|Hypotheses]) :-
    ( memberchk(F/N, Candidates)
      -> sort([Fact|Facts], HypothesisFacts)
    ; HypothesisFacts = Facts ),
    summaries_with_result_hypothesis(Summaries, Candidates, Fact,
                                     Hypotheses).

key_preserves_result_fact(Analyses, Fact, F/N) :-
    findall(Outcome,
            member(clause_result(F, N, _, Outcome), Analyses),
            Outcomes),
    Outcomes = [_|_],
    maplist(analyzed_result_has_fact(Fact), Outcomes).

analyzed_result_has_fact(Fact, analyzed(_, _, _, Analysis)) :-
    analysis_result_has_fact(Analysis, Fact).

fact_proofs([], _, []).
fact_proofs([Key|Keys], Fact, [proved_result_fact(Key, Fact)|Proofs]) :-
    fact_proofs(Keys, Fact, Proofs).

add_proven_result_facts([], Summaries, Summaries).
add_proven_result_facts([proved_result_fact(F/N, Fact)|Proofs], Base, Solved) :-
    add_summary_result_fact(F, N, Fact, Base, WithFact),
    add_proven_result_facts(Proofs, WithFact, Solved).

add_summary_result_fact(_, _, _, [], []).
add_summary_result_fact(
        F, N, Fact,
        [function_summary(F0, N0, Card, Facts, Effects, Diagnostics)|Summaries],
        [function_summary(F0, N0, Card, UpdatedFacts,
                          Effects, Diagnostics)|Updated]) :-
    ( F == F0, N =:= N0
      -> sort([Fact|Facts], UpdatedFacts)
    ; UpdatedFacts = Facts ),
    add_summary_result_fact(F, N, Fact, Summaries, Updated).

finalize_recursive_result_fact_analyses([], _, _, Results, Results,
                                        Count, Count) :- !.
finalize_recursive_result_fact_analyses(_, Clauses, Solved, _, Results,
                                        Count0, Count) :-
    analyze_clause_universe(Clauses, Solved, Results),
    length(Clauses, ClauseCount),
    Count is Count0 + ClauseCount.

% Only these fields are observable by bridge_resolve_call/5; a seed-only
% diagnostic such as `initial` must not drive propagation.
summary_resolver_view_unchanged(Key, Left, Right) :-
    summary_for_key(Key, Left, LeftSummary),
    summary_for_key(Key, Right, RightSummary),
    summary_resolver_view(LeftSummary, LeftView),
    summary_resolver_view(RightSummary, RightView),
    LeftView == RightView.

summary_for_key(F/N, Summaries, Summary) :-
    member(Summary, Summaries),
    Summary = function_summary(F, N, _, _, _, _), !.

summary_resolver_view(
        function_summary(_, _, Card, Facts, Effects, _),
        resolver_summary(Card, NormalFacts, NormalEffects)) :-
    ground(Card-Facts-Effects),
    sort(Facts, NormalFacts),
    sort(Effects, NormalEffects).

analyze_clause_universe([], _, []).
analyze_clause_universe([prepared_clause(F, N, Source, Lowered)|Clauses], Summaries,
                        [clause_result(F, N, Source, Outcome)|Results]) :-
    analyze_prepared_clause(Lowered, Summaries, Outcome),
    analyze_clause_universe(Clauses, Summaries, Results).

clause_records_from_results([], _, []).
clause_records_from_results([clause_source(_, _, Source)|Pending], Results,
                            Records) :-
    ( clause_record_from_results(Source, Results, Record)
      -> Records = [Record|Rest]
    ; Records = Rest ),
    clause_records_from_results(Pending, Results, Rest).

clause_record_from_results(Source, Results,
                           clause_record(Source, IR, Env, Origins, Analysis)) :-
    clause_result_for_source(Results, Source,
                             analyzed(IR, Env, Origins, Analysis)).

clause_result_for_source([clause_result(_, _, Stored, Outcome)|_], Source,
                         Outcome) :-
    Stored == Source, !.
clause_result_for_source([_|Results], Source, Outcome) :-
    clause_result_for_source(Results, Source, Outcome).

try_lower_source_clause(Source, Outcome) :-
    catch(( lower_clause_with_origins(Source, IR, _, Env, Origins)
            -> Outcome = lowered(IR, Env, Origins)
          ; Outcome = unsupported(lowering_failed) ),
          Error,
          unsupported_lowering_error(Error, Outcome)).

unsupported_lowering_error(error(domain_error(Domain, _), Context),
                           unsupported(Domain)) :-
    memberchk(Domain, [case_pair, let_binding, nonempty_sequence]),
    lowering_error_context(Context), !.
unsupported_lowering_error(Error, _) :- throw(Error).

lowering_error_context(lower_expr/4).

analyze_prepared_clause(unsupported(Reason), _, unsupported(Reason)) :- !.
analyze_prepared_clause(lowered(IR, Env, Origins), Summaries, Outcome) :-
    ( analyze_lowered_clause(IR, Summaries, Analysis)
      -> Outcome = analyzed(IR, Env, Origins, Analysis)
    ; Outcome = unsupported(analysis_failed) ).

analyze_lowered_clause(IR, Summaries, Analysis) :-
    state_empty(State),
    Options = [resolve_type(unified_checker_bridge:bridge_resolve_type),
               constructor_signature(
                   unified_checker_bridge:bridge_constructor_signature),
               resolve_call(
                   unified_checker_bridge:bridge_resolve_call(Summaries))],
    analyze_ir(IR, State, Options, Analysis).

summarize_keys([], _, []).
summarize_keys([F/N|Keys], Analyses,
               [function_summary(F, N, Card, Facts, Effects, Diagnostics)|Rest]) :-
    include(clause_result_key(F, N), Analyses, FunctionAnalyses),
    summarize_function(F, N, FunctionAnalyses,
                       Card, Facts, Effects, Diagnostics),
    summarize_keys(Keys, Analyses, Rest).

clause_result_key(F, N, clause_result(F, N, _, _)).

summarize_function(F, N, Results, Card, Facts, Effects, Diagnostics) :-
    findall(Reason,
            member(clause_result(F, N, _, unsupported(Reason)), Results),
            Unsupported),
    ( Unsupported = [_|_]
      -> Card = card(0,many), Facts = [], Effects = [opaque],
         sort(Unsupported, Reasons),
         Diagnostics = [unsupported_clauses(Reasons)]
    ; maplist(clause_analysis, Results, Analyses),
      analyses_common_result_facts(Analyses, Facts),
      analyses_effects(Analyses, Effects0), sort(Effects0, Effects),
      analyses_diagnostics(Analyses, Diagnostics0),
      sort(Diagnostics0, Diagnostics),
      ( declared_call_card(F, N, DeclaredCard)
        -> Card = DeclaredCard
      ; analyses_choice_card(Analyses, Card) ) ).

clause_analysis(clause_result(_, _, _, analyzed(_, _, _, Analysis)), Analysis).

analyses_common_result_facts([], []).
analyses_common_result_facts([Analysis|Analyses], Facts) :-
    analysis_result_facts(Analysis, First),
    foldl(intersect_analysis_result_facts, Analyses, First, Common),
    include(exportable_fact, Common, Facts0),
    variant_dedup(Facts0, Facts).

analysis_result_facts(Analysis, Facts) :-
    analysis_result(Analysis, Result),
    analysis_state(Analysis, State),
    state_facts(State, Result, Facts).

intersect_analysis_result_facts(Analysis, Facts0, Facts) :-
    analysis_result_facts(Analysis, Other),
    include(fact_in(Other), Facts0, Facts).

fact_in(Facts, Fact) :- variant_member(Fact, Facts).

exportable_fact(type(Type)) :- ground(Type).
exportable_fact(proper_bool).
exportable_fact(proper_list).
exportable_fact(nonempty_list).
exportable_fact(proper_list_length(Length)) :-
    integer(Length), Length >= 0.
exportable_fact(expr).
exportable_fact(number).
exportable_fact(ground).
exportable_fact(nonvar).
exportable_fact(duplicate_free).

analyses_effects([], []).
analyses_effects([Analysis|Analyses], Effects) :-
    analysis_effects(Analysis, Here),
    analyses_effects(Analyses, Rest),
    append(Here, Rest, Effects).

analyses_diagnostics([], []).
analyses_diagnostics([Analysis|Analyses], Diagnostics) :-
    analysis_diagnostics(Analysis, Here),
    analyses_diagnostics(Analyses, Rest),
    append(Here, Rest, Diagnostics).

analyses_choice_card([], card(0,0)).
analyses_choice_card([Analysis|Analyses], Card) :-
    analysis_card(Analysis, First),
    foldl(choice_analysis_card, Analyses, First, Card).

choice_analysis_card(Analysis, Card0, Card) :-
    analysis_card(Analysis, Other),
    card_choice(Card0, Other, Card).

replace_key_summaries(Keys, Current, Derived, Next) :-
    exclude(summary_for_keys(Keys), Current, External),
    append(Derived, External, Next0),
    sort(Next0, Next).

summary_for_keys(Keys, function_summary(F, N, _, _, _, _)) :-
    memberchk(F/N, Keys).

% -- Closed summary cache boundary --------------------------------------

summary_cache_entries([], _, []).
summary_cache_entries([Summary|Summaries], Clauses, Entries) :-
    ( cacheable_summary_entry(Summary, Clauses, Entry)
      -> Entries = [Entry|Rest]
    ; Entries = Rest ),
    summary_cache_entries(Summaries, Clauses, Rest).

cacheable_summary_entry(
        Summary, Clauses, cache_entry(Summary, Dependencies)) :-
    Summary = function_summary(F, N, _, _, _, _),
    \+ generated_summary_symbol(F),
    current_predicate(user:analysis_function_decl_dependencies/2),
    current_predicate(user:analysis_type_dependency/2),
    function_summary_dependencies(F/N, Clauses, Dependencies),
    ground(Summary-Dependencies).

generated_summary_symbol(F) :-
    catch(user:ho_specialization(_, F), _, fail), !.

function_summary_dependencies(F/N, Clauses, Dependencies) :-
    findall(CallKey,
            ( member(Clause, Clauses),
              prepared_clause_key(Clause, F/N),
              prepared_clause_call_key(Clause, CallKey) ),
            CallKeys0),
    sort(CallKeys0, CallKeys),
    findall(Dependency,
            ( member(CallKey, CallKeys),
              summary_call_dependency(CallKey, Dependency) ),
            CallDependencies),
    findall(ConstructorKey,
            ( member(Clause, Clauses),
              prepared_clause_key(Clause, F/N),
              prepared_clause_constructor_key(Clause, ConstructorKey) ),
            ConstructorKeys0),
    sort(ConstructorKeys0, ConstructorKeys),
    findall(Dependency,
            ( member(ConstructorKey, ConstructorKeys),
              summary_constructor_dependency(ConstructorKey, Dependency) ),
            ConstructorDependencies),
    findall(TypeRef,
            ( member(Clause, Clauses),
              prepared_clause_key(Clause, F/N),
              prepared_clause_type_ref(Clause, TypeRef) ),
            TypeRefs0),
    sort(TypeRefs0, TypeRefs),
    findall(Dependency,
            ( member(TypeRef, TypeRefs),
              user:analysis_type_dependency(TypeRef, Dependency) ),
            TypeDependencies),
    findall(Name,
            ( member(Key, [F/N|CallKeys]), Key = Name/_
            ; member(Key, ConstructorKeys), Key = Name/_ ),
            DeclarationNames0),
    sort(DeclarationNames0, DeclarationNames),
    findall(Dependency,
            ( member(Name, DeclarationNames),
              user:analysis_function_decl_dependencies(Name, NameDependencies),
              member(Dependency, NameDependencies) ),
            DeclarationDependencies),
    append([CallDependencies, ConstructorDependencies,
            TypeDependencies, DeclarationDependencies], All0),
    sort(All0, Dependencies).

summary_call_dependency(F/N, summary(F/N)).
summary_call_dependency(F/N, clause_set(F/N)).
summary_call_dependency(F/N, decl(F/N)).
summary_call_dependency(F/N, effect(F/N)).
summary_call_dependency(F/_, declaration(origin, F)).

prepared_clause_constructor_key(
        prepared_clause(_, _, _, lowered(IR, _, _)), Tag/Arity) :-
    ir_node(IR, construct(_, pattern(Tag), Children)),
    atom(Tag), is_list(Children), length(Children, Arity).

prepared_clause_type_ref(
        prepared_clause(_, _, _, lowered(IR, _, _)), TypeRef) :-
    ir_node(IR, construct(_, typed_pattern(TypeRef), _)),
    nonvar(TypeRef),
    TypeRef \= declared_arg(_, _, _).

summary_constructor_dependency(F/N, decl(F/N)).
summary_constructor_dependency(F/_, declaration(origin, F)).

publish_cache_entries_if_current(Generation, Entries) :-
    current_bridge_generation(Current),
    ( Current =:= Generation
      -> unified_summary_cache_store_many(Entries)
    ; true ).

publish_or_refresh_cache_entries(Generation, Pending, Entries, Events) :-
    current_bridge_generation(Current),
    ( Current =:= Generation
      -> unified_summary_cache_store_many(Entries)
    ; refresh_pending_cache_entries(Pending, Events) ).


refresh_pending_cache_entries(Pending, Events) :-
    maplist(unified_checker_invalidate_event, Events),
    source_keys(Pending, Keys),
    maplist(invalidate_pending_summary, Keys),
    current_stored_clause_sources(Stored),
    sources_for_keys(Keys, Stored, CurrentRoots),
    ( CurrentRoots == []
      -> true
    ; solve_source_closure(
          CurrentRoots, Stored, Summaries, _, Relevant),
      summary_cache_entries(Summaries, Relevant, FreshEntries),
      unified_summary_cache_store_many(FreshEntries) ).

% -- Legacy resolvers ----------------------------------------------------

bridge_resolve_type(declared_arg(F, Arity, Index), Type) :-
    findall(ATs,
            declared_signature_candidate(F, Arity, ATs, _),
            Declarations0),
    variant_dedup(Declarations0, Declarations),
    Declarations = [ArgTypes],
    nth0(Index, ArgTypes, Type).

bridge_constructor_signature(Tag, Arity, ArgTypes, ResultType) :-
    atom(Tag), integer(Arity),
    findall(ATs-OT,
            declared_signature_candidate(Tag, Arity, ATs, OT),
            Candidates0),
    variant_dedup(Candidates0, Candidates),
    Candidates = [ArgTypes-ResultType].

bridge_resolve_call(Summaries, F, ArgIds, _, Resolution) :-
    atom(F), length(ArgIds, N),
    ( member(function_summary(F, N, Card, Facts, Effects, _), Summaries)
      -> facts_posts(Facts, Posts),
         Resolution = summary(Posts, Card, Effects)
    ; unified_function_summary(F, N, Card, Facts, Effects, _)
      -> facts_posts(Facts, Posts),
         Resolution = summary(Posts, Card, Effects)
    ; declared_call_card(F, N, Card)
      -> declared_result_facts(F, N, Facts),
         facts_posts(Facts, Posts),
         Resolution = summary(Posts, Card, [call(F/N)])
    ; \+ catch(user:fun(F), _, fail)
      -> Resolution = data
    ; Resolution = unknown ).

declared_result_facts(F, N, Facts) :-
    findall(OT, declared_signature_candidate(F, N, _, OT), Outputs0),
    variant_dedup(Outputs0, [Output]), !,
    ( ground(Output) -> Facts = [type(Output)] ; Facts = [] ).
declared_result_facts(_, _, []).

% Production reads fn_decl_arity/4; the module matrix loads the bridge without
% the declaration views, so its fixtures assert fn_decl/6 directly.
declared_signature_candidate(F, N, ArgTypes, Output) :-
    current_predicate(user:fn_decl_arity/4),
    user:fn_decl_arity(F, N, ArgTypes, Output).
declared_signature_candidate(F, N, ArgTypes, Output) :-
    \+ normalized_signature_available(F, N),
    current_predicate(user:fn_decl/6),
    user:fn_decl(F, N, scheme(ArgTypes, Output), _, _, _).

normalized_signature_available(F, N) :-
    current_predicate(user:fn_decl_arity/4),
    once(user:fn_decl_arity(F, N, _, _)).

facts_posts([], []).
facts_posts([Fact|Facts], [ensure(result, Fact)|Posts]) :-
    facts_posts(Facts, Posts).

declared_call_card(F, N, Card) :-
    catch(user:fn_determinism(F, N, Det), _, fail),
    declared_det_card(Det, Card).

declared_det_card(det, card(1,1)).
declared_det_card(semidet, card(0,1)).
declared_det_card(nondet, card(0,many)).
declared_det_card(effect(det), card(1,1)).
declared_det_card(effect(semidet), card(0,1)).
declared_det_card(effect(nondet), card(0,many)).


% -- Projection into code generation -----------------------------------

project_edge_types(Env, BaseState, EdgeState, Goal) :-
    edge_typed_variables(Env, BaseState, EdgeState, Typed),
    snapshot_environment_attrs(Env, OriginalAttrs),
    copy_term(OriginalAttrs, BranchAttrs),
    setup_call_cleanup(
        ( install_environment_attrs(Env, BranchAttrs),
          apply_edge_types(Typed) ),
        ( call(Goal),
          branch_inference_updates(
              Env, Typed, OriginalAttrs, BranchAttrs, Updates) ),
        restore_environment_attrs(Env, OriginalAttrs, Updates)).

% Even an analyzer-proven unreachable arm must be compiled for runtime code
% shape, but none of the facts or inference constraints learned while doing so
% may escape.  This is the no-edge counterpart of project_edge_types/4.
isolate_environment(Env, Goal) :-
    snapshot_environment_attrs(Env, OriginalAttrs),
    copy_term(OriginalAttrs, BranchAttrs),
    setup_call_cleanup(
        install_environment_attrs(Env, BranchAttrs),
        call(Goal),
        install_environment_attrs(Env, OriginalAttrs)).

restore_environment_attrs(Env, OriginalAttrs, Updates) :-
    install_environment_attrs(Env, OriginalAttrs),
    ( var(Updates) -> true ; apply_inference_updates(Updates) ).

% Branch isolation keeps genuine input requirements found by inference: only
% bindings of the inference engine's own open parameter variables survive, and
% never for a variable typed by the edge itself. Arithmetic in a reachable arm
% still infers a Number parameter, but a constructor match cannot specialize a
% declared polymorphic parameter.
branch_inference_updates([], _, [], [], []).
branch_inference_updates([binding(_, Var)|Bindings], Typed,
                         [attrs(OriginalKnown, _, _)|OriginalAttrs],
                         [attrs(BranchKnown, _, _)|BranchAttrs], Updates) :- !,
    ( \+ typed_variable(Typed, Var),
      OriginalKnown = some([OriginalType]), var(OriginalType),
      BranchKnown = some([BranchType]), nonvar(BranchType),
      current_inference_assumption(Var, OriginalType),
      concrete_inference_constraint(BranchType)
      -> Updates = [bind(OriginalType, BranchType)|Rest]
    ; Updates = Rest ),
    branch_inference_updates(Bindings, Typed, OriginalAttrs, BranchAttrs, Rest).
branch_inference_updates([_|Bindings], Typed, [_|OriginalAttrs],
                         [_|BranchAttrs], Updates) :-
    branch_inference_updates(Bindings, Typed, OriginalAttrs, BranchAttrs, Updates).

typed_variable([typed(Stored, _)|_], Var) :- Stored == Var, !.
typed_variable([_|Typed], Var) :- typed_variable(Typed, Var).

current_inference_assumption(Var, Type) :-
    catch(b_getval('$assumptions', Pairs), _, fail),
    member(a(StoredVar, StoredType), Pairs),
    StoredVar == Var,
    StoredType == Type, !.

concrete_inference_constraint(Type) :-
    nonvar(Type),
    \+ catch(user:wildcard_type(Type), _, fail),
    \+ catch(user:unknown_candidate(Type), _, fail).

apply_inference_updates([]).
apply_inference_updates([bind(Open, Type)|Updates]) :-
    Open = Type,
    apply_inference_updates(Updates).

edge_typed_variables([], _, _, []).
edge_typed_variables([binding(Id, Var)|Bindings], BaseState, State, Typed) :- !,
    findall(Type,
            ( state_type_fact(State, Id, Type),
              \+ state_has_fact(BaseState, Id, type(Type)),
              concrete_projectable_type(Type),
              type_is_new_for_variable(Var, Type) ),
            Types0),
    variant_dedup(Types0, Types),
    ( Types == [] -> Typed = Rest ; Typed = [typed(Var, Types)|Rest] ),
    edge_typed_variables(Bindings, BaseState, State, Rest).
edge_typed_variables([_|Bindings], BaseState, State, Typed) :-
    edge_typed_variables(Bindings, BaseState, State, Typed).

state_type_fact(State, Id, Type) :-
    state_has_fact(State, Id, Fact),
    Fact = type(Type).

concrete_projectable_type(Type) :-
    ground(Type),
    \+ catch(user:wildcard_type(Type), _, fail).

type_is_new_for_variable(Var, Type) :-
    ( catch(user:known_candidates(Var, Known), _, fail)
      -> \+ ( member(Stored, Known), Stored =@= Type )
    ; true ).

apply_edge_types([]).
apply_edge_types([typed(Var, Types)|Typed]) :-
    apply_variable_types(Types, Var),
    apply_edge_types(Typed).

% Branch-local refinements must not bind the declaration's type variables, as
% add_known_type/2 would; replace only the temporary candidate view, which
% restore_environment_attrs/2 reinstates.
apply_variable_types(Types, Var) :-
    put_attr(Var, tknown, Types).

snapshot_environment_attrs([], []).
snapshot_environment_attrs([binding(_, Var)|Bindings],
                           [attrs(TKnown, MReq, ProperList)|Attrs]) :- !,
    snapshot_attribute(Var, tknown, TKnown),
    snapshot_attribute(Var, mreq, MReq),
    snapshot_attribute(Var, proper_list_cert, ProperList),
    snapshot_environment_attrs(Bindings, Attrs).
snapshot_environment_attrs([_|Bindings], [attrs(none, none, none)|Attrs]) :-
    snapshot_environment_attrs(Bindings, Attrs).

snapshot_attribute(Var, Module, some(Value)) :-
    get_attr(Var, Module, Value), !.
snapshot_attribute(_, _, none).

install_environment_attrs([], []).
install_environment_attrs([binding(_, Var)|Bindings],
                          [attrs(TKnown, MReq, ProperList)|Attrs]) :- !,
    del_attrs(Var),
    install_attribute(Var, tknown, TKnown),
    install_attribute(Var, mreq, MReq),
    install_attribute(Var, proper_list_cert, ProperList),
    install_environment_attrs(Bindings, Attrs).
install_environment_attrs([_|Bindings], [_|Attrs]) :-
    install_environment_attrs(Bindings, Attrs).

install_attribute(_, _, none) :- !.
install_attribute(Var, Module, some(Value)) :-
    put_attr(Var, Module, Value).


% -- Scope and set helpers ----------------------------------------------

bridge_scope(Scope) :-
    raw_bridge_scope(Scope),
    Scope = scope(Generation, _, _),
    current_bridge_generation(Current),
    Generation =:= Current.

raw_bridge_scope(Scope) :-
    catch(b_getval('$unified_checker_scope', Scope), _, fail),
    Scope = scope(_, _, _).

current_bridge_generation(Generation) :-
    ( catch(nb_getval('$unified_checker_generation', Stored), _, fail)
      -> Generation = Stored
    ; Generation = 0,
      nb_setval('$unified_checker_generation', Generation) ).

bump_bridge_generation :-
    current_bridge_generation(Current),
    Next is Current + 1,
    nb_setval('$unified_checker_generation', Next).

%Run Goal with the backtrackable global Key set to Value, then restore the
%outer value (Default when Key was unset).
with_b_value(Key, Default, Value, Goal) :-
    ( catch(b_getval(Key, Outer), _, fail) -> true ; Outer = Default ),
    setup_call_cleanup(b_setval(Key, Value), Goal, b_setval(Key, Outer)).

with_bridge_scope(Scope, Goal) :-
    with_b_value('$unified_checker_scope', inactive, Scope, Goal).

with_current_clause(Record, Goal) :-
    with_b_value('$unified_current_clause', inactive, Record, Goal).



:- begin_tests(unified_checker_bridge).

test(open_types_do_not_cross_summary_boundary) :-
    \+ exportable_fact(type(_)),
    exportable_fact(type('Bool')),
    \+ exportable_fact(proper_list_length(_)),
    exportable_fact(proper_list_length(2)).

test(exact_builtin_call_card_uses_retained_flow_analysis) :-
    Call = [decons, Term],
    Body = [if,
            [and, ['is-expr', Term], [not, [==, Term, []]]],
            [let, [Head, Tail], Call, true],
            false],
    Source = [=, [bridge_decons_card, Term], Body],
    try_lower_source_clause(Source, lowered(IR, Env, Origins)),
    state_empty(State),
    analyze_ir(IR, State, Analysis),
    Record = clause_record(Source, IR, Env, Origins, Analysis),
    copy_term(Call, CopiedCall),
    with_current_clause(Record,
        ( unified_builtin_call_card(Call, card(1,1)),
          \+ unified_builtin_call_card(CopiedCall, _) )),
    var(Term), var(Head), var(Tail).

test(node_card_does_not_hide_fallible_positional_match) :-
    Call = [decons, Term],
    Body = [if,
            [and, ['is-expr', Term], [not, [==, Term, []]]],
            [let, [Field, Field], Call, true],
            false],
    Source = [=, [bridge_decons_repeated, Term], Body],
    try_lower_source_clause(Source, lowered(IR, Env, Origins)),
    state_empty(State),
    analyze_ir(IR, State, Analysis),
    analysis_card(Analysis, card(0,1)),
    Record = clause_record(Source, IR, Env, Origins, Analysis),
    with_current_clause(Record,
        \+ unified_builtin_call_card(Call, _)),
    var(Term), var(Field).

test(ambiguous_equal_source_calls_require_one_card) :-
    First = [decons, Term],
    Second = [decons, Term],
    Source = [=, [bridge_decons_ambiguous, Term],
              [progn,
               [if, [and, ['is-expr', Term], [not, [==, Term, []]]],
                First, false],
               Second]],
    try_lower_source_clause(Source, lowered(IR, Env, Origins)),
    state_empty(State),
    analyze_ir(IR, State, Analysis),
    Record = clause_record(Source, IR, Env, Origins, Analysis),
    with_current_clause(Record,
        ( \+ unified_builtin_call_card(First, card(1,1)),
          \+ unified_builtin_call_card(Second, card(1,1)) )),
    var(Term).

test(unsupported_lowering_is_a_clause_local_fallback) :-
    Source = [=, [fallback_case, X], [case, X, [bad]]],
    try_lower_source_clause(Source, unsupported(case_pair)),
    var(X).

test(variant_clauses_keep_occurrence_identity) :-
    First = [=, [duplicate_identity, X], [==, X, a]],
    Second = [=, [duplicate_identity, Y], [==, Y, a]],
    Pending = [clause_source(duplicate_identity, 1, First),
               clause_source(duplicate_identity, 1, Second)],
    clause_universe([duplicate_identity/1], Pending, Universe),
    prepare_clause_universe(Universe, Prepared),
    once(( member(prepared_clause(_, _, S1, _), Prepared), S1 == First )),
    once(( member(prepared_clause(_, _, S2, _), Prepared), S2 == Second )),
    length(Prepared, 2),
    X \== Y.

test(variant_record_is_aligned_to_recompiled_source) :-
    Stored = [=, [aligned_clause, X], [pair, X, Y]],
    StoredRecord = clause_record(
                       Stored, aligned_ir,
                       [binding(id(1), X), binding(id(2), Y)],
                       [origin(id(1), X), origin(id(2), Y)], aligned_analysis),
    copy_term_nat(Stored, Source),
    Source = [=, [aligned_clause, SourceX], [pair, SourceX, SourceY]],
    aligned_record_for_source([StoredRecord], Source, Record),
    Record = clause_record(Aligned, aligned_ir,
                           [binding(id(1), EnvX), binding(id(2), EnvY)],
                           _, aligned_analysis),
    Aligned == Source,
    EnvX == SourceX,
    EnvY == SourceY,
    X \== SourceX,
    Y \== SourceY.

test(recompile_closure_stops_at_cached_callee,
     [setup((unified_summary_cache_reset,
             unified_summary_cache_store_many([cache_entry(
                 function_summary(cache_stop_leaf, 0, card(1,1),
                                  [proper_bool], [], []), [])]))),
      cleanup(unified_summary_cache_reset)]) :-
    Root = clause_source(cache_stop_root, 0,
                         [=, [cache_stop_root], [cache_stop_mid]]),
    Sources = [
        Root,
        clause_source(cache_stop_mid, 0,
                      [=, [cache_stop_mid], [cache_stop_leaf]]),
        clause_source(cache_stop_leaf, 0,
                      [=, [cache_stop_leaf], true]),
        clause_source(cache_stop_unrelated, 0,
                      [=, [cache_stop_unrelated], false])
    ],
    prepare_cached_source_closure([Root], Sources, Prepared, Keys),
    assertion(Keys == [cache_stop_mid/0, cache_stop_root/0]),
    assertion(length(Prepared, 2)).

test(acyclic_solver_analyzes_each_clause_once_and_retains_results) :-
    Sources = [
        clause_source(solver_chain_0, 0,
                      [=, [solver_chain_0], true]),
        clause_source(solver_chain_1, 0,
                      [=, [solver_chain_1], [solver_chain_0]]),
        clause_source(solver_chain_2, 0,
                      [=, [solver_chain_2], [solver_chain_1]]),
        clause_source(solver_chain_3, 0,
                      [=, [solver_chain_3], [solver_chain_2]]),
        clause_source(solver_chain_4, 0,
                      [=, [solver_chain_4], [solver_chain_3]])
    ],
    prepare_clause_universe(Sources, Prepared),
    source_keys(Sources, Keys),
    initial_touched_summaries(Keys, Initial),
    solve_fixed_point_counted(
        Prepared, Keys, Initial, Summaries, Results, Count),
    assertion(Count == 5),
    assertion(length(Results, 5)),
    summary_for_key(solver_chain_4/0, Summaries, LastSummary),
    summary_resolver_view(
        LastSummary,
        resolver_summary(card(1,1), Facts, _)),
    assertion(memberchk(proper_bool, Facts)).

test(recursive_diagnostic_only_delta_does_not_reanalyze) :-
    Sources = [
        clause_source(solver_inert_self, 0,
                      [=, [solver_inert_self], [solver_inert_self]])
    ],
    prepare_clause_universe(Sources, Prepared),
    source_keys(Sources, Keys),
    initial_touched_summaries(Keys, Initial),
    solve_fixed_point_counted(
        Prepared, Keys, Initial, Summaries, Results, Count),
    assertion(Count == 1),
    assertion(Results = [clause_result(solver_inert_self, 0, _, _)]),
    summary_for_key(solver_inert_self/0, Summaries,
                    function_summary(_, _, card(0,many), [], [opaque], [])),
    Initial = [InitialSummary],
    summary_for_key(solver_inert_self/0, Summaries, FinalSummary),
    assertion(summary_resolver_view(InitialSummary, View)),
    assertion(summary_resolver_view(FinalSummary, View)).

test(recursive_worklist_retains_final_propagated_analysis) :-
    Sources = [
        clause_source(solver_mutual_a, 0,
                      [=, [solver_mutual_a],
                          [if, true, true, [solver_mutual_b]]]),
        clause_source(solver_mutual_b, 0,
                      [=, [solver_mutual_b], [solver_mutual_a]])
    ],
    prepare_clause_universe(Sources, Prepared),
    source_keys(Sources, Keys),
    initial_touched_summaries(Keys, Initial),
    solve_fixed_point_counted(
        Prepared, Keys, Initial, Summaries, Results, Count),
    assertion(Count == 3),
    summary_for_key(solver_mutual_b/0, Summaries, BSummary),
    summary_resolver_view(
        BSummary, resolver_summary(card(1,1), BFacts, _)),
    assertion(memberchk(proper_bool, BFacts)),
    once(member(clause_result(solver_mutual_b, 0, _,
                              analyzed(_, _, _, BAnalysis)), Results)),
    assertion(analysis_result_has_fact(BAnalysis, proper_bool)).

% A least fixed point cannot discover a result-shape property when every base
% result is separated from the caller by a recursive edge; these tests cover
% the greatest-fixed-point part of the solver.

test(recursive_self_bool_uses_greatest_fixed_point,
     [setup(gfp_test_install_bool_declarations(
                [gfp_self_bool-['Atom']])),
      cleanup(gfp_test_clear_declarations([gfp_self_bool/1]))]) :-
    Sources = [
        clause_source(
            gfp_self_bool, 1,
            [=, [gfp_self_bool, X],
                [if, [==, X, base], false, [gfp_self_bool, X]]])
    ],
    gfp_test_solve(Sources, Summaries, Results),
    gfp_test_summary_has_fact(
        gfp_self_bool/1, Summaries, proper_bool),
    summary_for_key(
        gfp_self_bool/1, Summaries,
        function_summary(_, _, card(0,many), [proper_bool],
                         [opaque,pure], [])),
    gfp_test_retained_analysis_has_fact(
        gfp_self_bool/1, Results, proper_bool),
    var(X).

test(recursive_mutual_bool_uses_greatest_fixed_point,
     [setup(gfp_test_install_bool_declarations(
                [gfp_mutual_a-['Atom'], gfp_mutual_b-['Atom']])),
      cleanup(gfp_test_clear_declarations(
                  [gfp_mutual_a/1, gfp_mutual_b/1]))]) :-
    Sources = [
        clause_source(
            gfp_mutual_a, 1,
            [=, [gfp_mutual_a, A],
                [if, [==, A, base], true, [gfp_mutual_b, A]]]),
        clause_source(
            gfp_mutual_b, 1,
            [=, [gfp_mutual_b, B],
                [if, [==, B, base], false, [gfp_mutual_a, B]]])
    ],
    gfp_test_solve(Sources, Summaries, Results),
    forall(member(Key, [gfp_mutual_a/1, gfp_mutual_b/1]),
           ( gfp_test_summary_has_fact(Key, Summaries, proper_bool),
             gfp_test_retained_analysis_has_fact(
                 Key, Results, proper_bool) )),
    var(A), var(B).

test(recursive_bool_candidate_removed_by_unbound_base,
     [setup(gfp_test_install_bool_declarations(
                [gfp_unbound_base-['Bool', 'Atom']])),
      cleanup(gfp_test_clear_declarations([gfp_unbound_base/2]))]) :-
    Sources = [
        clause_source(
            gfp_unbound_base, 2,
            [=, [gfp_unbound_base, Value, Flag],
                [if, [==, Flag, base], Value,
                     [gfp_unbound_base, Value, Flag]]])
    ],
    gfp_test_solve(Sources, Summaries, _),
    gfp_test_summary_lacks_fact(
        gfp_unbound_base/2, Summaries, proper_bool),
    var(Value), var(Flag).

test(recursive_bool_candidate_removed_by_unsupported_clause,
     [setup(gfp_test_install_bool_declarations(
                [gfp_unsupported_bool-['Atom']])),
      cleanup(gfp_test_clear_declarations([gfp_unsupported_bool/1]))]) :-
    Sources = [
        clause_source(
            gfp_unsupported_bool, 1,
            [=, [gfp_unsupported_bool, X],
                [if, [==, X, base], true,
                     [gfp_unsupported_bool, X]]]),
        clause_source(
            gfp_unsupported_bool, 1,
            [=, [gfp_unsupported_bool, Y], [case, Y, [bad]]])
    ],
    gfp_test_solve(Sources, Summaries, _),
    gfp_test_summary_lacks_fact(
        gfp_unsupported_bool/1, Summaries, proper_bool),
    summary_for_key(
        gfp_unsupported_bool/1, Summaries,
        function_summary(_, _, _, _, [opaque], Diagnostics)),
    assertion(Diagnostics = [unsupported_clauses(_)]),
    var(X), var(Y).

test(recursive_bool_multiclause_requires_every_clause,
     [setup(gfp_test_install_bool_declarations(
                [gfp_multiclause_bool-['Bool']])),
      cleanup(gfp_test_clear_declarations([gfp_multiclause_bool/1]))]) :-
    Sources = [
        clause_source(
            gfp_multiclause_bool, 1,
            [=, [gfp_multiclause_bool, X],
                [if, [==, X, true], true,
                     [gfp_multiclause_bool, X]]]),
        clause_source(
            gfp_multiclause_bool, 1,
            [=, [gfp_multiclause_bool, Y], Y])
    ],
    gfp_test_solve(Sources, Summaries, _),
    gfp_test_summary_lacks_fact(
        gfp_multiclause_bool/1, Summaries, proper_bool),
    var(X), var(Y).

test(recursive_bool_candidates_are_removed_independently,
     [setup(gfp_test_install_bool_declarations(
                [gfp_mixed_bad-['Bool', 'Atom'],
                 gfp_mixed_good-['Bool', 'Atom']])),
      cleanup(gfp_test_clear_declarations(
                  [gfp_mixed_bad/2, gfp_mixed_good/2]))]) :-
    Sources = [
        clause_source(
            gfp_mixed_bad, 2,
            [=, [gfp_mixed_bad, ValueA, FlagA],
                [if, [==, FlagA, base], ValueA,
                     [gfp_mixed_good, ValueA, FlagA]]]),
        clause_source(
            gfp_mixed_good, 2,
            [=, [gfp_mixed_good, ValueB, FlagB],
                [if, [gfp_mixed_bad, ValueB, FlagB], true, false]])
    ],
    gfp_test_solve(Sources, Summaries, Results),
    gfp_test_summary_lacks_fact(
        gfp_mixed_bad/2, Summaries, proper_bool),
    gfp_test_summary_has_fact(
        gfp_mixed_good/2, Summaries, proper_bool),
    gfp_test_retained_analysis_has_fact(
        gfp_mixed_good/2, Results, proper_bool),
    var(ValueA), var(FlagA), var(ValueB), var(FlagB).

test(recursive_bool_candidate_removal_is_transitive,
     [setup(gfp_test_install_bool_declarations(
                [gfp_transitive_bad-['Bool', 'Atom'],
                 gfp_transitive_forward-['Bool', 'Atom']])),
      cleanup(gfp_test_clear_declarations(
                  [gfp_transitive_bad/2, gfp_transitive_forward/2]))]) :-
    Sources = [
        clause_source(
            gfp_transitive_bad, 2,
            [=, [gfp_transitive_bad, ValueA, FlagA],
                [if, [==, FlagA, base], ValueA,
                     [gfp_transitive_forward, ValueA, FlagA]]]),
        clause_source(
            gfp_transitive_forward, 2,
            [=, [gfp_transitive_forward, ValueB, FlagB],
                [gfp_transitive_bad, ValueB, FlagB]])
    ],
    gfp_test_solve(Sources, Summaries, _),
    forall(member(Key,
                  [gfp_transitive_bad/2, gfp_transitive_forward/2]),
           gfp_test_summary_lacks_fact(Key, Summaries, proper_bool)),
    var(ValueA), var(FlagA), var(ValueB), var(FlagB).

gfp_test_install_bool_declarations([]).
gfp_test_install_bool_declarations([F-ArgTypes|Declarations]) :-
    length(ArgTypes, N),
    assertz(user:fn_decl(
                F, N, scheme(ArgTypes, 'Bool'), effect_model(det, []),
                test(unified_checker_gfp),
                provenance(test(unified_checker_gfp), syntax(test)))),
    gfp_test_install_bool_declarations(Declarations).

gfp_test_clear_declarations([]).
gfp_test_clear_declarations([F/N|Keys]) :-
    retractall(user:fn_decl(
                   F, N, _, _, test(unified_checker_gfp), _)),
    gfp_test_clear_declarations(Keys).

gfp_test_solve(Sources, Summaries, Results) :-
    prepare_clause_universe(Sources, Prepared),
    source_keys(Sources, Keys),
    initial_touched_summaries(Keys, Initial),
    solve_fixed_point(Prepared, Keys, Initial, Summaries, Results).

gfp_test_summary_has_fact(F/N, Summaries, Fact) :-
    summary_for_key(
        F/N, Summaries, function_summary(F, N, _, Facts, _, _)),
    assertion(memberchk(Fact, Facts)).

gfp_test_summary_lacks_fact(F/N, Summaries, Fact) :-
    summary_for_key(
        F/N, Summaries, function_summary(F, N, _, Facts, _, _)),
    assertion(\+ memberchk(Fact, Facts)).

gfp_test_retained_analysis_has_fact(F/N, Results, Fact) :-
    once(member(clause_result(
                    F, N, _, analyzed(_, _, _, Analysis)), Results)),
    assertion(analysis_result_has_fact(Analysis, Fact)).

test(summary_delta_is_set_normalized_and_ignores_diagnostics) :-
    Left = [function_summary(
                normalized_summary, 0, card(1,1),
                [proper_bool, type('Bool')],
                [pure, call(helper/0)], [initial])],
    Right = [function_summary(
                 normalized_summary, 0, card(1,1),
                 [type('Bool'), proper_bool],
                 [call(helper/0), pure], [different_diagnostic])],
    summary_resolver_view_unchanged(
        normalized_summary/0, Left, Right).

test(retained_results_project_distinct_variant_occurrences,
     [setup(assertz((user:fn_decl_arity(
                         solver_identity, 1, ['Atom'], 'Bool')))),
      cleanup(retractall(user:fn_decl_arity(
                             solver_identity, _, _, _)))]) :-
    First = [=, [solver_identity, X], true],
    Second = [=, [solver_identity, Y], true],
    Sources = [clause_source(solver_identity, 1, First),
               clause_source(solver_identity, 1, Second)],
    prepare_clause_universe(Sources, Prepared),
    source_keys(Sources, Keys),
    initial_touched_summaries(Keys, Initial),
    solve_fixed_point_counted(
        Prepared, Keys, Initial, _, Results, Count),
    assertion(Count == 2),
    clause_records_from_results(Sources, Results, [FirstRecord, SecondRecord]),
    FirstRecord = clause_record(StoredFirst, _, FirstEnv, _, _),
    SecondRecord = clause_record(StoredSecond, _, SecondEnv, _, _),
    assertion(StoredFirst == First),
    assertion(StoredSecond == Second),
    member(binding(_, FirstVar), FirstEnv), FirstVar == X,
    member(binding(_, SecondVar), SecondEnv), SecondVar == Y,
    assertion(FirstVar \== SecondVar).

test(branch_projection_does_not_bind_parametric_candidate,
     [cleanup(del_attrs(Value))]) :-
    put_attr(Value, tknown, [OpenType]),
    apply_variable_types(['Goal'], Value),
    var(OpenType),
    get_attr(Value, tknown, ['Goal']).

test(branch_snapshot_restores_all_source_variable_attrs,
     [cleanup((del_attrs(Matched), del_attrs(Derived)))]) :-
    put_attr(Matched, tknown, [outer_type]),
    Env = [binding(id(1), Matched), binding(id(2), Derived)],
    snapshot_environment_attrs(Env, OriginalAttrs),
    copy_term(OriginalAttrs, BranchAttrs),
    install_environment_attrs(Env, BranchAttrs),
    put_attr(Matched, tknown, [edge_type]),
    put_attr(Derived, tknown, [derived_type]),
    install_environment_attrs(Env, OriginalAttrs),
    get_attr(Matched, tknown, [outer_type]),
    \+ get_attr(Derived, tknown, _).

test(branch_snapshot_preserves_external_open_type_identity,
     [cleanup(del_attrs(Value))]) :-
    put_attr(Value, tknown, [OpenType]),
    Env = [binding(id(1), Value)],
    snapshot_environment_attrs(Env, OriginalAttrs),
    copy_term(OriginalAttrs, BranchAttrs),
    install_environment_attrs(Env, BranchAttrs),
    get_attr(Value, tknown, [BranchOpen]),
    BranchOpen = 'TemporaryType',
    var(OpenType),
    install_environment_attrs(Env, OriginalAttrs),
    get_attr(Value, tknown, [Restored]),
    Restored == OpenType.

test(reachable_branch_preserves_inference_requirement) :-
    catch(b_getval('$assumptions', Saved0), _, Saved0 = []),
    setup_call_cleanup(
        b_setval('$assumptions', [a(Value, OpenType)]),
        ( branch_inference_updates(
              [binding(id(1), Value)], [],
              [attrs(some([OpenType]), none, none)],
              [attrs(some(['Number']), none, none)], Updates),
          apply_inference_updates(Updates),
          assertion(OpenType == 'Number') ),
        b_setval('$assumptions', Saved0)).

test(edge_type_is_not_promoted_to_inference_requirement) :-
    catch(b_getval('$assumptions', Saved0), _, Saved0 = []),
    setup_call_cleanup(
        b_setval('$assumptions', [a(Value, OpenType)]),
        ( branch_inference_updates(
              [binding(id(1), Value)], [typed(Value, ['Number'])],
              [attrs(some([OpenType]), none, none)],
              [attrs(some(['Number']), none, none)], Updates),
          assertion(Updates == []),
          assertion(var(OpenType)) ),
        b_setval('$assumptions', Saved0)).

test(variant_signature_candidates_collapse_to_one) :-
    Candidates0 = [[A]-result(A), [B]-result(B)],
    variant_dedup(Candidates0, Candidates),
    Candidates = [[Type]-result(Type)].

test(incompatible_signature_candidates_remain_ambiguous) :-
    Candidates0 = [['Number']-'Number', ['Atom']-'Atom'],
    variant_dedup(Candidates0, Candidates),
    Candidates = [_, _].

test(declared_arg_resolution_is_arity_qualified,
     [setup((assertz((user:fn_decl_arity(
                          bridge_arity_probe, 1,
                          ['Number'], 'Number'))),
             assertz((user:fn_decl_arity(
                          bridge_arity_probe, 2,
                          ['Atom', 'String'], 'Bool'))))),
      cleanup(retractall(user:fn_decl_arity(
                             bridge_arity_probe, _, _, _)))]) :-
    bridge_resolve_type(declared_arg(bridge_arity_probe, 2, 0), 'Atom'),
    bridge_resolve_type(declared_arg(bridge_arity_probe, 2, 1), 'String'),
    \+ bridge_resolve_type(declared_arg(bridge_arity_probe, 1, 1), _).

test(constructor_resolver_collapses_variant_type_signatures,
     [setup((assertz((user:fn_decl_arity(
                          bridge_duplicate_ctor, 1,
                          [A], result(A)))),
             assertz((user:fn_decl_arity(
                          bridge_duplicate_ctor, 1,
                          [B], result(B)))))),
      cleanup(retractall(user:fn_decl_arity(
                             bridge_duplicate_ctor, _, _, _)))]) :-
    bridge_constructor_signature(
        bridge_duplicate_ctor, 1, [Type], result(Type)).

test(constructor_resolver_refuses_genuine_same_arity_ambiguity,
     [setup((assertz((user:fn_decl_arity(
                          bridge_ambiguous_ctor, 1,
                          ['Number'], number_result))),
             assertz((user:fn_decl_arity(
                          bridge_ambiguous_ctor, 1,
                          ['Atom'], atom_result))))),
      cleanup(retractall(user:fn_decl_arity(
                             bridge_ambiguous_ctor, _, _, _)))]) :-
    \+ bridge_constructor_signature(
           bridge_ambiguous_ctor, 1, _, _).

test(semantic_mutation_clears_transitive_summary_cache,
     [setup((unified_summary_cache_reset,
             unified_summary_cache_store_many([cache_entry(
                 function_summary(test_callee, 0, card(1,1),
                                  [proper_bool], [], []), [])]),
             unified_summary_cache_store_many([cache_entry(
                 function_summary(test_caller, 0, card(1,1),
                                  [proper_bool], [], []),
                 [summary(test_callee/0)])]))),
      cleanup(unified_summary_cache_reset)]) :-
    unified_checker_invalidate_event(clause_changed(test_callee/0, runtime)),
    \+ unified_checker_bridge:unified_function_summary(
           test_callee, 0, _, _, _, _),
    \+ unified_checker_bridge:unified_function_summary(
           test_caller, 0, _, _, _, _).

test(scoped_summary_shadows_persistent_facts,
     [setup((unified_summary_cache_reset,
             unified_summary_cache_store_many([cache_entry(
                 function_summary(shadowed, 0, card(1,1),
                                  [proper_bool], [], []), [])]))),
      cleanup(unified_summary_cache_reset)]) :-
    current_bridge_generation(Generation),
    Record = clause_record(scope_test, scope_ir, [], [], scope_analysis),
    with_bridge_scope(scope(Generation,
                  [function_summary(shadowed, 0, card(1,1), [], [], [])],
                  [Record]),
        with_current_clause(Record,
            \+ unified_function_result_fact(shadowed, 0, proper_bool))).

test(runtime_mutation_invalidates_active_scope) :-
    current_bridge_generation(Generation),
    Record = clause_record(scope_test, scope_ir, [], [], scope_analysis),
    with_bridge_scope(scope(Generation,
                  [function_summary(producer, 0, card(1,1),
                                    [proper_bool], [], [])],
                  [Record]),
        with_current_clause(Record,
            ( unified_function_result_fact(producer, 0, proper_bool),
              unified_checker_invalidate_event(
                  clause_changed(producer/0, runtime)),
              \+ unified_function_result_fact(producer, 0, proper_bool),
              \+ bridge_scope(_) ))).

test(source_form_mutation_keeps_active_scope) :-
    current_bridge_generation(Generation),
    Record = clause_record(scope_test, scope_ir, [], [], scope_analysis),
    with_bridge_scope(scope(Generation,
                  [function_summary(producer, 0, card(1,1),
                                    [proper_bool], [], [])],
                  [Record]),
        with_current_clause(Record,
            ( with_unified_preanalyzed_form(
                  unified_checker_invalidate_event(
                      declaration_changed(value, token, added))),
              unified_function_result_fact(producer, 0, proper_bool),
              bridge_scope(_) ))).

:- end_tests(unified_checker_bridge).
