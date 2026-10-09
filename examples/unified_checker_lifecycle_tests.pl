:- module(unified_checker_lifecycle_tests, []).

/** <module> Integration regressions for unified-summary lifecycle boundaries

These tests run with the complete PeTTa runtime loaded.  They deliberately
inspect the closed summary cache and source-occurrence stores: a surface
MeTTa result alone cannot distinguish a reused fresh summary from a stale one,
or prove that a failed runtime transaction restored every exact occurrence.
*/

:- use_module(library(plunit)).

:- dynamic lifecycle_test_directory/1.
:- prolog_load_context(directory, Directory),
   assertz(lifecycle_test_directory(Directory)).

lifecycle_example_path(Name, Path) :-
    lifecycle_test_directory(Directory),
    directory_file_path(Directory, Name, Path).

run_removed_pending_lifecycle(ParsedForms, Source) :-
    unified_checker_bridge:with_unified_file_analysis(
        ParsedForms,
        unified_checker_lifecycle_tests:stage_and_remove_pending_source(
            Source)).

stage_and_remove_pending_source(Source) :-
    Source = [Eq, [F|Args], Body],
    Eq == (=),
    RawTerm =.. ['&self', Eq, [F|Args], Body],
    assertz(user:RawTerm, RawRef),
    length(Args, N),
    PredicateArity is N + 1,
    functor(Head, F, PredicateArity),
    assertz(user:(Head :- fail), ClauseRef),
    assertz(user:translated_from(ClauseRef, Source)),
    retractall(user:translated_from(ClauseRef, _)),
    erase(ClauseRef),
    erase(RawRef),
    unified_checker_bridge:unified_checker_invalidate_event(
        clause_changed(F/N, runtime)).


:- begin_tests(unified_checker_lifecycle).

test(nested_file_analysis_publishes_refreshed_outer_summary,
     [ setup(unified_checker_cache:unified_summary_cache_reset),
       cleanup(unified_checker_cache:unified_summary_cache_reset) ]) :-
    lifecycle_example_path(
        'strictdet_unified_cross_file_cache.metta', SourceFile),
    user:load_metta_file(SourceFile, Results),
    assertion(Results == [true, true]),
    unified_checker_cache:unified_summary_cache_lookup(
        'unified-cache-bool-consumer', 1,
        function_summary('unified-cache-bool-consumer', 1,
                         card(1,1), Facts, Effects, _),
        Dependencies),
    % The outer solve runs before its nested import.  Publishing that original
    % solve would omit the imported callee's proper-Bool result contract.
    assertion(memberchk(proper_bool, Facts)),
    assertion(Effects == [pure]),
    assertion(memberchk(
                  summary('unified-cache-bool-case'/1), Dependencies)).


test(removed_outer_pending_definition_is_not_cached,
     [ setup(unified_checker_cache:unified_summary_cache_reset),
       cleanup(unified_checker_cache:unified_summary_cache_reset) ]) :-
    user:sread(
        '(= (ucl-removed-pending $value) (case $value ((true true) (false false))))',
        RemovedSource),
    ParsedForms = [parsed(function, '<lifecycle-test>', 1, RemovedSource)],
    run_removed_pending_lifecycle(ParsedForms, RemovedSource),
    source_occurrence_counts(
        RemovedSource, original_counts(Translated, Raw)),
    assertion(Translated == 0),
    assertion(Raw == 0),
    assertion(\+ unified_checker_cache:unified_summary_cache_lookup(
                     'ucl-removed-pending', 1, _, _)).


test(late_prolog_callable_recompiles_data_consumer,
     [ setup(unified_checker_cache:unified_summary_cache_reset),
       cleanup(unified_checker_cache:unified_summary_cache_reset) ]) :-
    late_prolog_callable_fixture_source(Source),
    user:process_metta_string(Source, Results),
    assertion(Results == []),
    assertion(\+ user:fun('ucl-late-prolog')),
    user:sread(
        '(= (ucl-late-consumer $value) (ucl-late-prolog $value))',
        ConsumerSource),
    once(( user:translated_from(OriginalRef, StoredConsumer),
           StoredConsumer =@= ConsumerSource )),
    findall(Result,
            user:'ucl-late-consumer'(true, Result),
            BeforeRegistration),
    assertion(BeforeRegistration == [['ucl-late-prolog', true]]),
    user:compiled_function_dependencies(
        'ucl-late-consumer', InitialDependencies),
    assertion(memberchk(
                  late_call('ucl-late-prolog'/1), InitialDependencies)),
    setup_call_cleanup(
        assertz(user:'ucl-late-prolog'(true, true), ImplementationRef),
        ( user:import_prolog_function('ucl-late-prolog', Imported),
          assertion(Imported == true),
          assertion(user:fun('ucl-late-prolog')),
          assertion(\+ clause(_, _, OriginalRef)),
          findall(Result,
                  user:'ucl-late-consumer'(true, Result),
                  AfterRegistration),
          assertion(AfterRegistration == [true]) ),
        cleanup_late_prolog_callable(ImplementationRef)).


test(runtime_add_rollback_restores_post_swap_state,
     [ setup(unified_checker_cache:unified_summary_cache_reset),
       cleanup(unified_checker_cache:unified_summary_cache_reset) ]) :-
    rollback_fixture_source(Source),
    user:process_metta_string(Source, Results),
    assertion(Results == []),
    user:sread('(= (ucl-swap-producer true) true)', OriginalSource),
    user:sread(
        '(= (ucl-swap-producer ucl-swap-bogus) ucl-swap-bogus)',
        AddedSource),
    once(( user:translated_from(OriginalRef, StoredOriginal),
           StoredOriginal =@= OriginalSource )),
    source_occurrence_counts(
        OriginalSource, original_counts(OriginalTranslated0, OriginalRaw0)),
    source_occurrence_counts(
        AddedSource, original_counts(AddedTranslated0, AddedRaw0)),
    assertion(OriginalTranslated0 == 1),
    assertion(OriginalRaw0 == 1),
    assertion(AddedTranslated0 == 0),
    assertion(AddedRaw0 == 0),
    cache_row('ucl-swap-producer', 1, ProducerCache0),
    cache_row('ucl-swap-consumer', 1, ConsumerCache0),
    unified_checker_cache:unified_summary_cache_store(
        function_summary('ucl-swap-sentinel', 0, card(1,1),
                         [proper_bool], [], []), []),
    cache_row('ucl-swap-sentinel', 0, SentinelCache0),

    runtime_add_outcome(AddedSource, Outcome),
    assertion(Outcome \== succeeded),

    % notify_mutation/1 rebuilt the producer before its dependent rejected the
    % new result shape.  The saved ClauseRef is therefore stale here; this
    % assertion ensures the test really crosses that post-swap rollback seam.
    assertion(\+ clause(_, _, OriginalRef)),
    source_occurrence_counts(
        OriginalSource, original_counts(OriginalTranslated1, OriginalRaw1)),
    source_occurrence_counts(
        AddedSource, original_counts(AddedTranslated1, AddedRaw1)),
    assertion(OriginalTranslated1 == OriginalTranslated0),
    assertion(OriginalRaw1 == OriginalRaw0),
    assertion(AddedTranslated1 == AddedTranslated0),
    assertion(AddedRaw1 == AddedRaw0),
    cache_row('ucl-swap-producer', 1, ProducerCache1),
    cache_row('ucl-swap-consumer', 1, ConsumerCache1),
    cache_row('ucl-swap-sentinel', 0, SentinelCache1),
    assertion(ProducerCache1 == ProducerCache0),
    assertion(ConsumerCache1 == ConsumerCache0),
    assertion(SentinelCache1 == SentinelCache0),
    findall(Result, user:'ucl-swap-producer'(true, Result), TrueResults),
    findall(Result,
            user:'ucl-swap-producer'('ucl-swap-bogus', Result),
            BogusResults),
    assertion(TrueResults == [true]),
    assertion(BogusResults == []).


rollback_fixture_source(
"(: ucl-swap-bogus Bool)
(: ucl-swap-producer (-[semidet]-> Bool Bool))
(= (ucl-swap-producer true) true)
(: ucl-swap-consumer (-[semidet]-> Bool Bool))
(= (ucl-swap-consumer $value)
   (and (ucl-swap-producer $value) true))
").

late_prolog_callable_fixture_source(
"(: ucl-late-prolog (-[det]-> Bool Bool))
(: ucl-late-consumer (-[det]-> Bool Expression))
(= (ucl-late-consumer $value) (ucl-late-prolog $value))
").

cleanup_late_prolog_callable(ImplementationRef) :-
    ignore(catch(erase(ImplementationRef), _, fail)),
    retractall(user:fun('ucl-late-prolog')).

runtime_add_outcome(Source, Outcome) :-
    ( catch(user:'add-atom'('&self', Source, true), Error,
            Outcome = threw(Error))
      -> ( var(Outcome) -> Outcome = succeeded ; true )
    ; Outcome = failed ).

cache_row(F, N, Summary-Dependencies) :-
    unified_checker_cache:unified_summary_cache_lookup(
        F, N, Summary, Dependencies).

source_occurrence_counts(Source, original_counts(Translated, Raw)) :-
    findall(Ref,
            ( user:translated_from(Ref, Stored), Stored =@= Source ),
            TranslatedRefs),
    length(TranslatedRefs, Translated),
    raw_source_variant_refs(Source, RawRefs),
    length(RawRefs, Raw).

raw_source_variant_refs(Source, Refs) :-
    Source = [_|Args],
    length(Args, ArgumentCount),
    Arity is ArgumentCount + 1,
    findall(Ref,
            ( functor(Head, '&self', Arity),
              clause(user:Head, true, Ref),
              Head =.. ['&self', Relation|StoredArgs],
              Stored = [Relation|StoredArgs],
              Stored =@= Source ),
            Refs).

:- end_tests(unified_checker_lifecycle).
