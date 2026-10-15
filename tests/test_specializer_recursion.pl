:- ensure_loaded('../src/metta.pl').

% Translator extensions run in the runtime module, as do builtin registry hooks.
sr_throw(_, _) :- throw(specializer_test_exception).

:- begin_tests(specializer_recursion).

% Rollback must remove main's declaration and dependency records as well as code.
compiler_records(records(Declarations, Dependencies, Edges)) :-
    findall(fn(F,N,S,E,O,P), fn_decl(F,N,S,E,O,P), Declarations),
    findall(dep(R,F,O,D), compiled_deps(R,F,O,D), Dependencies),
    findall(edge(D,R,F), compiled_dep_edge(D,R,F), Edges).

records_match(Before, After) :- Before =@= After.

quiet :- retractall(user:silent(_)), assertz(user:silent(true)).

% A non-arrow declaration no longer rejects calls on current main. Inject a
% translation failure explicitly, leaving the original runtime clauses intact.
with_rejected_translation(F, Goal) :-
    Head =.. [F, _, _],
    setup_call_cleanup(
        ( assertz(user:translator_rule(F), RuleRef),
          asserta(user:(Head :- throw(error(recursion_test_refused, typecheck))), Ref) ),
        Goal,
        ( erase(Ref), erase(RuleRef) ) ).

test(failed_cycle_preserves_an_earlier_specialization, [setup(quiet)]) :-
    process_metta_string(
        "(: sr_broken Bad)\n\c
         (= (sr_broken $x) $x)\n\c
         (= (sr_inc $x) (+ $x 1))\n\c
         (= (sr_apply $f $x) ($f $x))\n\c
         (= (sr_outer $f $x) (pair (sr_inner $f $x) ($f $x)))\n\c
         (= (sr_inner $f $x) (if (== $x 0) done (sr_outer $f 0)))", _),
    once(maybe_specialize_call(sr_apply, [sr_inc, 1], Value, Goal)),
    once(call(Goal)), Value == 2,
    findall(F-S, ho_specialization(F, S), Before),
    compiler_records(RecordsBefore),
    with_rejected_translation(sr_broken,
        \+ maybe_specialize_call(sr_outer, [sr_broken, 1], _, _)),
    findall(F-S, ho_specialization(F, S), After),
    Before == After,
    compiler_records(RecordsAfter), records_match(RecordsBefore, RecordsAfter),
    ho_specialization_failed(sr_outer, 3, [sr_broken]),
    \+ nb_current('sr_inner_Spec_[sr_broken]', _),
    \+ translated_from(_, [=, ['sr_inner_Spec_[sr_broken]'|_], _]),
    findall(R, sr_inner(sr_broken, 1, R), [[pair, done, 0]]),
    once(call(Goal)).

% A translator extension can throw after a mutually recursive child was built.
test(exception_discards_generated_code_and_restores_the_stack, [setup(quiet)]) :-
    register_fun(sr_throw),
    assertz(user:translator_rule(sr_throw)),
    process_metta_string(
        "(= (sr_exc_outer $f $x) (pair (sr_exc_inner $f $x) ($f $x)))\n\c
         (= (sr_exc_inner $f $x) (if (== $x 0) done (sr_exc_outer $f 0)))", _),
    findall(F-S, ho_specialization(F, S), Before),
    compiler_records(RecordsBefore),
    catch(maybe_specialize_call(sr_exc_outer, [sr_throw, 1], _, _), Error, true),
    Error == specializer_test_exception,
    findall(F-S, ho_specialization(F, S), After),
    Before == After,
    compiler_records(RecordsAfter), records_match(RecordsBefore, RecordsAfter),
    nb_getval('$spec_stack', []),
    \+ nb_current('$spec_created', _),
    \+ nb_current('sr_exc_inner_Spec_[sr_throw]', _),
    \+ ho_specialization_failed(sr_exc_outer, _, _).

test(failed_attempt_discards_its_anonymous_functions, [setup(quiet)]) :-
    process_metta_string(
        "(: sr_lambda_broken Bad)\n\c
         (= (sr_lambda_broken $x) $x)\n\c
         (= (sr_lambda_outer $f $x) (pair (|-> ($y) ($f $y)) ($f $x)))", _),
    findall(F, fun(F), FunctionsBefore),
    compiler_records(RecordsBefore),
    findall(Ref, translated_from(Ref, _), SourcesBefore),
    with_rejected_translation(sr_lambda_broken,
        \+ maybe_specialize_call(sr_lambda_outer, [sr_lambda_broken, 1], _, _)),
    findall(F, fun(F), FunctionsAfter),
    findall(Ref, translated_from(Ref, _), SourcesAfter),
    FunctionsBefore == FunctionsAfter,
    compiler_records(RecordsAfter), records_match(RecordsBefore, RecordsAfter),
    SourcesBefore == SourcesAfter,
    nb_getval(lambda_counter, Count),
    format(atom(Last), 'lambda_~d', [Count]),
    \+ nb_current(Last, _).

test(failed_attempt_discards_a_completed_recursive_lambda, [setup(quiet)]) :-
    process_metta_string(
        "(: sr_back_broken Bad)\n\c
         (= (sr_back_broken $x) $x)\n\c
         (= (sr_back_outer $f $x) (pair (|-> ($y) (sr_back_outer $f $y)) ($f $x)))", _),
    findall(F, fun(F), Before),
    compiler_records(RecordsBefore),
    with_rejected_translation(sr_back_broken,
        \+ maybe_specialize_call(sr_back_outer, [sr_back_broken, 1], _, _)),
    findall(F, fun(F), After),
    Before == After,
    compiler_records(RecordsAfter), records_match(RecordsBefore, RecordsAfter),
    nb_getval(lambda_counter, Count),
    format(atom(Last), 'lambda_~d', [Count]),
    \+ nb_current(Last, _),
    functor(Head, Last, 2),
    \+ clause(Head, _, _).

test(successful_mutual_cycle_reuses_the_pending_parent, [setup(quiet)]) :-
    process_metta_string(
        "(= (sr_cycle_inc $x) (+ $x 1))\n\c
         (= (sr_cycle_even $f $n) (if (== $n 0) 0 ($f (sr_cycle_odd $f (- $n 1)))))\n\c
         (= (sr_cycle_odd $f $n) (if (== $n 0) 0 ($f (sr_cycle_even $f (- $n 1)))))", _),
    once(maybe_specialize_call(sr_cycle_even, [sr_cycle_inc, 4], Value, Goal)),
    findall(Value, call(Goal), [4]),
    ho_specialization(sr_cycle_even, Even),
    ho_specialization(sr_cycle_odd, Odd),
    functor(EvenHead, Even, 3), clause(EvenHead, EvenBody),
    functor(OddHead, Odd, 3), clause(OddHead, OddBody),
    once((sub_term(ToOdd, EvenBody), compound(ToOdd), functor(ToOdd, Odd, 3))),
    once((sub_term(ToEven, OddBody), compound(ToEven), functor(ToEven, Even, 3))),
    nb_getval('$spec_stack', []),
    \+ nb_current('$spec_created', _).

:- end_tests(specializer_recursion).
