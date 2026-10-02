:- begin_tests(specializer_recursion).

:- ensure_loaded('../src/metta.pl').

quiet :- retractall(silent(_)), assertz(silent(true)).

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
    \+ maybe_specialize_call(sr_outer, [sr_broken, 1], _, _),
    findall(F-S, ho_specialization(F, S), After),
    Before == After,
    ho_specialization_failed(sr_outer, 3, [sr_broken]),
    \+ nb_current('sr_inner_Spec_[sr_broken]', _),
    \+ translated_from(_, [=, ['sr_inner_Spec_[sr_broken]'|_], _]),
    findall(R, sr_inner(sr_broken, 1, R), [[pair, done, 0]]),
    once(call(Goal)).

% A translator extension can throw after a mutually recursive child was built.
sr_throw(_, _) :- throw(specializer_test_exception).

test(exception_discards_generated_code_and_restores_the_stack, [setup(quiet)]) :-
    register_fun(sr_throw),
    assertz(translator_rule(sr_throw)),
    process_metta_string(
        "(= (sr_exc_outer $f $x) (pair (sr_exc_inner $f $x) ($f $x)))\n\c
         (= (sr_exc_inner $f $x) (if (== $x 0) done (sr_exc_outer $f 0)))", _),
    findall(F-S, ho_specialization(F, S), Before),
    catch(maybe_specialize_call(sr_exc_outer, [sr_throw, 1], _, _), Error, true),
    Error == specializer_test_exception,
    findall(F-S, ho_specialization(F, S), After),
    Before == After,
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
    findall(Ref, translated_from(Ref, _), SourcesBefore),
    \+ maybe_specialize_call(sr_lambda_outer, [sr_lambda_broken, 1], _, _),
    findall(F, fun(F), FunctionsAfter),
    findall(Ref, translated_from(Ref, _), SourcesAfter),
    FunctionsBefore == FunctionsAfter,
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
    \+ maybe_specialize_call(sr_back_outer, [sr_back_broken, 1], _, _),
    findall(F, fun(F), After),
    Before == After,
    nb_getval(lambda_counter, Count),
    format(atom(Last), 'lambda_~d', [Count]),
    \+ nb_current(Last, _),
    functor(Head, Last, 2),
    \+ clause(Head, _, _).

:- end_tests(specializer_recursion).
