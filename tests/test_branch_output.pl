:- ensure_loaded('../src/metta').
:- use_module(library(plunit)).
:- set_test_options([load(always)]).

:- begin_tests(branch_output).

test(translation_keeps_branch_values_distinct_and_joins_their_types) :-
    user:add_known_type(Value, 'Number'),
    user:add_known_type(Out, 'String'),
    user:build_branch(Value=1, Value, Out, Goal),
    assertion(Value \== Out),
    assertion(user:known_candidates(Value, ['Number'])),
    user:known_candidates(Out, Candidates),
    assertion(memberchk('Number', Candidates)),
    assertion(memberchk('String', Candidates)),
    call(Goal),
    assertion(Value == 1), assertion(Out == 1).

test(an_unknown_branch_does_not_inherit_another_branchs_type) :-
    user:add_known_type(Out, 'Number'),
    user:build_branch(Value=unknown, Value, Out, _),
    assertion(Value \== Out),
    assertion(\+ user:known_candidates(Value, _)),
    user:known_candidates(Out, Candidates),
    user:unknown_marker(Unknown),
    assertion(memberchk('Number', Candidates)),
    assertion(memberchk(Unknown, Candidates)).

test(a_prebound_result_is_visible_to_the_selected_body) :-
    user:build_branch((nonvar(Value)->Seen=bound;Seen=unbound), Value, Out, Goal),
    assertion(Value \== Out),
    Out=42, call(Goal),
    assertion(Value == 42), assertion(Seen == bound).

:- end_tests(branch_output).
