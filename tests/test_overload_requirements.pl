:- ensure_loaded('../src/metta').
:- use_module(library(plunit)).
:- set_test_options([load(always)]).

:- begin_tests(overload_requirements).

poly_guards(X, T, [typecheck_match(X, ['List', T]), typecheck_match(X, 'String')]).
number_string(X, [typecheck_match(X, 'Number'), typecheck_match(X, 'String')]).
requirements(X, N) :- get_attr(X, mreq, Rs), length(Rs, N).

repeat_private(0, _) :- !.
repeat_private(N, X) :-
    poly_guards(X, T, G), user:overload_keep(G, [X], private([T])),
    M is N-1, repeat_private(M, X).

private_calls(0, _, done) :- !.
private_calls(N, X, [let, _, ['requirement-probe',X], Rest]) :-
    M is N-1, private_calls(M, X, Rest).

test(compiled_calls_mark_declaration_variables_private) :-
    user:process_metta_string("(: requirement-probe (-> (List $a) %Undefined%))
                              (: requirement-probe (-> String %Undefined%))
                              (= (requirement-probe $x) ok)", _),
    private_calls(40, X, Expr), user:translate_expr(Expr, Goals, Out),
    user:goals_list_to_conj(Goals, Goal), once(user:call(Goal)),
    assertion(Out == done), requirements(X, 1), X = "ok".

test(private_schema_variables_do_not_accumulate) :-
    repeat_private(400, X), requirements(X, 1), X = [1, 2].

test(shared_type_variables_remain_distinct, [throws(error(_, typecheck))]) :-
    poly_guards(X, T, G1), poly_guards(X, U, G2),
    user:overload_keep(G1, [X]), user:overload_keep(G2, [X]), requirements(X, 2),
    T = 'Number', U = 'String', X = [1].

test(unlisted_type_variables_remain_distinct) :-
    poly_guards(X, T, G1), poly_guards(X, U, G2),
    user:overload_keep(G1, [X], private([])),
    user:overload_keep(G2, [X], private([])), requirements(X, 2),
    assertion(T \== U).

test(program_variable_contexts_remain_distinct) :-
    G1 = [(typecheck_match(X, 'Number'), typecheck_match(Y, 'Number')),
          (typecheck_match(X, 'String'), typecheck_match(Y, 'String'))],
    G2 = [(typecheck_match(X, 'Number'), typecheck_match(Z, 'Number')),
          (typecheck_match(X, 'String'), typecheck_match(Z, 'String'))],
    user:overload_keep(G1, [X,Y], private([])),
    user:overload_keep(G2, [X,Z], private([])), requirements(X, 2).

test(pending_output_states_are_never_reused) :-
    number_string(X, G),
    user:overload_enter(G, [X], S1), user:overload_enter(G, [X], S2),
    arg(1, S1, I1), arg(1, S2, I2), assertion(I1 \== I2),
    user:overload_keep(G, [X], private([])),
    user:overload_keep(G, [X], private([])), requirements(X, 3).

test(copied_private_requirements_are_independent) :-
    repeat_private(2, X), copy_term(X, Y), repeat_private(2, Y),
    requirements(X, 1), requirements(Y, 1), X = [1], Y = "s".

test(private_requirement_attachment_backtracks) :-
    ( repeat_private(2, X), fail ; true ), \+ get_attr(X, mreq, _).

test(attributed_type_parameters_are_not_renamed) :-
    freeze(T, true), freeze(U, true),
    poly_guards(X, T, G1), poly_guards(X, U, G2),
    user:overload_keep(G1, [X], private([T])),
    user:overload_keep(G2, [X], private([U])), requirements(X, 2).

:- end_tests(overload_requirements).
