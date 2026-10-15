:- ensure_loaded('../src/metta').

:- begin_tests(type_guards).

% Exercise emitted goals directly: these values cross an unresolved caller
%boundary, so neither the optimizer nor static literal checking can hide it.
test(primitive_tuple, [forall(member(V, [[stv, 2, 3], [stv, 0.5, -1]]))]) :-
    guard_goal(V, [stv, 'Number', 'Number'], Goal),
    call(Goal).

test(nested_mixed_primitives) :-
    guard_goal([record, [stv, 2, 3], "yes", true],
               [record, [stv, 'Number', 'Number'], 'String', 'Bool'], Goal),
    call(Goal).

test(scalar_fast_paths, [forall(member(T-Test, ['Number'-number(V),
                                              'String'-string(V),
                                              'Bool'-(V == true ; V == false)]))]) :-
    guard_goal(V, T, Goal),
    Goal = (Test -> true ; typecheck_or_error(V, T)).

test(tuple_error_preserved,
     [forall(member(V, [[stv, x, 3], [stv, 2], [wrong, 2, 3], [stv, 2, 3, 4]])),
      throws(error(literal_type_mismatch(V, [stv, 'Number', 'Number']), typecheck))]) :-
    guard_goal(V, [stv, 'Number', 'Number'], Goal),
    call(Goal).

test(nested_error_preserved,
     [forall(member(V, [[record, [stv, x, 3], "yes", true],
                       [record, [stv, 2, 3], text, true],
                       [record, [stv, 2, 3], "yes", 1]])),
      throws(error(literal_type_mismatch(V, T), typecheck))]) :-
    T = [record, [stv, 'Number', 'Number'], 'String', 'Bool'],
    guard_goal(V, T, Goal),
    call(Goal).

test(variable_tag_not_instantiated,
     [throws(error(literal_type_mismatch([_, 2, 3], _), typecheck))]) :-
    guard_goal([_Tag, 2, 3], [stv, 'Number', 'Number'], Goal),
    call(Goal).

test(open_tail_not_instantiated,
     [throws(error(literal_type_mismatch([stv, 2, 3|_], _), typecheck))]) :-
    guard_goal([stv, 2, 3|_Tail], [stv, 'Number', 'Number'], Goal),
    call(Goal).

test(unbound_value_stays_constrained_variable) :-
    T = [stv, 'Number', 'Number'],
    guard_goal(V, T, Goal),
    call(Goal),
    var(V),
    get_attr(V, mreq, [T]).

test(partial_field_uses_generic_semantics) :-
    V = [stv, X, 3], T = [stv, 'Number', 'Number'],
    guard_goal(V, T, Goal),
    call(Goal),
    var(X),
    get_attr(X, mreq, ['Number']).

test(attributed_value_is_not_matched_to_a_tuple) :-
    constrain_var_type(V, 'Number'),
    guard_goal(V, [stv, 'Number', 'Number'], Goal),
    call(Goal),
    var(V),
    get_attr(V, mreq, Types),
    memberchk([stv, 'Number', 'Number'], Types),
    memberchk('Number', Types).

test(nested_attributed_variable_is_not_instantiated) :-
    constrain_var_type(X, 'Number'),
    guard_goal([record, X], [record, [stv, 'Number', 'Number']], Goal),
    call(Goal),
    var(X),
    get_attr(X, mreq, Types),
    memberchk([stv, 'Number', 'Number'], Types).

test(attributed_open_tail_keeps_whole_value_error,
     [throws(error(literal_type_mismatch([stv, 2|_], [stv, 'Number', 'Number']), typecheck))]) :-
    constrain_var_type(Tail, 'Number'),
    guard_goal([stv, 2|Tail], [stv, 'Number', 'Number'], Goal),
    call(Goal).

test(nonprimitive_field_keeps_generic_guard) :-
    guard_goal(V, [pair, 'A', '%Undefined%'], Goal),
    Goal == typecheck_or_error(V, [pair, 'A', '%Undefined%']).

:- end_tests(type_guards).
