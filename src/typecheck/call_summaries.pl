:- module(call_summaries,
          [ builtin_mode/6,
            mode_applicable/3,
            select_builtin_mode/6,
            post_fact/3,
            validate_summary_table/0
          ]).

/** <module> Declarative call summaries for the relational checker IR

This module describes builtin calls without consulting the legacy checker.
A row has the logical shape

  mode(F/N, Preconditions, Postconditions, Card, Effects, Priority)

where a precondition is `req(ArgIndex, Fact)` or a whitelisted
`guard(Constraint)`, and a postcondition is `ensure(result, Fact)` or
`ensure(arg(ArgIndex), Fact)`.  Argument indexes are zero based.  Guards run
left-to-right after preceding fact requirements have instantiated their
parameters.  Cardinalities are closed `card(Lower, Upper)` terms; the upper
bound is an integer or `many`.  A larger priority wins among applicable rows.

`select_builtin_mode/6` deliberately returns no answer for an unknown builtin.
Its fact callback is called as `call(HasFact, ArgIndex, Fact)`.  A requirement
whose fact contains variables (for example `literal_list(Values)`) is queried
by enumeration: the callback receives a variable fact and must return the
stored fact, which is then unified with the requirement.  This matches the
immutable domain's non-binding exact lookup while still allowing guarded
summaries to inspect a literal payload.
*/

:- meta_predicate mode_applicable(+, 2, -).
:- meta_predicate select_builtin_mode(+, +, 2, -, -, -).
:- discontiguous builtin_mode/6.

% Arithmetic.  The numeric preconditions capture the usable mode; arithmetic
% results are instantiated numbers when a call succeeds.
arithmetic_builtin('+').
arithmetic_builtin('-').
arithmetic_builtin('*').
arithmetic_builtin('/').
arithmetic_builtin('%').
arithmetic_builtin(min).
arithmetic_builtin(max).

builtin_mode(F/2,
             [req(0, number), req(1, number)],
             [ensure(result, type('Number')), ensure(result, number),
              ensure(result, ground), ensure(result, nonvar)],
             card(1, 1), [pure], 100) :-
    arithmetic_builtin(F).
builtin_mode(F/2, [], [], card(0, 1), [pure], 0) :-
    arithmetic_builtin(F).

% Numeric comparisons and reified equality always produce a proper Bool.
numeric_comparison('<').
numeric_comparison('<=').
numeric_comparison('>').
numeric_comparison('>=').

builtin_mode(F/2,
             [req(0, number), req(1, number)],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(1, 1), [pure], 100) :-
    numeric_comparison(F).
builtin_mode(F/2, [],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(0, 1), [pure], 0) :-
    numeric_comparison(F).

equality_builtin('=').
equality_builtin('==').
equality_builtin('!=').
equality_builtin('=?').
equality_builtin('=alpha').
equality_builtin('=@=').

builtin_mode(F/2, [],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(1, 1), [pure], 50) :-
    equality_builtin(F).

% Boolean predicates enumerate missing Bool inputs, but are exactly-one tests
% once every operand is already a proper Boolean.
binary_bool_builtin(and).
binary_bool_builtin(or).
binary_bool_builtin(xor).
binary_bool_builtin(implies).

builtin_mode(F/2,
             [req(0, proper_bool), req(1, proper_bool)],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(1, 1), [pure], 100) :-
    binary_bool_builtin(F).
builtin_mode(F/2, [],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(0, many), [pure], 0) :-
    binary_bool_builtin(F).

builtin_mode(not/1, [req(0, proper_bool)],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(1, 1), [pure], 100).
builtin_mode(not/1, [],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(0, many), [pure], 0).

% Reflection tests are total and always reify their answer as a proper Bool.
reflection_test('is-var').
reflection_test('is-ground').
reflection_test('is-expr').
reflection_test('is-space').

builtin_mode(F/1, [],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(1, 1), [pure], 50) :-
    reflection_test(F).

% is-member has an exactly-one guarded mode when the probe and list are ground
% and the list is duplicate-free.  `nonvar` is deliberately insufficient for
% the general rule: f(X) against [f(a),f(b)] has two true solutions even though
% its outer constructor is bound.
builtin_mode('is-member'/2,
             [req(0, ground), req(1, proper_list), req(1, ground),
              req(1, duplicate_free)],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(1, 1), [pure], 100).

% `literal_list(Values)` preserves an exact source-list snapshot rather than
% flattening it into unrelated shape facts.  It lets a summary reconstruct the
% ground/duplicate-free proof even when the caller has not materialised those
% derived facts separately.  A ground probe is safe for arbitrary ground
% elements.  A merely nonvar probe is safe only when every element is atomic:
% a nonvar compound cannot unify with an atom, while an atomic probe can match
% at most its one duplicate-free occurrence.
builtin_mode('is-member'/2,
             [req(0, ground), req(1, literal_list(Values)),
              guard(ground_duplicate_free_list(Values))],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(1, 1), [pure], 95).
% Empty and singleton lists are exactly-one tests in every probe mode: either
% their sole member solution succeeds, or the false fallback does.
builtin_mode('is-member'/2,
             [req(1, literal_list(Values)),
              guard(at_most_one_list(Values))],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(1, 1), [pure], 92).
builtin_mode('is-member'/2,
             [req(0, nonvar), req(1, literal_list(Values)),
              guard(ground_atomic_duplicate_free_list(Values))],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(1, 1), [pure], 90).
builtin_mode('is-member'/2, [req(1, proper_list)],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(1, many), [pure], 50).
builtin_mode('is-member'/2, [],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(0, many), [pure], 0).

% The implementation commits to the first alpha-equivalent member and has a
% mutually exclusive false fallback, so it is total for every input mode.
builtin_mode('is-alpha-member'/2, [],
             [ensure(result, type('Bool')), ensure(result, proper_bool),
              ensure(result, ground), ensure(result, nonvar)],
             card(1, 1), [pure], 50).

% decons succeeds once precisely for nonempty list-shaped expressions.  The
% result is the proper two-field `(head tail)` pair.
decons_builtin(decons).
decons_builtin('decons-atom').

builtin_mode(F/1, [req(0, nonempty_list)],
             [ensure(result, expr), ensure(result, proper_list),
              ensure(result, nonempty_list),
              ensure(result, proper_list_length(2)), ensure(result, nonvar)],
             card(1, 1), [pure], 100) :-
    decons_builtin(F).
builtin_mode(F/1, [],
             [ensure(result, expr), ensure(result, proper_list),
              ensure(result, nonempty_list),
              ensure(result, proper_list_length(2)), ensure(result, nonvar)],
             card(0, 1), [pure], 0) :-
    decons_builtin(F).

% car-atom throws on the empty list and returns exactly once otherwise; cdr-atom
% returns () on non-list inputs.  A nonempty-list argument enables useful shape
% facts without changing their exactly-one cardinality.
builtin_mode('car-atom'/1, [req(0, nonempty_list)],
             [], card(1, 1), [pure], 100).
builtin_mode('car-atom'/1, [], [], card(1, 1), [pure], 0).

builtin_mode('cdr-atom'/1, [req(0, nonempty_list)],
             [ensure(result, proper_list), ensure(result, nonvar)],
             card(1, 1), [pure], 100).
builtin_mode('cdr-atom'/1, [], [], card(1, 1), [pure], 0).

% Constructors always build a nonvar pair.  A proper tail is required to call
% that pair a proper, nonempty expression; the unguarded row therefore records
% only the fact true even for an improper tail.
cons_builtin(cons).
cons_builtin('cons-atom').

builtin_mode(F/2, [req(1, proper_list)],
             [ensure(result, expr), ensure(result, proper_list),
              ensure(result, nonempty_list), ensure(result, nonvar)],
             card(1, 1), [pure], 100) :-
    cons_builtin(F).
builtin_mode(F/2, [],
             [ensure(result, nonvar)],
             card(1, 1), [pure], 0) :-
    cons_builtin(F).

% Compiler forms.  collapse collects every inner solution into exactly one
% proper list. once caps the upper bound at one but may still return nothing.
builtin_mode(collapse/1, [],
             [ensure(result, expr), ensure(result, proper_list),
              ensure(result, nonvar)],
             card(1, 1), [control], 50).
builtin_mode(once/1, [], [], card(0, 1), [control], 50).
builtin_mode(empty/0, [], [], card(0, 0), [control], 50).

% A nonempty source list makes superpose productive; its general mode may have
% no answers.  Both remain unbounded because list length is not tracked here.
builtin_mode(superpose/1, [req(0, nonempty_list)], [],
             card(1, many), [control], 100).
builtin_mode(superpose/1, [], [], card(0, many), [control], 0).

%!  mode_applicable(+Key, :HasFact, -Mode) is nondet.
%
%   Enumerate applicable rows for Key. HasFact is called once per requirement.
mode_applicable(Key, HasFact,
                mode(Key, Preconditions, Postconditions,
                     Card, Effects, Priority)) :-
    builtin_mode(Key, Preconditions, Postconditions, Card, Effects, Priority),
    requirements_hold(Preconditions, HasFact).

requirements_hold([], _).
requirements_hold([req(ArgIndex, Fact)|Rest], HasFact) :-
    requirement_fact(HasFact, ArgIndex, Fact),
    requirements_hold(Rest, HasFact).
requirements_hold([guard(Constraint)|Rest], HasFact) :-
    summary_guard(Constraint),
    requirements_hold(Rest, HasFact).

requirement_fact(HasFact, ArgIndex, Fact) :-
    ( ground(Fact)
      -> call(HasFact, ArgIndex, Fact)
    ; call(HasFact, ArgIndex, Stored),
      Stored = Fact
    ).

summary_guard(ground_duplicate_free_list(Values)) :-
    ground(Values),
    is_list(Values),
    sort(Values, Unique),
    same_length(Values, Unique).
summary_guard(ground_atomic_duplicate_free_list(Values)) :-
    summary_guard(ground_duplicate_free_list(Values)),
    maplist(atomic, Values).
summary_guard(at_most_one_list(Values)) :-
    is_list(Values),
    length(Values, Length),
    Length =< 1.

%!  select_builtin_mode(+F, +N, :HasFact, -Posts, -Card, -Effects) is semidet.
%
%   Select the applicable row with the greatest priority. Equal-priority rows
%   are required to be identical by validate_summary_table/0.
select_builtin_mode(F, N, HasFact, Posts, Card, Effects) :-
    findall(Priority-mode(Preconditions, Postconditions, RowCard, RowEffects),
            mode_applicable(F/N, HasFact,
                            mode(F/N, Preconditions, Postconditions,
                                 RowCard, RowEffects, Priority)),
            Applicable),
    Applicable = [_|_],
    keysort(Applicable, Sorted),
    last(Sorted, _-mode(_, Posts, Card, Effects)).

%!  post_fact(+Posts, +Target, -Fact) is nondet.
%
%   Project facts ensured for result or arg(Index).
post_fact(Posts, Target, Fact) :-
    member(ensure(Target, Fact), Posts).

%!  validate_summary_table is det.
%
%   Check row shape, known facts/effects, valid cardinalities and deterministic
%   selection at each priority. Throws error(invalid_builtin_summary(...), _)
%   on the first malformed row.
validate_summary_table :-
    forall(builtin_mode(Key, Preconditions, Postconditions,
                        Card, Effects, Priority),
           validate_row(Key, Preconditions, Postconditions,
                        Card, Effects, Priority)),
    validate_no_priority_ties.

validate_row(Key, Preconditions, Postconditions, Card, Effects, Priority) :-
    summary_assert(valid_key(Key), bad_key(Key)),
    summary_assert(is_list(Preconditions), bad_preconditions(Key, Preconditions)),
    maplist(validate_requirement(Key), Preconditions),
    validate_guard_scope(Key, Preconditions),
    summary_assert(is_list(Postconditions), bad_postconditions(Key, Postconditions)),
    maplist(validate_postcondition(Key), Postconditions),
    summary_assert(valid_card(Card), bad_cardinality(Key, Card)),
    summary_assert(is_list(Effects), bad_effects(Key, Effects)),
    maplist(validate_effect(Key), Effects),
    summary_assert(integer(Priority), bad_priority(Key, Priority)).

valid_key(F/N) :- atom(F), integer(N), N >= 0.

validate_requirement(Key, Requirement) :-
    Requirement = req(Index, Fact), !,
    summary_assert(integer(Index), bad_argument_index(Key, Index)),
    Key = _/Arity,
    summary_assert(Index >= 0, bad_argument_index(Key, Index)),
    summary_assert(Index < Arity, bad_argument_index(Key, Index)),
    summary_assert(nonvar(Fact), bad_fact(Key, Fact)),
    summary_assert(known_fact(Fact), bad_fact(Key, Fact)).
validate_requirement(Key, Requirement) :-
    Requirement = guard(Constraint), !,
    summary_assert(nonvar(Constraint), bad_guard(Key, Constraint)),
    summary_assert(known_guard(Constraint), bad_guard(Key, Constraint)).
validate_requirement(Key, Requirement) :-
    throw(error(invalid_builtin_summary(bad_requirement(Key, Requirement)),
                call_summaries)).

validate_guard_scope(Key, Preconditions) :-
    validate_guard_scope(Preconditions, [], Key).

validate_guard_scope([], _, _).
validate_guard_scope([req(_, Fact)|Rest], Seen0, Key) :-
    term_variables(Fact, FactVars),
    append(FactVars, Seen0, Seen),
    validate_guard_scope(Rest, Seen, Key).
validate_guard_scope([guard(Constraint)|Rest], Seen, Key) :-
    term_variables(Constraint, GuardVars),
    summary_assert(vars_subset_eq(GuardVars, Seen),
                   unbound_guard_variables(Key, Constraint)),
    validate_guard_scope(Rest, Seen, Key).

vars_subset_eq([], _).
vars_subset_eq([Var|Vars], Seen) :-
    member_eq(Var, Seen),
    vars_subset_eq(Vars, Seen).

member_eq(Var, [Seen|_]) :- Var == Seen, !.
member_eq(Var, [_|Seen]) :- member_eq(Var, Seen).

validate_postcondition(Key, Postcondition) :-
    summary_assert(Postcondition = ensure(Target, Fact),
                   bad_postcondition(Key, Postcondition)),
    summary_assert(valid_target(Key, Target), bad_post_target(Key, Target)),
    summary_assert(nonvar(Fact), bad_fact(Key, Fact)),
    summary_assert(known_fact(Fact), bad_fact(Key, Fact)).

valid_target(_, result).
valid_target(_/Arity, arg(Index)) :-
    integer(Index), Index >= 0, Index < Arity.

valid_card(card(Lower, Upper)) :-
    memberchk(Lower, [0, 1]),
    ( Upper == many
    ; memberchk(Upper, [0, 1]), Upper >= Lower ).

validate_effect(Key, Effect) :-
    summary_assert(known_effect(Effect), bad_effect(Key, Effect)).

known_fact(nonvar).
known_fact(ground).
known_fact(proper_bool).
known_fact(proper_list).
known_fact(nonempty_list).
known_fact(proper_list_length(Length)) :-
    integer(Length), Length >= 0.
known_fact(duplicate_free).
known_fact(expr).
known_fact(number).
known_fact(variable).
known_fact(type(Type)) :- nonvar(Type).
% The row declaration carries a variable here; the application-time guard
% checks the concrete callback-supplied value is a proper source list.
known_fact(literal_list(_)).

known_guard(ground_duplicate_free_list(_)).
known_guard(ground_atomic_duplicate_free_list(_)).
known_guard(at_most_one_list(_)).

known_effect(pure).
known_effect(control).
known_effect(state).
known_effect(opaque).

summary_assert(Goal, _) :- call(Goal), !.
summary_assert(_, Problem) :-
    throw(error(invalid_builtin_summary(Problem), call_summaries)).

validate_no_priority_ties :-
    findall(Key-Priority-mode(Pre, Post, Card, Effects),
            builtin_mode(Key, Pre, Post, Card, Effects, Priority),
            Rows),
    forall(( select(Key-Priority-ModeA, Rows, Rest),
             member(Key-Priority-ModeB, Rest) ),
           summary_assert(ModeA =@= ModeB,
                          ambiguous_priority(Key, Priority, ModeA, ModeB))).

:- begin_tests(call_summaries).

has_none(_, _) :- fail.
has_proper_bool(_, proper_bool).
has_bool_type(_, type('Bool')).
has_nonempty(0, nonempty_list).
has_ground_unique_membership(0, ground).
has_ground_unique_membership(1, proper_list).
has_ground_unique_membership(1, ground).
has_ground_unique_membership(1, duplicate_free).
has_nonground_probe_unique_list(0, nonvar).
has_nonground_probe_unique_list(1, proper_list).
has_nonground_probe_unique_list(1, ground).
has_nonground_probe_unique_list(1, duplicate_free).
has_unbound_probe_unique_list(1, proper_list).
has_unbound_probe_unique_list(1, ground).
has_unbound_probe_unique_list(1, duplicate_free).
has_ground_duplicate_membership(0, ground).
has_ground_duplicate_membership(1, proper_list).
has_ground_duplicate_membership(1, ground).
has_literal_atomic_unique(0, nonvar).
has_literal_atomic_unique(1, proper_list).
has_literal_atomic_unique(1, literal_list([alpha, beta, gamma])).
has_literal_atomic_duplicate(0, nonvar).
has_literal_atomic_duplicate(1, proper_list).
has_literal_atomic_duplicate(1, literal_list([alpha, alpha])).
has_literal_compound_unique(0, nonvar).
has_literal_compound_unique(1, proper_list).
has_literal_compound_unique(1, literal_list([f(a), f(b)])).
has_ground_literal_compound_unique(0, ground).
has_ground_literal_compound_unique(1, proper_list).
has_ground_literal_compound_unique(1, literal_list([f(a), f(b)])).
has_literal_singleton(1, literal_list([f(a)])).

test(summary_table_valid) :-
    validate_summary_table.

test(bool_specific_mode_outranks_fallback) :-
    select_builtin_mode(and, 2, has_proper_bool, Posts, Card, Effects),
    assertion(Card == card(1, 1)),
    assertion(Effects == [pure]),
    assertion(post_fact(Posts, result, proper_bool)).

test(bool_fallback_without_facts) :-
    select_builtin_mode(and, 2, has_none, _, Card, _),
    assertion(Card == card(0, many)).

test(bool_type_does_not_imply_proper_bool) :-
    select_builtin_mode(and, 2, has_bool_type, _, Card, _),
    assertion(Card == card(0, many)).

test(requirements_use_zero_based_argument_indexes) :-
    once(mode_applicable(superpose/1, has_nonempty,
                         mode(superpose/1, [req(0, nonempty_list)], _,
                              card(1, many), _, 100))).

test(is_member_guarded_mode) :-
    select_builtin_mode('is-member', 2, has_ground_unique_membership,
                        Posts, Card, _),
    assertion(Card == card(1, 1)),
    assertion(post_fact(Posts, result, proper_bool)).

test(is_member_non_ground_nonvar_probe_is_not_det) :-
    select_builtin_mode('is-member', 2, has_nonground_probe_unique_list,
                        _, Card, _),
    assertion(Card == card(1, many)).

test(is_member_unbound_probe_is_not_det) :-
    select_builtin_mode('is-member', 2, has_unbound_probe_unique_list,
                        _, Card, _),
    assertion(Card == card(1, many)).

test(is_member_duplicate_ground_list_is_not_det) :-
    select_builtin_mode('is-member', 2, has_ground_duplicate_membership,
                        _, Card, _),
    assertion(Card == card(1, many)).

test(is_member_atomic_literal_reconstructs_det_contract) :-
    select_builtin_mode('is-member', 2, has_literal_atomic_unique,
                        _, Card, _),
    assertion(Card == card(1, 1)).

test(is_member_duplicate_literal_is_not_det) :-
    select_builtin_mode('is-member', 2, has_literal_atomic_duplicate,
                        _, Card, _),
    assertion(Card == card(1, many)).

test(is_member_non_ground_compound_probe_is_not_det) :-
    select_builtin_mode('is-member', 2, has_literal_compound_unique,
                        _, Card, _),
    assertion(Card == card(1, many)).

test(is_member_ground_compound_literal_is_det) :-
    select_builtin_mode('is-member', 2, has_ground_literal_compound_unique,
                        _, Card, _),
    assertion(Card == card(1, 1)).

test(is_member_singleton_literal_is_det_for_unbound_probe) :-
    select_builtin_mode('is-member', 2, has_literal_singleton,
                        _, Card, _),
    assertion(Card == card(1, 1)).

test(cons_improper_tail_does_not_claim_expression) :-
    select_builtin_mode(cons, 2, has_none, Posts, Card, _),
    assertion(Card == card(1, 1)),
    assertion(\+ post_fact(Posts, result, expr)),
    assertion(post_fact(Posts, result, nonvar)).

test(decons_fallback_is_semidet) :-
    select_builtin_mode(decons, 1, has_none, Posts, Card, _),
    assertion(Card == card(0, 1)),
    assertion(post_fact(Posts, result, proper_list)),
    assertion(post_fact(Posts, result, nonempty_list)),
    assertion(post_fact(Posts, result, proper_list_length(2))).

test(decons_nonempty_mode_is_det_and_exact_pair) :-
    select_builtin_mode(decons, 1, has_nonempty, Posts, Card, _),
    assertion(Card == card(1, 1)),
    assertion(post_fact(Posts, result, proper_list_length(2))).

test(unknown_builtin_has_no_summary, [fail]) :-
    select_builtin_mode('__missing__', 0, has_none, _, _, _).

:- end_tests(call_summaries).
