:- module(abstract_domain,
          [ card_det/1,
            card_semidet/1,
            card_multi/1,
            card_nondet/1,
            card_zero/1,
            card_seq/3,
            card_choice/3,
            card_exclusive/3,
            card_once/2,
            card_join/3,
            card_level/2,
            card_satisfies/2,
            state_empty/1,
            state_add_fact/4,
            state_add_facts/4,
            state_has_fact/3,
            state_facts/3,
            state_remove_fact/4,
            state_join/3,
            state_meet/3
          ]).

/** <module> Abstract domains for the unified checker

Cardinalities are closed intervals `card(Min, Max)`.  `Min` is zero or one;
`Max` is zero, one, or `many`.  The five inhabitants are therefore zero,
det, semidet, multi (one or more), and nondet (zero or more).

States are immutable finite maps from ground value identifiers to sets of
facts.  Facts may contain variables.  Set operations compare such facts with
variant equality (`=@=`), which neither binds their variables nor conflates
non-variant type payloads.  The representation is deliberately private:

    state([entry(ValueId, [Fact, ...]), ...])

Map and fact order is stable but has no abstract meaning.  CFG join keeps only
facts known on every incoming path; meet/refinement combines facts from both
inputs.  This domain does not assign contradictory meanings to any fact pair,
so meet has no additional conflict case.
*/


% -- Cardinality -----------------------------------------------------------

card_zero(card(0, 0)).
card_det(card(1, 1)).
card_semidet(card(0, 1)).
card_multi(card(1, many)).
card_nondet(card(0, many)).

%!  card_level(?Card, ?Level) is nondet.
%
%   Relate every canonical interval to its conventional name.

card_level(card(0, 0), zero).
card_level(card(1, 1), det).
card_level(card(0, 1), semidet).
card_level(card(1, many), multi).
card_level(card(0, many), nondet).

canonical_card(Card) :-
    once(card_level(Card, _)).

%!  card_seq(+Left, +Right, -Result) is det.
%
%   Sequential composition multiplies solution bounds.  A guaranteed result
%   survives only when both sides guarantee one; an upper bound is zero if
%   either side cannot produce a result, one if both are at most one, and many
%   otherwise.

card_seq(Left, Right, card(Min, Max)) :-
    card_parts(Left, LMin, LMax),
    card_parts(Right, RMin, RMax),
    seq_min(LMin, RMin, Min),
    seq_max(LMax, RMax, Max), !.

seq_min(1, 1, 1) :- !.
seq_min(_, _, 0).

seq_max(0, _, 0) :- !.
seq_max(_, 0, 0) :- !.
seq_max(1, 1, 1) :- !.
seq_max(_, _, many).

%!  card_choice(+Left, +Right, -Result) is det.
%
%   Add the alternatives of an ordinary nondeterministic choice.  Two
%   possible singleton results already require the unbounded `many` bucket.

card_choice(Left, Right, card(Min, Max)) :-
    card_parts(Left, LMin, LMax),
    card_parts(Right, RMin, RMax),
    choice_min(LMin, RMin, Min),
    choice_max(LMax, RMax, Max), !.

choice_min(0, 0, 0) :- !.
choice_min(_, _, 1).

choice_max(0, Max, Max) :- !.
choice_max(Max, 0, Max) :- !.
choice_max(_, _, many).

%!  card_exclusive(+Left, +Right, -Result) is det.
%
%   Combine mutually exclusive branches.  Only one branch executes, so this
%   is the interval hull rather than additive choice.

card_exclusive(Left, Right, Result) :-
    card_join(Left, Right, Result).

%!  card_once(+Card, -Once) is det.
%
%   Commit to at most the first solution without changing whether success is
%   guaranteed.

card_once(Card, card(Min, Max)) :-
    card_parts(Card, Min, OldMax),
    once_max(OldMax, Max), !.

once_max(0, 0).
once_max(1, 1).
once_max(many, 1).

%!  card_join(+Left, +Right, -Join) is det.
%
%   Least interval containing both operands.

card_join(Left, Right, card(Min, Max)) :-
    card_parts(Left, LMin, LMax),
    card_parts(Right, RMin, RMax),
    lower_hull(LMin, RMin, Min),
    upper_hull(LMax, RMax, Max), !.

lower_hull(1, 1, 1) :- !.
lower_hull(_, _, 0).

upper_hull(Left, Right, Max) :-
    max_rank(Left, LRank),
    max_rank(Right, RRank),
    Rank is max(LRank, RRank),
    rank_max(Rank, Max).

max_rank(0, 0).
max_rank(1, 1).
max_rank(many, 2).

rank_max(0, 0).
rank_max(1, 1).
rank_max(2, many).

%!  card_satisfies(+Actual, +Declared) is semidet.
%
%   True when Actual's interval is contained in Declared's interval.  Either
%   argument may use a canonical `card/2` term or its level atom.

card_satisfies(Actual0, Declared0) :-
    card_term(Actual0, card(ActualMin, ActualMax)),
    card_term(Declared0, card(DeclaredMin, DeclaredMax)),
    DeclaredMin =< ActualMin,
    max_rank(ActualMax, ActualRank),
    max_rank(DeclaredMax, DeclaredRank),
    ActualRank =< DeclaredRank, !.

card_term(Card, Card) :-
    nonvar(Card),
    Card = card(_, _), !,
    canonical_card(Card).
card_term(Level, Card) :-
    atom(Level),
    once(card_level(Card, Level)).

card_parts(Card, Min, Max) :-
    canonical_card(Card),
    Card = card(Min, Max).


% -- Immutable fact state --------------------------------------------------

state_empty(state([])).

%!  state_add_fact(+State0, +ValueId, +Fact, -State) is det.
%
%   Add Fact unless a variant is already present for ValueId.

state_add_fact(state(Entries0), ValueId, Fact, State) :-
    state_add_facts(state(Entries0), ValueId, [Fact], State).

%!  state_add_facts(+State0, +ValueId, +Facts, -State) is det.
%
%   Bulk variant of state_add_fact/4.  Analyzer fact closure commonly adds
%   four to seven implications at once; rebuilding and detaching the complete
%   immutable map after every individual fact dominated large-file checking.

state_add_facts(state(Entries0), ValueId, Facts, State) :-
    require_ground_value_id(ValueId),
    add_facts_entries(Entries0, ValueId, Facts, Entries),
    detached_state(Entries, State).

add_facts_entries([], ValueId, Facts0, Entries) :-
    variant_dedup_facts(Facts0, Facts),
    ( Facts == [] -> Entries = [] ; Entries = [entry(ValueId, Facts)] ).
add_facts_entries([entry(Key, Facts0)|Entries], ValueId, NewFacts,
                  [entry(Key, Facts)|Entries]) :-
    same_value_id(Key, ValueId), !,
    append_new_facts(NewFacts, Facts0, Facts).
add_facts_entries([Entry|Entries0], ValueId, Facts,
                  [Entry|Entries]) :-
    add_facts_entries(Entries0, ValueId, Facts, Entries).

append_new_facts([], Facts, Facts).
append_new_facts([Fact|NewFacts], Facts0, Facts) :-
    ( variant_member(Fact, Facts0)
      -> Facts1 = Facts0
    ; append(Facts0, [Fact], Facts1) ),
    append_new_facts(NewFacts, Facts1, Facts).

variant_dedup_facts([], []).
variant_dedup_facts([Fact|Facts], Unique) :-
    ( variant_member(Fact, Facts)
      -> variant_dedup_facts(Facts, Unique)
    ; Unique = [Fact|Rest], variant_dedup_facts(Facts, Rest) ).

add_fact_entries([], ValueId, Fact, [entry(ValueId, [Fact])]).
add_fact_entries([entry(Key, Facts0)|Entries], ValueId, Fact,
                 [entry(Key, Facts)|Entries]) :-
    same_value_id(Key, ValueId), !,
    ( variant_member(Fact, Facts0)
      -> Facts = Facts0
    ; append(Facts0, [Fact], Facts) ).
add_fact_entries([Entry|Entries0], ValueId, Fact, [Entry|Entries]) :-
    add_fact_entries(Entries0, ValueId, Fact, Entries).

%!  state_has_fact(+State, +ValueId, ?Fact) is nondet.
%
%   With a variable Fact, enumerate detached copies.  With a supplied Fact,
%   test exact variant membership without binding its payload variables.

state_has_fact(state(Entries), ValueId, Fact) :-
    require_ground_value_id(ValueId),
    lookup_facts(Entries, ValueId, Facts),
    ( var(Fact)
      -> member(Stored, Facts), copy_term(Stored, Fact)
    ; variant_member(Fact, Facts) ).

%!  state_facts(+State, +ValueId, -Facts) is det.
%
%   Return a detached fact list, or the empty list when ValueId is absent.

state_facts(state(Entries), ValueId, Facts) :-
    require_ground_value_id(ValueId),
    ( lookup_facts(Entries, ValueId, Stored)
      -> copy_term(Stored, Facts)
    ; Facts = [] ).

%!  state_remove_fact(+State0, +ValueId, +Fact, -State) is det.
%
%   Remove the exact fact variant.  Empty map entries are removed as well.

state_remove_fact(state(Entries0), ValueId, Fact, State) :-
    require_ground_value_id(ValueId),
    remove_fact_entries(Entries0, ValueId, Fact, Entries),
    detached_state(Entries, State).

remove_fact_entries([], _, _, []).
remove_fact_entries([entry(Key, Facts0)|Entries], ValueId, Fact, Result) :-
    same_value_id(Key, ValueId), !,
    remove_fact_variants(Facts0, Fact, Facts),
    ( Facts == []
      -> Result = Entries
    ; Result = [entry(Key, Facts)|Entries] ).
remove_fact_entries([Entry|Entries0], ValueId, Fact, [Entry|Entries]) :-
    remove_fact_entries(Entries0, ValueId, Fact, Entries).

remove_fact_variants([], _, []).
remove_fact_variants([Stored|Facts0], Fact, Facts) :-
    ( Stored =@= Fact
      -> remove_fact_variants(Facts0, Fact, Facts)
    ; Facts = [Stored|Rest],
      remove_fact_variants(Facts0, Fact, Rest) ).

%!  state_join(+Left, +Right, -Join) is det.
%
%   CFG join: retain facts present (up to variable renaming) on both paths.

state_join(state(Left), state(Right), State) :-
    intersect_entries(Left, Right, Entries),
    detached_state(Entries, State).

intersect_entries([], _, []).
intersect_entries([entry(Key, LeftFacts)|Left], Right, Entries) :-
    ( lookup_facts(Right, Key, RightFacts)
      -> intersect_facts(LeftFacts, RightFacts, Common)
    ; Common = [] ),
    ( Common == []
      -> Entries = Rest
    ; Entries = [entry(Key, Common)|Rest] ),
    intersect_entries(Left, Right, Rest).

intersect_facts([], _, []).
intersect_facts([Fact|Facts], Other, Common) :-
    ( variant_member(Fact, Other)
      -> Common = [Fact|Rest]
    ; Common = Rest ),
    intersect_facts(Facts, Other, Rest).

%!  state_meet(+Left, +Right, -Meet) is det.
%
%   Refinement meet: combine the compatible knowledge in both states.  Facts
%   have no built-in negation or exclusivity in this domain, hence all facts are
%   compatible and the operation is variant-set union.

state_meet(state(Left), state(Right), State) :-
    union_entries(Left, Right, Entries),
    detached_state(Entries, State).

union_entries(Left, [], Left).
union_entries(Left0, [entry(Key, Facts)|Right], Union) :-
    add_entry_facts(Facts, Key, Left0, Left),
    union_entries(Left, Right, Union).

add_entry_facts([], _, Entries, Entries).
add_entry_facts([Fact|Facts], Key, Entries0, Entries) :-
    add_fact_entries(Entries0, Key, Fact, Entries1),
    add_entry_facts(Facts, Key, Entries1, Entries).

lookup_facts([entry(Key, Facts)|_], ValueId, Facts) :-
    same_value_id(Key, ValueId), !.
lookup_facts([_|Entries], ValueId, Facts) :-
    lookup_facts(Entries, ValueId, Facts).

same_value_id(Left, Right) :-
    Left == Right.

variant_member(Fact, [Stored|_]) :-
    Fact =@= Stored, !.
variant_member(Fact, [_|Facts]) :-
    variant_member(Fact, Facts).

detached_state(Entries, State) :-
    copy_term(state(Entries), State).

require_ground_value_id(ValueId) :-
    ( ground(ValueId)
      -> true
    ; throw(error(instantiation_error,
                  context(abstract_domain, 'value identifier must be ground'))) ).


:- begin_tests(abstract_domain).

test(card_constructors_and_levels) :-
    card_zero(card(0, 0)),
    card_det(card(1, 1)),
    card_semidet(card(0, 1)),
    card_multi(card(1, many)),
    card_nondet(card(0, many)),
    findall(Level, card_level(_, Level),
            [zero, det, semidet, multi, nondet]).

test(card_sequential_composition) :-
    card_seq(card(0, 0), card(0, many), card(0, 0)),
    card_seq(card(0, 1), card(1, 1), card(0, 1)),
    card_seq(card(1, many), card(0, 1), card(0, many)),
    card_seq(card(1, 1), card(1, many), card(1, many)).

test(card_nondeterministic_choice) :-
    card_choice(card(0, 0), card(1, 1), card(1, 1)),
    card_choice(card(0, 1), card(0, 1), card(0, many)),
    card_choice(card(1, 1), card(1, 1), card(1, many)),
    card_choice(card(1, many), card(0, 0), card(1, many)).

test(card_exclusive_and_join_are_interval_hulls) :-
    card_exclusive(card(0, 0), card(1, 1), card(0, 1)),
    card_exclusive(card(1, 1), card(1, many), card(1, many)),
    card_join(card(0, 0), card(1, many), card(0, many)).

test(card_once_caps_only_the_upper_bound) :-
    card_once(card(0, 0), card(0, 0)),
    card_once(card(1, 1), card(1, 1)),
    card_once(card(0, many), card(0, 1)),
    card_once(card(1, many), card(1, 1)).

test(card_containment) :-
    card_satisfies(det, semidet),
    card_satisfies(zero, semidet),
    card_satisfies(card(1, 1), multi),
    card_satisfies(multi, nondet),
    \+ card_satisfies(semidet, det),
    \+ card_satisfies(nondet, multi).

test(state_add_is_variant_set_and_does_not_bind_payloads) :-
    state_empty(S0),
    state_add_fact(S0, value(1), type(pair(X, X)), S1),
    state_add_fact(S1, value(1), type(pair(Y, Y)), S2),
    state_add_fact(S2, value(1), type(pair(A, B)), S3),
    var(X), var(Y), var(A), var(B), A \== B,
    state_facts(S3, value(1), Facts),
    length(Facts, 2),
    state_has_fact(S3, value(1), type(pair(P, P))),
    var(P),
    state_has_fact(S3, value(1), type(pair(Q, R))),
    var(Q), var(R), Q \== R.

test(state_fact_enumeration_is_detached) :-
    state_empty(S0),
    state_add_fact(S0, v, type(list(T)), S1),
    state_has_fact(S1, v, Fact),
    Fact = type(list(number)),
    var(T),
    state_facts(S1, v, [type(list(Stored))]),
    var(Stored).

test(state_remove_fact_and_empty_entry) :-
    state_empty(S0),
    state_add_fact(S0, v, type(number), S1),
    state_add_fact(S1, v, effect(det), S2),
    state_remove_fact(S2, v, type(number), S3),
    \+ state_has_fact(S3, v, type(number)),
    state_has_fact(S3, v, effect(det)),
    state_remove_fact(S3, v, effect(det), S4),
    state_facts(S4, v, []).

test(state_add_facts_is_variant_safe_and_detached) :-
    state_empty(S0),
    state_add_facts(S0, v, [type(pair(X, X)), marker,
                             type(pair(Y, Y))], S1),
    X = changed,
    Y = changed_too,
    state_facts(S1, v, Facts),
    Facts = [marker, type(pair(Stored, Stored))],
    var(Stored).

test(state_join_is_common_path_knowledge) :-
    state_empty(S0),
    state_add_fact(S0, v, type(list(X)), Left0),
    state_add_fact(Left0, v, left_only, Left),
    state_add_fact(S0, v, type(list(Y)), Right0),
    state_add_fact(Right0, v, right_only, Right),
    state_join(Left, Right, Join),
    var(X), var(Y),
    state_facts(Join, v, [type(list(Z))]),
    var(Z).

test(state_meet_is_variant_set_union) :-
    state_empty(S0),
    state_add_fact(S0, v, type(list(X)), Left0),
    state_add_fact(Left0, v, left_only, Left),
    state_add_fact(S0, v, type(list(Y)), Right0),
    state_add_fact(Right0, v, right_only, Right),
    state_meet(Left, Right, Meet),
    var(X), var(Y),
    state_facts(Meet, v, Facts),
    length(Facts, 3),
    state_has_fact(Meet, v, type(list(Q))),
    state_has_fact(Meet, v, left_only),
    state_has_fact(Meet, v, right_only),
    var(Q).

test(state_keys_must_be_ground,
     [throws(error(instantiation_error, _))]) :-
    state_empty(State),
    state_add_fact(State, value(_), fact, _).

:- end_tests(abstract_domain).
