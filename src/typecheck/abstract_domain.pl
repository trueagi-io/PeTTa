:- module(abstract_domain,
          [ card_zero/1,
            card_seq/3,
            card_choice/3,
            card_once/2,
            card_join/3,
            card_level/2,
            state_empty/1,
            state_add_fact/4,
            state_add_facts/4,
            state_has_fact/3,
            state_facts/3,
            state_join/3,
            variant_member/2,
            variant_dedup/2
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
facts known on every incoming path.
*/


% -- Cardinality -----------------------------------------------------------

card_zero(card(0, 0)).

%!  card_level(?Card, ?Level) is nondet.
%
%   Relate every canonical interval to its conventional name.

card_level(card(0, 0), zero).
card_level(card(1, 1), det).
card_level(card(0, 1), semidet).
card_level(card(1, many), multi).
card_level(card(0, many), nondet).

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
%   Least interval containing both operands: the cardinality of mutually
%   exclusive branches, of which only one executes.

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

card_parts(Card, Min, Max) :-
    once(card_level(Card, _)),
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
    variant_dedup(Facts0, Facts),
    ( Facts == [] -> Entries = [] ; Entries = [entry(ValueId, Facts)] ).
add_facts_entries([entry(Key, Facts0)|Entries], ValueId, NewFacts,
                  [entry(Key, Facts)|Entries]) :-
    Key == ValueId, !,
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

variant_dedup([], []).
variant_dedup([Fact|Facts], Unique) :-
    ( variant_member(Fact, Facts)
      -> variant_dedup(Facts, Unique)
    ; Unique = [Fact|Rest], variant_dedup(Facts, Rest) ).

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

lookup_facts([entry(Key, Facts)|_], ValueId, Facts) :-
    Key == ValueId, !.
lookup_facts([_|Entries], ValueId, Facts) :-
    lookup_facts(Entries, ValueId, Facts).

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

test(card_levels) :-
    card_zero(card(0, 0)),
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

test(card_join_is_the_interval_hull) :-
    card_join(card(0, 0), card(1, 1), card(0, 1)),
    card_join(card(1, 1), card(1, many), card(1, many)),
    card_join(card(0, 0), card(1, many), card(0, many)).

test(card_once_caps_only_the_upper_bound) :-
    card_once(card(0, 0), card(0, 0)),
    card_once(card(1, 1), card(1, 1)),
    card_once(card(0, many), card(0, 1)),
    card_once(card(1, many), card(1, 1)).

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

test(state_keys_must_be_ground,
     [throws(error(instantiation_error, _))]) :-
    state_empty(State),
    state_add_fact(State, value(_), fact, _).

:- end_tests(abstract_domain).
