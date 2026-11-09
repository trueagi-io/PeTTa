:- ensure_loaded('../src/metta').
:- use_module(library(plunit)).
:- set_test_options([load(always)]).

:- begin_tests(type_chains).

reference_group([AV0-C|Pairs], AV, [C|Cs]) :-
    partition(same_key(AV0), Pairs, Same, _),
    AV = AV0, bind_pairs(Same, AV, Cs).
reference_group([AV0-_|Pairs], AV, Cs) :-
    exclude(same_key(AV0), Pairs, Rest), reference_group(Rest, AV, Cs).
same_key(A, B-_) :- A =@= B.
bind_pairs([], _, []).
bind_pairs([AV-C|Ps], AV, [C|Cs]) :- bind_pairs(Ps, AV, Cs).

check_groups(Pairs) :-
    copy_term_nat(Pairs, ReferencePairs),
    copy_term_nat(Pairs, ActualPairs),
    findall(AV-Cs, reference_group(ReferencePairs, AV, Cs), Expected),
    findall(AV-Cs, user:group_arg_chains(ActualPairs, AV, Cs), Actual),
    assertion(Actual =@= Expected).

test(group_order_and_duplicate_keys) :-
    forall((between(1,20,N), between(1,10,K)),
        ( findall([A,B]-chain([],checked(pair(A,B))),
                  (between(1,N,I), A is I mod K, B is I mod 3), Pairs),
          check_groups(Pairs) )).

test(argument_and_type_variables_stay_shared) :-
    check_groups([pair(X,Y)-chain([checked(X)],checked(Y)),
                  pair(A,B)-chain([checked(B)],checked(A)),
                  pair(P,P)-chain([checked(P)],checked(P))]).

test(original_representative_is_retained) :-
    once(user:group_arg_chains([X-chain([],checked(X)),
                               Y-chain([],checked(Y))], AV, Chains)),
    assertion(AV == X), assertion(Y == X),
    assertion(Chains == [chain([],checked(X)),chain([],checked(X))]).

test(hash_collisions_do_not_merge_distinct_bindings) :-
    user:ordered_arg_groups([0-(0-(left-a)),0-(1-(right-b)),0-(2-(left-c))], Groups),
    assertion(Groups == [0-[left-a,left-c],1-[right-b]]).

test(attributed_keys) :-
    freeze(X,integer(X)), freeze(Y,integer(Y)), check_groups([X-a,Y-b]).

test(cyclic_keys) :-
    X=loop(X), Y=loop(Y), check_groups([X-a,Y-b]).

test(partial_translation_leaves_no_choice_point) :-
    setup_call_cleanup(
        ( assertz(user:'&self'(':', tc_partial_det, [->, A, A, 'Bool'])),
          assertz(user:fun(tc_partial_det)),
          assertz(user:arity(tc_partial_det, 3)) ),
        ( findall(Out-Goals, user:translate_expr([tc_partial_det, 1], Goals, Out), Translations),
          assertion(Translations = [partial(tc_partial_det, [1])-_]),
          Translations = [_-Goals],
          assertion(\+ ( sub_term(Sub, Goals), compound(Sub),
                         functor(Sub, with_type_evidence, 2) )),
          findall(Out, user:eval([tc_partial_det, 1], Out), Outs),
          assertion(Outs == [partial(tc_partial_det, [1])]) ),
        ( retractall(user:'&self'(':', tc_partial_det, _)),
          retractall(user:fun(tc_partial_det)),
          retractall(user:arity(tc_partial_det, _)) )).

test(evidence_arity_filter_preserves_bindings) :-
    Evidence = [chain([checked(type(X))], checked(out(X))),
                chain([checked(other), checked(extra)], checked('Bool'))],
    Remaining = [chain([checked(fresh)], checked('Bool'))],
    user:evidence_chains_for_call(Remaining, Evidence, Selected),
    assertion(Selected == [chain([checked(type(X))], checked(out(X)))]),
    X = bound,
    assertion(Selected == [chain([checked(type(bound))], checked(out(bound)))]),
    findall(SelectedAgain, user:evidence_chains_for_call(Remaining, Evidence, SelectedAgain), Solutions),
    assertion(Solutions == [[chain([checked(type(bound))], checked(out(bound)))]]).

test(evidence_arity_filter_rejects_empty_selection) :-
    Evidence = [chain([checked(type(X))], checked(out(X)))],
    Remaining = [chain([checked(first), checked(second)], checked('Bool'))],
    findall(Selected, user:evidence_chains_for_call(Remaining, Evidence, Selected), Solutions),
    assertion(Solutions == []),
    assertion(var(X)).

:- end_tests(type_chains).
