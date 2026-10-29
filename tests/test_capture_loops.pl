:- ensure_loaded('../src/metta').

'flat-test-touch'(_,true) :- flag(flat_test_calls,N,N+1).
'flat-test-sum'(Xs,S) :- sum_list(Xs,S).
'flat-test-save'(F,true) :- nb_setval(flat_saved_closure,F).
'flat-test-log'(X,Y) :- Y is log(X).
'flat-test-spine'(Term,Args) :- Term=..[p|Args].
'flat-test-dynamic'(X,Y) :- Y is log(X).
:- dynamic 'flat-test-dynamic'/2.

:- begin_tests(capture_loops).
:- retractall(user:silent(_)),assertz(user:silent(true)).
:- process_metta_string("
!(import! &self (library lib_roman))
(: flat-test-touch (-[det]-> Number Bool))
(: flat-test-sum (-[det]-> (List Number) Number))
(: flat-test-save (-[det]-> (-[det]-> Number Number) Bool))
(: flat-test-log (-[det]-> Number Number))
(: FlatArray (Foreign 0))
(: flat-test-spine (-[det]-> FlatArray (List Number)))
(: flat-test-dynamic (-[det]-> Number Number))
!(import_prolog_function flat-test-touch)
!(import_prolog_function flat-test-sum)
!(import_prolog_function flat-test-save)
!(import_prolog_function flat-test-log)
!(import_prolog_function flat-test-spine)
!(import_prolog_function flat-test-dynamic)
(: flat-test-shift (-[det]-> Number Number Number))
(= (flat-test-shift $k $x) (progn (flat-test-touch $x) (+ $k $x)))
(: flat-test-div (-[det]-> Number Number Number))
(= (flat-test-div $k $x) (progn (flat-test-touch $x) (/ $k $x)))
(: flat-test-choices (-[nondet]-> Number Number Number))
(= (flat-test-choices $k $x) (+ $k $x))
(= (flat-test-choices $k $x) (+ $k $x))
(= (flat-test-choices $k $x) (+ (+ $k $x) 100))
(: flat-test-each (-[$e]-> (-[$e]-> Number Number) (List Number) (List Number)))
(= (flat-test-each $f ()) ())
(= (flat-test-each $f (cons $x $xs)) (cons ($f $x) (flat-test-each $f $xs)))
(: flat-test-run (-[nondet]-> Number (List Number) (List Number)))
(= (flat-test-run $k $xs) (flat-test-each (flat-test-shift $k) $xs))
(: flat-test-total (-[nondet]-> Number (List Number) Number))
(= (flat-test-total $k $xs) (flat-test-sum (flat-test-each (flat-test-shift $k) $xs)))
(: flat-test-div-total (-[nondet]-> Number (List Number) Number))
(= (flat-test-div-total $k $xs) (flat-test-sum (flat-test-each (flat-test-div $k) $xs)))
(: flat-test-alternatives (-[nondet]-> Number (List Number) (List Number)))
(= (flat-test-alternatives $k $xs) (flat-test-each (flat-test-choices $k) $xs))
(: flat-test-observed (-[nondet]-> (-[det]-> Number Number) (List Number) (List Number)))
(= (flat-test-observed $f $xs) (progn (flat-test-save $f) (flat-test-each $f $xs)))
(: flat-test-escaped (-[nondet]-> Number (List Number) (List Number)))
(= (flat-test-escaped $k $xs) (flat-test-observed (flat-test-shift $k) $xs))
(: flat-test-logged (-[det]-> Number Number))
(= (flat-test-logged $x) (+ (flat-test-log $x) 1))
(: flat-test-dynamic-call (-[det]-> Number Number))
(= (flat-test-dynamic-call $x) (+ (flat-test-dynamic $x) 1))
(: flat-test-spine-total (-[det]-> Number FlatArray Number))
(= (flat-test-spine-total $k $term) (flat-test-sum (flat-test-each (flat-test-shift $k) (flat-test-spine $term))))
(: flat-test-branch-each (-[nondet]-> (-[det]-> Number Number) (List Number) (List Number)))
(= (flat-test-branch-each $f ()) ())
(= (flat-test-branch-each $f (cons $x $xs)) (cons (flat-test-choices 0 ($f $x)) (flat-test-branch-each $f $xs)))
(: flat-test-branch-total (-[nondet]-> Number (List Number) Number))
(= (flat-test-branch-total $k $xs) (flat-test-sum (flat-test-branch-each (flat-test-shift $k) $xs)))
",_).

test(flat_captures_keep_distinct_calls) :-
    findall(R,'flat-test-run'(2,[1,2,3],R),[[3,4,5]]),
    findall(R,'flat-test-run'(10,[1,2,3],R),[[11,12,13]]).
test(empty_input_does_not_observe_unbound_capture) :-
    'flat-test-run'(K,[],[]),var(K).
test(open_input_preserves_the_empty_first_answer) :-
    once('flat-test-run'(2,[1|Tail],[3])),Tail==[].
test(late_result_mismatch_keeps_every_callback) :-
    flag(flat_test_calls,_,0),\+ 'flat-test-run'(2,[1,2],[999]),
    flag(flat_test_calls,N,N),N==2.
test(fresh_result_has_one_effect_per_element) :-
    flag(flat_test_calls,_,0),'flat-test-total'(2,[1,2,3],12),
    flag(flat_test_calls,N,N),N==3,
    ho_fresh_result(_,'flat-test-each_Spec_[partial(flat-test-shift,[_])]'),!.
test(fresh_result_preserves_callback_error,
     [throws(error(evaluation_error(zero_divisor),_))]) :- 'flat-test-div-total'(2,[1,0],_).
test(nondeterministic_callbacks_preserve_order_and_duplicates) :-
    findall(R,'flat-test-alternatives'(2,[1,2],R),
            [[3,4],[3,4],[3,104],[3,4],[3,4],[3,104],[103,4],[103,4],[103,104]]).
test(escaping_closure_is_not_flattened) :-
    'flat-test-escaped'(2,[1],[3]),
    nb_getval(flat_saved_closure,partial('flat-test-shift',[2])),
    ho_specialization('flat-test-observed',S),\+ho_capture_layout(S,_).
test(observed_output_keeps_the_original_loop) :-
    S='flat-test-each_Spec_[partial(flat-test-shift,[_])]',
    Goal=..[S,[1,2],2,Out],
    fresh_result_calls(Goal,observed(Out),Copy,[]),Copy==Goal.
test(failed_condition_is_not_a_proper_list_proof) :-
    \+seen_proper_list(X,failed(is_list(X))).
test(rewrite_has_one_answer) :-
    S='flat-test-each_Spec_[partial(flat-test-shift,[_])]',
    Goal=..[S,Xs,2,_],
    findall(Out,fresh_result_calls((is_list(Xs),Goal),head,Out,_),[(_,Copy)]),
    Copy\==Goal.
test(alternative_branch_is_not_a_proper_list_proof) :-
    S='flat-test-each_Spec_[partial(flat-test-shift,[_])]',
    Goal=..[S,Xs,2,_],
    fresh_result_calls((is_list(Xs);Goal),head,Out,_),Out=(_;Copy),Copy==Goal,
    fresh_result_calls((Goal;is_list(Xs)),head,Out2,_),Out2=(Copy2;_),Copy2==Goal.
test(then_branch_is_not_a_proof_for_its_condition) :-
    S='flat-test-each_Spec_[partial(flat-test-shift,[_])]',
    Goal=..[S,Xs,2,_],
    fresh_result_calls((Goal->is_list(Xs)),head,Out,_),Out=(Copy->_),Copy==Goal.
test(succeeded_condition_proves_its_then_branch) :-
    S='flat-test-each_Spec_[partial(flat-test-shift,[_])]',
    Goal=..[S,Xs,2,_],
    fresh_result_calls((is_list(Xs)->Goal),head,Out,_),Out=(_->Copy),Copy\==Goal.
test(an_independent_nondeterministic_prefix_keeps_its_answers) :-
    findall(R,'flat-test-branch-total'(2,[1,2],R),[7,7,107,7,7,107,107,107,207]),
    ho_specialization('flat-test-branch-each',S),\+ho_fresh_result(_,S).
test(static_math_is_inlined) :-
    clause('flat-test-logged'(_,_),Body),
    \+ (sub_term(G,Body),compound(G),functor(G,'flat-test-log',2)),
    'flat-test-logged'(1.0,1.0).
test(static_univ_certifies_its_output) :- proper_list_output('flat-test-spine',1).
test(static_univ_avoids_the_proper_list_walk) :-
    'flat-test-spine-total'(2,p(1,2),7),
    clause('flat-test-spine-total'(_,_,_),Body),
    \+ (sub_term(G,Body),compound(G),functor(G,is_list,1)).
test(dynamic_foreign_math_keeps_its_call_boundary) :-
    clause('flat-test-dynamic-call'(_,_),Body),
    once((sub_term(G,Body),compound(G),functor(G,'flat-test-dynamic',2))).
test(reimport_invalidates_static_math) :-
    setup_call_cleanup(
        true,
        (abolish(user:'flat-test-log'/2),assertz(user:('flat-test-log'(X,Y):-Y is X*2)),
         import_prolog_function('flat-test-log',true),
         'flat-test-logged'(3,7)),
        true).
test(reimport_invalidates_the_foreign_list_certificate) :-
    abolish(user:'flat-test-spine'/2),
    assertz(user:('flat-test-spine'(_, [1|_]))),
    import_prolog_function('flat-test-spine',true),
    \+proper_list_output('flat-test-spine',1),
    clause('flat-test-spine-total'(_,_,_),Body),
    once((sub_term(G,Body),compound(G),functor(G,is_list,1))).
test(invalidation_removes_generated_worker_metadata) :-
    invalidate_specializations('flat-test-each'),
    \+ (ho_fresh_result(_,Parent),sub_atom(Parent,0,_,_,'flat-test-each_Spec_')),
    \+ (ho_capture_layout(Parent,_),sub_atom(Parent,0,_,_,'flat-test-each_Spec_')).

:- end_tests(capture_loops).
