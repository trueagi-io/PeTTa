:- ensure_loaded('../src/metta').

'sf-touch'(X,X) :- flag(sf_calls,N,N+1).
'sf-save'(X,true) :- nb_setval(sf_saved,X).
'sf-offset'(X,Y) :- Y is X+1.

:- begin_tests(scalar_folds).
:- retractall(user:silent(_)),assertz(user:silent(true)).
:- process_metta_string("
!(import! &self (library lib_roman))
(: sf-touch (-[det]-> Number Number))
(: sf-save (-[det]-> (Number Number) Bool))
!(import_prolog_function sf-touch)
!(import_prolog_function sf-save)
(: sf-step (-[det]-> Number (Number Number) Number (Number Number)))
(= (sf-step $k ($a $b) $x)
   (let $v (sf-touch $x) ((+ $a $v) (+ $b (* $k $v)))))
(: sf-total (-[det]-> Number (List Number) (Number Number)))
(= (sf-total $k $xs) (fold-flat (sf-step $k) (data 0 0) $xs))
(: sf-from (-[det]-> Number (Number Number) (List Number) (Number Number)))
(= (sf-from $k $start $xs) (fold-flat (sf-step $k) $start $xs))
(: sf-div-step (-[det]-> Number (Number Number) Number (Number Number)))
(= (sf-div-step $k ($a $b) $x) ((+ $a $x) (+ $b (/ $k $x))))
(: sf-div (-[det]-> Number (List Number) (Number Number)))
(= (sf-div $k $xs) (fold-flat (sf-div-step $k) (data 0 0) $xs))
(: sf-escape-step (-[semidet]-> Number (Number Number) Number (Number Number)))
(= (sf-escape-step $k $state $x)
   (progn (sf-save $state) (let ($a $b) $state ((+ $a $x) (+ $b $k)))))
(: sf-escape (-[semidet]-> Number (List Number) (Number Number)))
(= (sf-escape $k $xs) (fold-flat (sf-escape-step $k) (data 0 0) $xs))
(: sf-open-step (-[det]-> Number (Number Number) Number (Number Number)))
(= (sf-open-step $k ($a $b) $x) ($a (+ $b (+ $k $x))))
(: sf-open (-[det]-> Number (Number Number) (List Number) (Number Number)))
(= (sf-open $k $state $xs) (fold-flat (sf-open-step $k) $state $xs))
(: sf-choices (-[nondet]-> Number (Number Number) Number (Number Number)))
(= (sf-choices $k ($a $b) $x)
   (let $n (superpose ($x $x)) ((+ $a $n) (+ $b $k))))
(: sf-many (-[nondet]-> Number (List Number) (Number Number)))
(= (sf-many $k $xs) (fold-flat (sf-choices $k) (data 0 0) $xs))
(: sf-offset (-[det]-> Number Number))
!(import_prolog_function sf-offset)
(: sf-changing-step (-[det]-> Number (Number Number) Number (Number Number)))
(= (sf-changing-step $k ($a $b) $x) ((+ $a (sf-offset $x)) (+ $b $k)))
(: sf-changing (-[det]-> Number (List Number) (Number Number)))
(= (sf-changing $k $xs) (fold-flat (sf-changing-step $k) (data 0 0) $xs))
(: sf-third-step (-[det]-> Number (Number Number Number) Number (Number Number Number)))
(= (sf-third-step $k ($a $b $c) $x) ((+ $a $x) (+ $b $k) (+ $c (* $x $x))))
(: sf-third (-[det]-> Number (List Number) (Number Number Number)))
(= (sf-third $k $xs) (fold-flat (sf-third-step $k) (data 0 0 0) $xs))
(: sf-constrained-step (-[semidet]-> Number (Number Number) Number (Number Number)))
(= (sf-constrained-step $k (0 $b) $x) ((+ $x 1) (+ $b $k)))
(: sf-constrained (-[semidet]-> Number (List Number) (Number Number)))
(= (sf-constrained $k $xs) (fold-flat (sf-constrained-step $k) (data 0 0) $xs))
(: sf-sum (-[det]-> Number (List Number) Number))
(= (sf-sum $k $xs) (let ($a $b) (sf-total $k $xs) (+ $a $b)))
(: sf-div-sum (-[det]-> Number (List Number) Number))
(= (sf-div-sum $k $xs) (let ($a $b) (sf-div $k $xs) (+ $a $b)))
(: sf-changing-sum (-[det]-> Number (List Number) Number))
(= (sf-changing-sum $k $xs) (let ($a $b) (sf-changing $k $xs) (+ $a $b)))
(: sf-third-sum (-[det]-> Number (List Number) Number))
(= (sf-third-sum $k $xs) (let ($a $b $c) (sf-third $k $xs) (+ (+ $a $b) $c)))
(: sf-constrained-sum (-[semidet]-> Number (List Number) Number))
(= (sf-constrained-sum $k $xs) (let ($a $b) (sf-constrained $k $xs) (+ $a $b)))
(: sf-escape-sum (-[semidet]-> Number (List Number) Number))
(= (sf-escape-sum $k $xs) (let ($a $b) (sf-escape $k $xs) (+ $a $b)))
(: sf-open-sum (-[det]-> Number (Number Number) (List Number) Number))
(= (sf-open-sum $k $state $xs) (let ($a $b) (sf-open $k $state $xs) (+ $a $b)))
(: sf-many-sum (-[nondet]-> Number (List Number) Number))
(= (sf-many-sum $k $xs) (let ($a $b) (sf-many $k $xs) (+ $a $b)))
",_).

test(uses_scalar_worker) :-
    once(ho_scalar_fold(Entry,'sf-total')),
    ho_specialization(Entry,Loop),functor(H,Loop,6),
    clause(H,B),H=..[_,_,[_|_]|_],
    \+ (sub_term(G,B),nonvar(G),G=(_=[_,_])),!.

test(values_and_one_effect_per_element) :-
    flag(sf_calls,_,0),findall(R,'sf-sum'(3,[1,2,3],R),[24]),
    flag(sf_calls,N,N),N==3.

test(empty_state_retains_cell_identity) :-
    Parent='fold-flat_Spec_[partial(sf-step,[_])]',
    State=[A,B],Goal=..[Parent,State,[],9,R],once(call(Goal)),R==State,
    var(A),var(B),nb_setarg(1,State,changed),R=[changed,_].

test(bound_result_does_not_suppress_effects) :-
    flag(sf_calls,_,0),\+'sf-sum'(3,[1,2,3],999),
    flag(sf_calls,N,N),N==3.

test(bound_result_keeps_later_error,[throws(error(evaluation_error(zero_divisor),_))]) :-
    'sf-div-sum'(1,[2,0],999).

test(open_input_keeps_empty_first_answer) :-
    once('fold-flat_Spec_[partial(sf-step,[_])]'([0,0],Xs,2,[0,0])),Xs==[].

test(improper_input_fails_after_effects) :-
    flag(sf_calls,_,0),\+'fold-flat_Spec_[partial(sf-step,[_])]'([0,0],[1,2|bad],2,_),flag(sf_calls,N,N),N==2.

test(escaping_state_keeps_original_fold) :-
    'sf-escape-sum'(3,[1,2],9),nb_getval(sf_saved,[1,3]),
    \+ho_scalar_fold(_,'sf-escape').

test(unbound_fields_keep_original_fold) :-
    'sf-open'(2,[A,0],[1],[B,3]),A==B,var(A),
    'sf-open-sum'(2,[1,0],[1],4),\+ho_scalar_fold(_,'sf-open').

test(nondeterministic_steps_keep_order_and_duplicates) :-
    findall(R,'sf-many-sum'(3,[1,2],R),[9,9,9,9]),\+ho_scalar_fold(_,'sf-many').

test(observed_fields_are_not_scalarized) :-
    G=(fresh_data_list([A,B],P),'sf-total'(2,[1],P)),
    scalar_fold_calls(G,observed(A),Copy,[]),Copy==G,var(A),var(B),var(P).

test(escaping_result_is_not_scalarized) :-
    G=(fresh_data_list([A,B],P),('sf-total'(2,[1],P),'sf-save'(P,_))),
    scalar_fold_calls(G,none,Copy,[]),Copy==G,var(A),var(B),var(P).

test(wrong_tuple_arity_keeps_the_original_failure) :-
    G=(fresh_data_list([_,_,_],P),'sf-total'(2,[1],P)),
    scalar_fold_calls(G,none,Copy,[]),Copy==G,\+call(Copy).

test(three_field_accumulator) :-
    'sf-third-sum'(3,[1,2,3],29),
    once(ho_scalar_fold(_,'sf-third')).

test(last_state_need_not_match_the_step_pattern) :-
    'sf-constrained-sum'(3,[1],5),\+'sf-constrained-sum'(3,[1,2],_),
    once(ho_scalar_fold(_,'sf-constrained')).

test(reimport_discards_stale_workers_and_uses_current_callback) :-
    'sf-changing-sum'(3,[1,2],11),
    once(ho_scalar_fold(Old,'sf-changing')),
    abolish(user:'sf-offset'/2),assertz(user:('sf-offset'(X,Y):-Y is X*2)),
    import_prolog_function('sf-offset',true),
    'sf-changing-sum'(3,[1,2],12),
    \+ho_scalar_fold(Old,_),\+compiled_deps(_,Old/_,_,_),
    \+compiled_body_stale(_).

test(invalidation_removes_workers_and_metadata) :-
    findall(E,ho_scalar_fold(E,'sf-total'),Entries),Entries\=[],
    findall(L,(member(E,Entries),ho_specialization(E,L)),Loops),
    invalidate_specializations('sf-total'),
    forall(member(E,Entries),(\+ho_scalar_fold(E,_),\+fun(E),\+compiled_deps(_,E/_,_,_))),
    forall(member(L,Loops),(\+fun(L),\+compiled_deps(_,L/_,_,_))).

test(workers_can_be_recreated_after_invalidation) :-
    recompile_function_clauses('sf-sum'),
    'sf-sum'(3,[1,2,3],24),
    once(ho_scalar_fold(_,'sf-total')).

:- end_tests(scalar_folds).
