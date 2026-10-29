:- ensure_loaded('../src/metta').

'scalar-touch'(X,X) :- flag(scalar_touches,N,N+1).
:- multifile metta_memoized_dispatch_call/4.
metta_memoized_dispatch_call('scalar-memo',[X],Out,(Out is X+100)).

:- begin_tests(scalar_inlining).
:- retractall(user:silent(_)),assertz(user:silent(true)).
:- process_metta_string("
(: scalar-bayes (-[det]-> Number Number Number Number))
(= (scalar-bayes $a $l1 $l0)
   (let* (($n (* $l1 $a)) ($d (+ $n (* $l0 (- 1 $a)))))
      (if (> $d 0) (/ $n $d) $a)))
(: scalar-use (-[det]-> Number Number Number Number))
(= (scalar-use $a $l1 $l0) (scalar-bayes $a $l1 $l0))
(: scalar-associated (-[det]-> Number Number Number Number))
(= (scalar-associated $x $y $z) (+ (+ $x $y) $z))
(: scalar-associated-use (-[det]-> Number Number Number Number))
(= (scalar-associated-use $x $y $z) (scalar-associated $x $y $z))
(: scalar-ret (-[det]-> Number Number))
(= (scalar-ret $x) (let $unused (/ 1 $x) $x))
(: scalar-ret-use (-[semidet]-> Number))
(= (scalar-ret-use) (let 2 (scalar-ret $x) $x))
(: scalar-throws (-[det]-> Number Number))
(= (scalar-throws $x) (let $unused (/ 1 0) (+ $x 1)))
(: scalar-throws-use (-[semidet]-> Number))
(= (scalar-throws-use) (let 9 (scalar-throws 1) 9))
(: scalar-constant (-[det]-> Number Number))
(= (scalar-constant $x) (let $unused (/ 1 0) 2))
(: scalar-constant-use (-[semidet]-> Number))
(= (scalar-constant-use) (let 9 (scalar-constant 1) 9))
(: scalar-touch (-[det]-> Number Number))
!(import_prolog_function scalar-touch)
(: scalar-effect-use (-[semidet]-> Number))
(= (scalar-effect-use) (let 9 (scalar-constant (scalar-touch 1)) 9))
(: scalar-choice (-[det]-> Number Number))
(= (scalar-choice $x) (if (> $x 0) 1 (/ 1 0)))
(: scalar-choice-use (-[det]-> Number Number))
(= (scalar-choice-use $x) (scalar-choice $x))
(: scalar-memo (-[det]-> Number Number))
(= (scalar-memo $x) (+ $x 1))
(: scalar-memo-use (-[det]-> Number Number))
(= (scalar-memo-use $x) (scalar-memo $x))
(: scalar-tuple-error (-[det]-> Number (Number Number)))
(= (scalar-tuple-error $x) ((/ 1 0) (+ $x 1)))
(: scalar-tuple-error-use (-[semidet]-> Number))
(= (scalar-tuple-error-use) (let ($a 9) (scalar-tuple-error 1) $a))
",_).

test(helper_call_is_inlined) :-
    clause('scalar-use'(_,_,_,_),Body),
    \+ (sub_term(G,Body),nonvar(G),functor(G,'scalar-bayes',4)),
    'scalar-use'(0.3,0.8,0.2,R),abs(R-0.631578947368421)=<1e-15.

test(zero_evidence_fallback) :- 'scalar-use'(0.3,0.0,0.0,0.3).
test(floating_point_association) :-
    'scalar-associated-use'(1.0e16,-1.0e16,1.0,1.0).
test(returned_input_is_bound_at_entry) :- 'scalar-ret-use'(2).
test(computed_result_constraint_cannot_suppress_earlier_error,
     [throws(error(evaluation_error(zero_divisor),_))]) :- 'scalar-throws-use'(_).
test(constant_head_mismatch_precedes_body) :- \+ 'scalar-constant-use'(_).
test(argument_effect_precedes_head_mismatch) :-
    flag(scalar_touches,_,0),\+ 'scalar-effect-use'(_),flag(scalar_touches,N,N),N==1.
test(untaken_branch_arithmetic_is_deferred) :- 'scalar-choice-use'(1,1).
test(taken_branch_error_is_preserved,
     [throws(error(evaluation_error(zero_divisor),_))]) :- 'scalar-choice-use'(-1,_).
test(memoized_dispatch_is_preserved) :- 'scalar-memo-use'(2,102).
test(tuple_result_constraint_cannot_suppress_earlier_error,
     [throws(error(evaluation_error(zero_divisor),_))]) :- 'scalar-tuple-error-use'(_).
test(condition_and_callee_dependencies_are_recorded) :-
    clause('scalar-use'(_,_,_,_),_,Ref),compiled_deps(Ref,_,_,Deps),
    memberchk(clause_set('scalar-bayes'/3),Deps),
    memberchk(clause_set('>'/2),Deps),memberchk(decl('>'/2),Deps),
    memberchk(effect('>'/2),Deps).

test(source_mutation_can_restore_an_effectful_call_boundary) :-
    process_metta_string("
(: scalar-mutable (-[det]-> Number Number))
(= (scalar-mutable $x) (+ $x 1))
(: scalar-mutable-use (-[det]-> Number Number))
(= (scalar-mutable-use $x) (scalar-mutable $x))
",_),
    'scalar-mutable-use'(3,4),
    clause('scalar-mutable'(_,_),_,Ref),
    retract(user:translated_from(Ref,[=,['scalar-mutable',X],_])),
    assertz(user:translated_from(Ref,[=,['scalar-mutable',X],
                                   [let,T,['scalar-touch',X],[*,T,2]]])),
    notify_mutation(clause_changed('scalar-mutable'/1,runtime)),
    flag(scalar_touches,_,0),'scalar-mutable-use'(3,6),
    flag(scalar_touches,N,N),N==1.

:- end_tests(scalar_inlining).
