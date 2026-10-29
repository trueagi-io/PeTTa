:- ensure_loaded('../src/metta').

'inline-arg-touch'(X,X) :- flag(inline_arg_touches,N,N+1).

:- begin_tests(inline_arguments).
:- retractall(user:silent(_)),assertz(user:silent(true)).
:- process_metta_string("
(: ia-bayes (-[det]-> Number Number Number Number))
(= (ia-bayes $a $u $v)
   (let* (($n (* $u $a)) ($d (+ $n (* $v (- 1 $a)))))
      (if (> $d 0) (/ $n $d) $a)))
(: ia-negative (-[det]-> Number Number Number Number))
(= (ia-negative $a $u $v) (ia-bayes $a (- 1 $u) (- 1 $v)))
(: ia-add (-[det]-> Number Number Number))
(= (ia-add $x $y) (+ $x $y))
(: ia-group (-[det]-> Number Number Number Number))
(= (ia-group $x $y $z) (ia-add (+ $x $y) $z))
(: ia-square (-[det]-> Number Number))
(= (ia-square $x) (* $x $x))
(: ia-shared (-[det]-> Number Number))
(= (ia-shared $x) (ia-square (- 1 $x)))
(: ia-select (-[det]-> Number Number Number))
(= (ia-select $x $y) (if (> $x 0) $y 7))
(: ia-branch (-[det]-> Number Number))
(= (ia-branch $x) (ia-select $x (/ 1 $x)))
(: ia-sign (-[det]-> Number Number))
(= (ia-sign $x) (if (> $x 0) 1 -1))
(: ia-sign-use (-[det]-> Number Number))
(= (ia-sign-use $x) (ia-sign (/ 1 $x)))
(: ia-deferred (-[det]-> Bool Number))
(= (ia-deferred $flag) (if $flag (ia-sign (/ 1 0)) 42))
(: ia-ignore (-[det]-> Number Number))
(= (ia-ignore $x) 7)
(: ia-unused (-[det]-> Number Number))
(= (ia-unused $x) (ia-ignore (/ 1 $x)))
(: ia-constant-use (-[semidet]-> Number Number))
(= (ia-constant-use $x) (let 9 (ia-ignore (/ 1 $x)) 9))
(: ia-id (-[det]-> Number Number))
(= (ia-id $x) $x)
(: ia-constrained (-[semidet]-> Number Number))
(= (ia-constrained $x) (let 0.5 (ia-id (/ 1 $x)) 0.5))
(: ia-choice (-[nondet]-> Number Number))
(= (ia-choice $x) (ia-add (* (superpose (1 2)) $x) 3))
(: ia-mutable (-[det]-> Number Number))
(= (ia-mutable $x) (+ $x 1))
(: ia-mutable-use (-[det]-> Number Number))
(= (ia-mutable-use $x) (ia-mutable (- 1 $x)))
(: inline-arg-touch (-[det]-> Number Number))
!(import_prolog_function inline-arg-touch)
(: ia-effect (-[det]-> Number Number))
(= (ia-effect $x) (+ (inline-arg-touch 3) $x))
(: ia-effect-use (-[det]-> Number Number))
(= (ia-effect-use $x) (ia-effect (/ 1 $x)))
",_).

test(bayes_arguments_fuse_into_their_consumers) :-
    clause('ia-negative'(A,U,V,_),Body),
    once((sub_term(G1,Body),nonvar(G1),G1=(N is (1-U)*A))),
    once((sub_term(G2,Body),nonvar(G2),G2=(_ is N+(1-V)*(1-A)))),
    findall(E,(sub_term(G,Body),nonvar(G),G=(_ is E)),Es),length(Es,3),
    'ia-negative'(0.3,0.8,0.2,R),abs(R-0.0967741935483871)<1e-15.
test(floating_point_grouping) :- 'ia-group'(1.0e16,-1.0e16,1.0,1.0).
test(repeated_argument_is_computed_once) :-
    clause('ia-shared'(X,_),Body),
    findall(E,(sub_term(G,Body),nonvar(G),G=(_ is E)),[Minus,Square]),
    Minus =@= 1-X,Square=(_*_), 'ia-shared'(0.3,R),abs(R-0.49)<1e-15.
test(argument_can_fuse_into_an_unconditional_numeric_guard) :-
    clause('ia-sign-use'(X,_),Body),
    once((sub_term(G,Body),nonvar(G),G =@= (1/X>0))),
    'ia-sign-use'(2,1),'ia-sign-use'(-2,-1).
test(argument_in_guard_still_throws,[throws(error(evaluation_error(zero_divisor),_))]) :- 'ia-sign-use'(0,_).
test(outer_untaken_branch_defers_constant_argument_error) :- 'ia-deferred'(false,42).
test(outer_taken_branch_keeps_constant_argument_error,[throws(error(evaluation_error(zero_divisor),_))]) :- 'ia-deferred'(true,_).
test(untaken_use_keeps_argument_error,[throws(error(evaluation_error(zero_divisor),_))]) :- 'ia-branch'(0,_).
test(unused_argument_still_throws,[throws(error(evaluation_error(zero_divisor),_))]) :- 'ia-unused'(0,_).
test(argument_error_precedes_constant_head_mismatch,[throws(error(evaluation_error(zero_divisor),_))]) :- 'ia-constant-use'(0,_).
test(argument_error_precedes_output_constraint,[throws(error(evaluation_error(zero_divisor),_))]) :- 'ia-constrained'(0,_).
test(computed_output_keeps_its_constraint) :- 'ia-constrained'(2,0.5),\+ 'ia-constrained'(1,_).
test(argument_error_precedes_callee_effect) :-
    flag(inline_arg_touches,_,0),
    catch('ia-effect-use'(0,_),error(evaluation_error(zero_divisor),_),Caught=true),
    Caught==true,flag(inline_arg_touches,0,0).
test(source_variable_producer_stays_bound) :-
    G=(Temp is X+1),Body=[Out is Temp*2],
    fuse_inline_arguments([G],[Temp,X,Out],[],Body,Actual),Actual==[G|Body].
test(prior_observation_blocks_fusion) :-
    A=(_Seen=Temp),G=(Temp is X+1),Body=[Out is Temp*2],
    fuse_inline_arguments([A,G],[X,Out],[],Body,Actual),Actual==[A,G|Body].
test(failing_binding_is_a_barrier) :-
    G=(Temp is 1/X),Body=[2=3,Out is Temp*2],
    fuse_inline_arguments([G],[X,Out],[],Body,Actual),Actual==[G|Body].
test(observable_arithmetic_target_is_a_barrier) :-
    G=(Temp is 1/X),Body=[Bound is 3,Out is Temp*2],
    fuse_inline_arguments([G],[X,Out,Bound],[],Body,Actual),Actual==[G|Body].
test(foreign_call_is_a_barrier) :-
    G=(Temp is 1/X),Body=['inline-arg-touch'(3,Y),Out is Temp+Y],
    fuse_inline_arguments([G],[X,Out],[],Body,Actual),Actual==[G|Body].

test(nondeterministic_arguments_keep_all_answers) :-
    findall(R,'ia-choice'(4,R),[7,11]).
test(callee_mutation_rebuilds_the_fused_caller) :-
    'ia-mutable-use'(0.3,1.7),
    clause('ia-mutable'(_,_),_,Ref),
    retract(user:translated_from(Ref,[=,['ia-mutable',X],_])),
    assertz(user:translated_from(Ref,[=,['ia-mutable',X],
        [let,_Touch,['inline-arg-touch',3],[+,X,2]]])),
    notify_mutation(clause_changed('ia-mutable'/1,runtime)),
    flag(inline_arg_touches,_,0),'ia-mutable-use'(0.3,2.7),flag(inline_arg_touches,1,1).

:- end_tests(inline_arguments).
