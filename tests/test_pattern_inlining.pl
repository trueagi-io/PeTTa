:- ensure_loaded('../src/metta').

pattern_observed_arg(I,C,V) :- flag(pattern_reads,N,N+1),arg(I,C,V).
'pattern-mutate'(Value,true) :- arg(2,Value,Tail),nb_setarg(1,Tail,7.0).

:- dynamic user:metta_memoized_dispatch_call/4.

:- begin_tests(pattern_inlining).
:- retractall(user:silent(_)),assertz(user:silent(true)).
:- process_metta_string("
(: PArray (Foreign 0))
(: PValue Type)
(: PPoint (-[det]-> Number PValue))
(: PCloud (-[det]-> PArray PValue))
(: PEmpty PValue)
(: arg (-[det]-> Number PArray Number))
!(import_prolog_function arg)
(: pattern-read (-[semidet]-> PValue Number Number Number))
(= (pattern-read $v $max $i)
   (case $v (((PPoint $w) 1.0) ((PCloud $cloud) (/ (arg $i $cloud) $max)))))
(: pattern-use (-[semidet]-> PValue Number Number Number))
(= (pattern-use $v $max $i) (pattern-read $v $max $i))
(: pattern-return (-[semidet]-> PValue Number))
(= (pattern-return $v) (case $v (((PPoint $x) $x))))
(: pattern-return-use (-[semidet]-> PValue Number))
(= (pattern-return-use $v) (pattern-return $v))
(: pattern-priority (-[semidet]-> PValue Number))
(= (pattern-priority $v)
   (case $v (((PPoint $x) (/ 1 $x)) ((PPoint $y) 42))))
(: pattern-priority-use (-[semidet]-> PValue Number))
(= (pattern-priority-use $v) (pattern-priority $v))
(: pattern-constrained (-[semidet]-> PValue Number))
(= (pattern-constrained $v) (let 42 (pattern-priority $v) 42))
(: pattern-build (-[semidet]-> PValue Number))
(= (pattern-build $v) (case $v (((PPoint 0.0) 1.0))))
(: pattern-mutate (-[det]-> PValue Bool))
!(import_prolog_function pattern-mutate)
(: pattern-two-builds (-[semidet]-> (PValue PValue)))
(= (pattern-two-builds)
   (let* (($_one (pattern-build $first))
          ($_change (pattern-mutate $first))
          ($_two (pattern-build $second)))
      (data $first $second)))
(: pattern-head (-[semidet]-> PValue PValue Number))
(= (pattern-head (PPoint $x) $v) (case $v (((PPoint $y) (+ $x $y)))))
(: pattern-head-use (-[semidet]-> PValue PValue Number))
(= (pattern-head-use $a $b) (pattern-head $a $b))
(: pattern-repeat (-[semidet]-> PValue PValue Number))
(= (pattern-repeat $v $v) (case $v (((PPoint $x) 1.0))))
(: pattern-repeat-use (-[semidet]-> PValue PValue Number))
(= (pattern-repeat-use $a $b) (pattern-repeat $a $b))
(: m-pattern-scale (-[det]-> Number Number))
(= (m-pattern-scale $x) (+ $x 1))
(: z-pattern-read (-[semidet]-> PValue Number Number Number))
(= (z-pattern-read $v $max $i)
   (case $v (((PPoint $x) (m-pattern-scale $x))
             ((PCloud $cloud) (let $x (/ (arg $i $cloud) $max) (m-pattern-scale $x))))))
(: a-pattern-use (-[semidet]-> PValue Number Number Number))
(= (a-pattern-use $v $max $i) (z-pattern-read $v $max $i))
",_).

test(constructor_helper_call_is_removed) :-
    clause('pattern-use'(_,_,_,_),Body),
    \+ (sub_term(G,Body),nonvar(G),functor(G,'pattern-read',4)),
    'pattern-use'(['PCloud',p(2.0,6.0)],2.0,2,3.0).

test(point_branch_does_not_read_an_array) :- 'pattern-use'(['PPoint',0.4],0,999,1.0).
test(missing_constructor_fails) :- \+ 'pattern-use'('PEmpty',1,1,_).
test(returned_field_constraint_is_preserved) :-
    'pattern-return-use'(['PPoint',3.0],3.0),
    \+ 'pattern-return-use'(['PPoint',3.0],4.0).
test(first_match_keeps_one_solution) :-
    findall(R,'pattern-priority-use'(['PPoint',2.0],R),Rs),Rs==[0.5].
test(failed_selected_branch_does_not_try_a_later_match) :-
    \+ 'pattern-constrained'(['PPoint',2.0],_).
test(selected_branch_error_is_preserved,
     [throws(error(evaluation_error(zero_divisor),_))]) :-
    'pattern-priority-use'(['PPoint',0.0],_).
test(array_error_timing_is_preserved,
     [throws(error(evaluation_error(zero_divisor),_))]) :-
    'pattern-use'(['PCloud',p(2.0)],0.0,1,_).
test(input_binding_remains_at_the_call) :-
    'pattern-use'(V,0,999,1.0),V=['PPoint',Field],var(Field).
test(separate_calls_keep_separate_constructor_cells) :-
    'pattern-two-builds'([['PPoint',7.0],['PPoint',0.0]]).
test(callee_dependencies_are_inherited) :-
    clause('pattern-use'(_,_,_,_),_,Ref),compiled_deps(Ref,_,_,Ds),
    memberchk(clause_set('pattern-read'/3),Ds),memberchk(decl(arg/2),Ds).
test(oracle_keeps_the_original_call_boundary) :-
    setup_call_cleanup(
        (retractall(user:oracle_det_mode(_)),assertz(user:oracle_det_mode(true))),
        (Args=[_,_,_],append(Args,[Out],All),Call=..['pattern-read'|All],
         \+ compiled_inline('pattern-read',Args,Out,Call,_)),
        (retractall(user:oracle_det_mode(_)),assertz(user:oracle_det_mode(false)))).

% Rebuild a caller before its callee after changing a transitive numeric helper.
% Both consumers depend on m-pattern-scale; lexical order puts a-pattern-use
% before z-pattern-read. Copying z's old executable body would return 4, not 6.
test(transitive_mutation_does_not_embed_a_stale_body) :-
    clause('a-pattern-use'(_,_,_,_),Before,CallerRef),
    \+ (sub_term(G,Before),nonvar(G),functor(G,'z-pattern-read',4)),
    compiled_deps(CallerRef,_,_,CallerDeps),
    memberchk(clause_set('m-pattern-scale'/1),CallerDeps),
    'a-pattern-use'(['PCloud',p(3.0)],1.0,1,4.0),
    clause('m-pattern-scale'(_,_),_,Ref),
    retract(user:translated_from(Ref,[=,['m-pattern-scale',X],_])),
    assertz(user:translated_from(Ref,[=,['m-pattern-scale',X],[*,X,2]])),
    notify_mutation(clause_changed('m-pattern-scale'/1,runtime)),
    'a-pattern-use'(['PCloud',p(3.0)],1.0,1,6.0),
    'z-pattern-read'(['PCloud',p(3.0)],1.0,1,6.0),
    forall(member(F/A,['m-pattern-scale'/2,'z-pattern-read'/4,'a-pattern-use'/4]),
           (functor(H,F,A),clause(H,_,Current),\+ compiled_body_stale(Current))).

test(patterned_heads_keep_the_original_match) :-
    clause('pattern-head-use'(_,_,_),Body),
    \+ (sub_term(G,Body),nonvar(G),functor(G,'pattern-head',3)),
    'pattern-head-use'(['PPoint',2],['PPoint',3],5).

test(repeated_head_parameters_keep_the_original_constraint) :-
    clause('pattern-repeat-use'(_,_,_),Body),
    \+ (sub_term(G,Body),nonvar(G),functor(G,'pattern-repeat',3)),
    'pattern-repeat-use'(['PPoint',2],['PPoint',2],1.0),
    \+ 'pattern-repeat-use'(['PPoint',2],['PPoint',3],_).

test(custom_primitive_dispatch_is_preserved) :-
    setup_call_cleanup(
        assertz((user:metta_memoized_dispatch_call(arg,[I,C],V,
                                                pattern_observed_arg(I,C,V))),Hook),
        (process_metta_string("
(: pattern-hook-read (-[semidet]-> PValue Number Number Number))
(= (pattern-hook-read $v $max $i)
   (case $v (((PPoint $w) 1.0) ((PCloud $cloud) (/ (arg $i $cloud) $max)))))
(: pattern-hook-use (-[semidet]-> PValue Number Number Number))
(= (pattern-hook-use $v $max $i) (pattern-hook-read $v $max $i))
",_),
         flag(pattern_reads,_,0),'pattern-hook-use'(['PCloud',p(6.0)],2,1,3.0),
         flag(pattern_reads,N,N),N==1),
        erase(Hook)).

:- end_tests(pattern_inlining).
