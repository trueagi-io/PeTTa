:- ensure_loaded('../src/metta').

'fusion-touch'(X, X) :- flag(fusion_touches, N, N + 1).

:- begin_tests(arithmetic_fusion).

:- retractall(silent(_)), assertz(silent(true)).
:- process_metta_string("
(: fusion-touch (-[det]-> Number Number))
!(import_prolog_function fusion-touch)
(: fusion-mp (-[det]-> Number Number Number Number))
(= (fusion-mp $x $y $z) (+ (* $y $x) (* $z (- 1 $x))))
(: fusion-associated (-[det]-> Number Number Number Number))
(= (fusion-associated $x $y $z) (+ (+ $x $y) $z))
(: fusion-effect-order (-[det]-> Number Number))
(= (fusion-effect-order $x) (+ (/ 1 $x) (fusion-touch 3)))
(: fusion-shared (-[det]-> Number Number))
(= (fusion-shared $x) (let $t (/ 10 $x) (+ $t $t)))
(: fusion-unused (-[det]-> Number Number))
(= (fusion-unused $x) (progn (/ 1 $x) (+ $x 1)))
(: fusion-bound (-[semidet]-> Number Number Number))
(= (fusion-bound $x $expected) (let $expected (+ $x 1) (* $expected 2)))
(: fusion-condition (-[det]-> Number Number Number))
(= (fusion-condition $x $y) (if (> $x (+ (* $y $y) 1)) 7 9))
(: fusion-condition-use (-[det]-> Number Number Number))
(= (fusion-condition-use $x $y) (fusion-condition $x $y))
(: fusion-identity (-[det]-> Number Bool))
(= (fusion-identity $x) (== (+ $x 1) 3))
(: fusion-choices (-[nondet]-> Number Number))
(= (fusion-choices $x) (+ (* (superpose (1 2)) $x) 3))
", _).

native_expressions(Body, Exprs) :-
    findall(Expr, (sub_term(G,Body), nonvar(G), G=(_ is Expr)), Exprs).

test(one_native_expression_preserves_tree) :-
    clause('fusion-mp'(X,Y,Z,Out),Body),
    findall(G,(sub_term(G,Body),nonvar(G),G=(_ is _)),Goals),
    Goals = [Is], Is =@= (Out is Y*X+Z*(1-X)),
    'fusion-mp'(0.3,0.8,0.2,R), R =:= 0.38.

test(floating_point_association) :-
    'fusion-associated'(1.0e16,-1.0e16,1.0,R), R == 1.0.

test(error_precedes_later_effect) :-
    flag(fusion_touches,_,0),
    catch('fusion-effect-order'(0,_),error(evaluation_error(zero_divisor),_),Caught=true),
    Caught == true, flag(fusion_touches,N,N), N == 0.

test(successful_effect_occurs_once) :-
    flag(fusion_touches,_,0), 'fusion-effect-order'(2,R), R =:= 3.5,
    flag(fusion_touches,N,N), N == 1.

test(shared_result_is_not_recomputed) :-
    clause('fusion-shared'(_,_),Body), native_expressions(Body,Exprs),
    length(Exprs,2), 'fusion-shared'(2,R), R =:= 10.

test(discarded_computation_still_throws,
     [throws(error(evaluation_error(zero_divisor),_))]) :- 'fusion-unused'(0,_).

test(source_binding_remains_a_constraint) :-
    'fusion-bound'(2,3,R), R =:= 6, \+ 'fusion-bound'(2,4,_).

test(nondeterministic_operands_keep_every_answer, Results == [7,11]) :-
    findall(R,'fusion-choices'(4,R),Results).

% A prior reference to an arithmetic result is a binding barrier, even if it
% happens to be an alias. Do not hide that alias by substituting the RHS.
test(prior_alias_is_preserved) :-
    Inner=[Visible=Temp,Temp is X+1],
    fuse_arithmetic_goals(Inner,[X],[Out is Temp*2],Goals),
    Goals == [Visible=Temp,Temp is X+1,Out is Temp*2].

% Source variables may escape through a result or a foreign call. Inlining
% their producer must never turn a bound source variable into a fresh one.
test(source_variable_producer_is_preserved) :-
    fuse_arithmetic_goals([Temp is X+1],[Temp,X],[Out is Temp*2],Goals),
    Goals == [Temp is X+1,Out is Temp*2].

test(multiply_used_temporary_is_preserved) :-
    fuse_arithmetic_goals([Temp is X/2],[X],[Out is Temp+Temp],Goals),
    Goals == [Temp is X/2,Out is Temp+Temp].

test(numeric_condition_keeps_its_expression_tree) :-
    clause('fusion-condition'(X,Y,_),Body),
    once((sub_term(G,Body),nonvar(G),G =@= (X>Y*Y+1))),
    'fusion-condition'(6,2,7),'fusion-condition'(5,2,9),
    'fusion-condition-use'(6,2,7),'fusion-condition-use'(5,2,9).
test(identity_does_not_compare_expression_syntax) :-
    'fusion-identity'(2,true),'fusion-identity'(3,false).
test(comparison_keeps_source_bindings) :-
    fuse_numeric_comparison([T is X+1],[T,X],(T>0),Kept,Test),
    Kept==[T is X+1],Test==(T>0).
test(comparison_does_not_cross_an_effect) :-
    Gs=[T is 1/X,'fusion-touch'(3,Y)],
    fuse_numeric_comparison(Gs,[X],(T>Y),Kept,Test),Kept==Gs,Test==(T>Y).
test(comparison_preserves_observed_alias) :-
    Gs=[Seen=T,T is X+1],
    fuse_numeric_comparison(Gs,[X],(T>0),Kept,Test),Kept==Gs,Test==(T>0).
test(comparison_keeps_repeated_temporary) :-
    Gs=[T is X+1],
    fuse_numeric_comparison(Gs,[X],(T>T),Kept,Test),Kept==Gs,Test==(T>T).

:- end_tests(arithmetic_fusion).
