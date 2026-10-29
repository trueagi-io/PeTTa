:- ensure_loaded('../src/metta').

% The argument observer detects entry bindings incorrectly hoisted across an
% effectful argument. The memo hook must override the otherwise inlineable body.
'pre-tuple-input'(X, Field, X) :- var(Field), flag(tuple_inputs, N, N+1).
:- multifile metta_memoized_dispatch_call/4.
metta_memoized_dispatch_call('memo-pair', [X], Out, 'memo-pair-value'(X,Out)).
'memo-pair-value'(X, [X,100]).

:- begin_tests(tuple_lowering).

:- retractall(user:silent(_)), assertz(user:silent(true)).
:- process_metta_string("
(: tuple-values (-[det]-> Number (Number Number)))
(= (tuple-values $x) ($x (+ $x 1)))
(: tuple-total (-[det]-> Number Number))
(= (tuple-total $x) (let ($a $b) (tuple-values $x) (+ $a $b)))
(: arithmetic-pair (-[det]-> Number Number (Number Number)))
(= (arithmetic-pair $x $y) ((+ (* $x $y) 1) (- 1 $x)))
(: arithmetic-pair-total (-[det]-> Number Number Number))
(= (arithmetic-pair-total $x $y)
   (let ($a $b) (arithmetic-pair $x $y) (+ $a $b)))
(: arithmetic-pair-escape (-[det]-> Number Number (Number Number)))
(= (arithmetic-pair-escape $x $y) (arithmetic-pair $x $y))
(: throwing-pair (-[det]-> Number (Number Number)))
(= (throwing-pair $x) ((/ 1 $x) (+ $x 1)))
(: throwing-pair-total (-[det]-> Number Number))
(= (throwing-pair-total $x) (let ($a $b) (throwing-pair $x) (+ $a $b)))
(: pre-tuple-input (-[det]-> Number Atom Number))
!(import_prolog_function pre-tuple-input)
(: constant-pair (-[det]-> Number (Number Number)))
(= (constant-pair $x) (1 (+ $x 2)))
(: observed-pair-total (-[det]-> Number Number))
(= (observed-pair-total $x)
   (let ($a $b) (constant-pair (pre-tuple-input $x $a)) (+ $a $b)))
(: memo-pair (-[det]-> Number (Number Number)))
(= (memo-pair $x) ((+ $x 1) (+ $x 2)))
(: memo-pair-total (-[det]-> Number Number))
(= (memo-pair-total $x) (let ($a $b) (memo-pair $x) (+ $a $b)))
(: tuple-inc (-[det]-> Number Number))
(= (tuple-inc $x) (+ $x 1))
(: functional-pattern (-[semidet]-> Number Number))
(= (functional-pattern $x) (let (tuple-inc $x) 3 $x))
", _).

test(fresh_numeric_destructuring_uses_no_reducer) :-
    clause('tuple-total'(_,_),Body),
    \+ (sub_term(G,Body),nonvar(G),functor(G,reduce,2)),
    'tuple-total'(2,R),R =:= 5.

test(literal_functional_pattern_still_executes) :-
    'functional-pattern'(2,2),\+ 'functional-pattern'(1,_).

test(fresh_tuple_is_constructed_at_its_original_point) :-
    Goal=(reduce([X,Y],Pair),var(X),Pair=[one,two]),
    fold_clause_bindings(Goal,none,Folded),
    var(X),var(Y),var(Pair),
    Folded == (fresh_data_list([X,Y],Pair),var(X),Pair=[one,two]),
    call(Folded),X==one,Y==two.

test(tuple_object_sharing_is_preserved) :-
    Goal=(reduce([X,Y],Pair),nb_setarg(1,Pair,changed),arg(1,Pair,Seen)),
    fold_clause_bindings(Goal,result(Seen),Folded),
    var(Pair),call(Folded),Seen==changed,var(X),var(Y).

test(head_visible_field_keeps_dispatch) :-
    Goal=reduce([F,X],Pair),
    fold_clause_bindings(Goal,input(F),Folded),
    Folded == Goal, F='tuple-inc',X=2,call(Folded),Pair=:=3.

test(earlier_field_observation_keeps_dispatch) :-
    Goal=(var(F),reduce([F,X],Pair)),
    fold_clause_bindings(Goal,none,Folded),
    Folded == Goal,var(F),var(X),var(Pair).

test(existing_result_keeps_dispatch) :-
    Goal=reduce([F,X],Pair),
    fold_clause_bindings(Goal,result(Pair),Folded),
    Folded == Goal,var(F),var(X).

test(result_occurring_in_tuple_keeps_cycle_check) :-
    Goal=reduce([Pair,X],Pair),
    fold_clause_bindings(Goal,none,Folded),
    Folded == Goal,\+ call(Folded),var(Pair),var(X).

test(cyclic_literal_field_keeps_cycle_check) :-
    Cyclic=loop(Cyclic),Goal=reduce([F,Cyclic],Pair),
    fold_clause_bindings(Goal,none,Folded),
    Folded == Goal,\+ call(Folded),var(F),var(Pair).

test(open_list_spine_is_not_lowered) :-
    Goal=reduce([F|Tail],Pair),
    fold_clause_bindings(Goal,none,Folded),
    Folded == Goal,var(F),var(Tail),var(Pair).

test(sibling_branch_fields_are_not_narrowed) :-
    Goal=(fail -> reduce([F,X],Pair) ; (F='tuple-inc',X=2)),
    fold_clause_bindings(Goal,none,Folded),
    Folded == Goal,var(Pair).

test(nested_fresh_data_preserves_variables) :-
    Goal=(reduce([F,box(X),X],Pair),Pair=[tag,box(value),value]),
    fold_clause_bindings(Goal,none,Folded),
    call(Folded),F==tag,X==value.

test(duplicate_fields_preserve_equality_constraint) :-
    Goal=(reduce([X,X],Pair),Pair=[one,two]),
    fold_clause_bindings(Goal,none,Folded),
    var(X),\+ call(Folded).

test(producer_side_effect_order_is_preserved) :-
    Goal=(reduce([X,Y],Pair),var(X),X=left,Y=right,Pair=[left,right]),
    fold_clause_bindings(Goal,none,Folded),
    call(Folded).

test(arithmetic_tuple_is_scalar_replaced) :-
    clause('arithmetic-pair-total'(X,Y,Out),Body),
    Body =@= (!,A is X*Y+1,B is 1-X,Out is A+B),
    'arithmetic-pair-total'(2,3,R),R==6.

test(escaping_arithmetic_tuple_retains_its_value) :-
    'arithmetic-pair-escape'(0.2,0.5,R), R == [1.1,0.8].

test(inlined_arithmetic_error_is_preserved,
     [throws(error(evaluation_error(zero_divisor),_))]) :-
    'throwing-pair-total'(0,_).

test(bare_parameter_tuple_keeps_its_value) :-
    'tuple-total'(2,5),
    'tuple-values'(2,Pair),Pair==[2,3],
    nb_setarg(1,Pair,changed),'tuple-values'(2,Fresh),Fresh==[2,3].

test(unbound_fields_retain_their_cells) :-
    Goal=(reduce([X,Y],Pair),Pair=[D,E],var(D),
          nb_setarg(1,Pair,changed),var(X),Y=E),
    fold_clause_bindings(Goal,none,Folded),
    call(Folded),var(X),var(Y).

test(escaping_pair_is_not_scalar_replaced) :-
    Goal=(reduce([X,Y],Pair),Pair=[D,E],D is 1+2,E is 2+3,nb_setarg(1,Pair,changed)),
    fold_clause_bindings(Goal,none,Folded),
    call(Folded),X==3,Y==5,Pair==[changed,5].

test(effect_between_assignments_keeps_tuple) :-
    Goal=(reduce([X,Y],Pair),Pair=[D,E],D is 1+2,var(Y),E is 2+3),
    fold_clause_bindings(Goal,none,Folded),
    call(Folded),X==3,Y==5.

test(argument_effect_precedes_entry_field_binding) :-
    flag(tuple_inputs,_,0), 'observed-pair-total'(3,6),
    flag(tuple_inputs,N,N),N==1.

test(memoization_dispatch_is_not_bypassed) :-
    'memo-pair-total'(2,102).

test(oracle_call_boundary_is_retained) :-
    setup_call_cleanup(
        (retractall(user:oracle_det_mode(_)),assertz(user:oracle_det_mode(true))),
        (Args=[_,_],append(Args,[Out],All),Call=..['arithmetic-pair'|All],
         \+ numeric_inline('arithmetic-pair',Args,Out,Call,_)),
        (retractall(user:oracle_det_mode(_)),assertz(user:oracle_det_mode(false)))).

test(variable_operator_is_not_specialized) :-
    \+ numeric_expr([Op,1,2],_,_), var(Op).

test(inlined_producer_records_transitive_arithmetic_dependencies) :-
    clause('arithmetic-pair-total'(_,_,_),_,Ref),
    compiled_deps(Ref,_,_,Deps),
    memberchk(clause_set('arithmetic-pair'/2),Deps),
    forall(member(Op,[+,-,*]),
           (memberchk(clause_set(Op/2),Deps),
            memberchk(decl(Op/2),Deps),memberchk(effect(Op/2),Deps))).

% Stage a changed source while its previous executable clause is still live.
% Rebuild through the real dependency graph: callers must consume current
% source even if the graph visits them before the producer.
test(source_mutation_rebuilds_inlined_callers) :-
    process_metta_string("
(: mutable-pair (-[det]-> Number (Number Number)))
(= (mutable-pair $x) ((+ $x 1) (* $x 2)))
(: mutable-pair-total (-[det]-> Number Number))
(= (mutable-pair-total $x) (let ($a $b) (mutable-pair $x) (+ $a $b)))
",_),
    'mutable-pair-total'(3,10),
    clause('mutable-pair'(_,_),_,Ref),
    retract(user:translated_from(Ref,[=,['mutable-pair',X],_])),
    assertz(user:translated_from(Ref,[=,['mutable-pair',X],[[+,X,5],[*,X,3]]])),
    notify_mutation(clause_changed('mutable-pair'/1,runtime)),
    'mutable-pair'(3,[8,9]),'mutable-pair-total'(3,17).

:- end_tests(tuple_lowering).
