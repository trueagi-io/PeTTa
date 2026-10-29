:- ensure_loaded('../src/metta').

'static-excluded'(-1.0Inf).
'static-low'(X,B) :- (X < -1.0e308 -> B=true ; B=false).
'static-touch'(X,B) :- flag(static_touches,N,N+1),(X<0->B=true;B=false).
'static-pattern'(0,B) :- (B=true).

:- begin_tests(static_scalars).
:- retractall(user:silent(_)),assertz(user:silent(true)).
:- process_metta_string("
(: static-excluded (-[det]-> Number))
(: static-low (-[det]-> Number Bool))
(: static-touch (-[det]-> Number Bool))
(: static-pattern (-[semidet]-> Number Bool))
!(import_prolog_function static-excluded)
!(import_prolog_function static-low)
!(import_prolog_function static-touch)
!(import_prolog_function static-pattern)
(: static-data (-[det]-> Number))
(= (static-data) (static-excluded))
(: static-select (-[det]-> Number Number))
(= (static-select $x) (if (static-low $x) 0 1))
(: static-bool (-[det]-> Number Bool))
(= (static-bool $x) (static-low $x))
(: static-effect (-[det]-> Number Number))
(= (static-effect $x) (if (static-touch $x) 0 1))
(: static-pattern-use (-[semidet]-> Number Bool))
(= (static-pattern-use $x) (static-pattern $x))
",_).

test(infinite_constant_stays_data) :-
    'static-data'(-1.0Inf),\+ 'static-data'(0),
    clause('static-data'(_),Body),
    \+ (sub_term(G,Body),compound(G),functor(G,'static-excluded',1)).
test(imported_test_has_no_call_or_boolean_round_trip) :-
    clause('static-select'(X,_),Body),
    \+ (sub_term(G,Body),compound(G),functor(G,'static-low',2)),
    once((sub_term(G,Body),nonvar(G),G=(X < -1.0e308))),
    'static-select'(-1.0Inf,0),'static-select'(0,1).
test(returned_boolean_and_constraints) :-
    'static-bool'(-1.0Inf,true),'static-bool'(0,false),\+ 'static-bool'(0,true).
test(effectful_comparison_keeps_its_call) :-
    flag(static_touches,_,0),'static-effect'(-1,0),flag(static_touches,1,1),
    clause('static-effect'(_,_),Body),
    once((sub_term(G,Body),compound(G),functor(G,'static-touch',2))).
test(head_constraint_is_preserved) :-
    'static-pattern-use'(0,true),\+ 'static-pattern-use'(1,_).
test(result_read_by_test_is_not_reified_away) :-
    \+ reified_cond((R=false->R=true;R=false),R,_).
test(variable_true_arm_is_not_a_boolean_literal) :-
    Goal=(X>0->R=Value;R=false),\+ reified_cond(Goal,R,_),
    var(X),var(R),var(Value).
test(variable_false_arm_is_not_a_boolean_literal) :-
    Goal=(X>0->R=true;R=Value),\+ reified_cond(Goal,R,_),
    var(X),var(R),var(Value).
test(nested_boolean_condition_preserves_runtime_parameters) :-
    % A source parameter in an arm must not become a literal during codegen.
    % Inside an outer branch this used to crash while restoring Bool attrs;
    % outside one it silently constrained the generated predicate's head.
    process_metta_string("
(: static-choice (-[det]-> Bool Bool Bool))
(= (static-choice $condition $fallback)
   (if (if $condition true $fallback) true false))
(: static-nested-choice (-[det]-> Bool Bool Bool Number))
(= (static-nested-choice $gate $condition $fallback)
   (if $gate (if (if $condition true $fallback) 1 2) 3))
(: static-choice-then (-[det]-> Bool Bool Bool))
(= (static-choice-then $condition $selected)
   (if (if $condition $selected false) true false))
",_),
    forall((member(C,[true,false]),member(F,[true,false])),
      ((C==true->Expected=true;Expected=F),
       findall(R,'static-choice'(C,F,R),[Expected]),
       (C==true->ThenExpected=F;ThenExpected=false),
       findall(R,'static-choice-then'(C,F,R),[ThenExpected]),
       forall(member(G,[true,false]),
         ((G==false->N=3;Expected==true->N=1;N=2),
          findall(R,'static-nested-choice'(G,C,F,R),[N]))))).
test(dependencies_record_both_new_scalar_forms) :-
    clause('static-data'(_),_,R1),compiled_deps(R1,_,_,D1),memberchk(clause_set('static-excluded'/0),D1),
    clause('static-select'(_,_),_,R2),compiled_deps(R2,_,_,D2),memberchk(clause_set('static-low'/1),D2).
test(reimport_rebuilds_constant_caller) :-
    abolish(user:'static-excluded'/1),assertz(user:'static-excluded'(42)),
    import_prolog_function('static-excluded',true),'static-data'(42),\+ 'static-data'(-1.0Inf).
test(reimport_restores_effectful_comparison) :-
    abolish(user:'static-low'/2),
    assertz(user:('static-low'(X,B):-flag(static_touches,N,N+1),(X<1->B=true;B=false))),
    import_prolog_function('static-low',true),
    flag(static_touches,_,0),'static-select'(0,0),flag(static_touches,1,1).

:- end_tests(static_scalars).
