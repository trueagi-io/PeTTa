:- ensure_loaded('../src/metta').

'beta-touch'(X,Y) :- flag(beta_touches,N,N+1),Y is X+1.
'beta-observe'(X,Cell,X) :- var(Cell),flag(beta_observers,N,N+1).
'beta-abs-hook'(_,123).
:- dynamic user:metta_memoized_dispatch_call/4.

:- begin_tests(compiled_inlining).
:- retractall(user:silent(_)),assertz(user:silent(true)).
:- process_metta_string("
(: BetaRecord Type)
(: BetaRecord (-[det]-> Number Number BetaRecord))
(: beta-touch (-[det]-> Number Number))
(: beta-observe (-[semidet]-> Number BetaRecord Number))
!(import_prolog_function beta-touch)
!(import_prolog_function beta-observe)
(: beta-math (-[det]-> Number Number))
(= (beta-math $x) (+ (sqrt-math (abs-math $x)) (pow-math 2 $x)))
(: beta-effect (-[det]-> Number Number))
(= (beta-effect $x) (+ (sqrt-math $x) (beta-touch 1)))
(: beta-unused (-[det]-> Number Number))
(= (beta-unused $x) (if (> $x 0) (sqrt-math -1) (exp $x)))
(: beta-pair (-[det]-> BetaRecord Number (Number Number)))
(= (beta-pair (BetaRecord $a $b) $x)
   ((+ $a (sqrt-math $x)) (+ $b (abs-math $x))))
(: beta-total (-[det]-> BetaRecord Number Number))
(= (beta-total $record $x)
   (let ($a $b) (beta-pair $record $x) (+ $a $b)))
(: beta-pattern (-[semidet]-> Number BetaRecord Number))
(= (beta-pattern $n (BetaRecord 7 $x)) (+ $n 7))
(: beta-pattern-use (-[semidet]-> BetaRecord Number))
(= (beta-pattern-use $record) (beta-pattern (beta-observe 1 $record) $record))
(: beta-pattern-effect (-[semidet]-> BetaRecord Number))
(= (beta-pattern-effect $record) (beta-pattern (beta-touch 1) $record))
(: beta-loop (-[det]-> Number Number))
(= (beta-loop $x) (if (<= $x 0) 1 (beta-loop (- $x 1))))
(: beta-loop-use (-[det]-> Number Number))
(= (beta-loop-use $x) (beta-loop $x))
",_).

test(native_math_tree) :-
    clause('beta-math'(X,Out),Body),
    findall(G,(sub_term(G,Body),nonvar(G),G=(_ is _)),[Arithmetic]),
    Arithmetic =@= (Out is sqrt(abs(X))+2**X),
    'beta-math'(-4,R),R=:=2.0625.

test(math_error_precedes_effect) :-
    flag(beta_touches,_,0),
    catch('beta-effect'(-1,_),error(evaluation_error(undefined),_),Caught=true),
    Caught==true,flag(beta_touches,N,N),N==0.

test(math_success_keeps_one_effect) :-
    flag(beta_touches,_,0),'beta-effect'(4,4.0),flag(beta_touches,N,N),N==1.

test(unused_literal_math_error_is_deferred) :- 'beta-unused'(0,1.0).
test(selected_literal_math_error,
     [throws(error(evaluation_error(undefined),_))]) :- 'beta-unused'(1,_).

test(structured_numeric_tuple_is_inlined) :-
    clause('beta-total'(_,_,_),Body),
    \+ (sub_term(G,Body),nonvar(G),functor(G,'beta-pair',3)),
    'beta-total'(['BetaRecord',2,3],4,11.0).

test(structured_input_binding_keeps_its_timing) :-
    flag(beta_observers,_,0),'beta-pattern-use'(Record,8),
    Record=['BetaRecord',7,Field],var(Field),
    flag(beta_observers,N,N),N==1.

test(head_mismatch_follows_effectful_argument) :-
    flag(beta_touches,_,0),\+ 'beta-pattern-effect'(['BetaRecord',8,9],_),
    flag(beta_touches,N,N),N==1.

test(recursive_helper_retains_its_boundary) :-
    clause('beta-loop-use'(_,_),Body),
    sub_term(G,Body),nonvar(G),functor(G,'beta-loop',2),!,
    'beta-loop-use'(3,1).

test(explicit_cut_retains_its_scope) :- \+ inline_body_budget((true,!),other,64,_).
test(meta_call_retains_its_scope) :- \+ inline_body_budget(once(true),other,64,_).

test(native_math_inherits_dependencies) :-
    clause('beta-math'(_,_),_,Ref),compiled_deps(Ref,_,_,Deps),
    forall(member(F/N,['sqrt-math'/1,'abs-math'/1,'pow-math'/2]),
           memberchk(clause_set(F/N),Deps)).

test(math_memo_override_is_preserved) :-
    setup_call_cleanup(
        assertz(user:metta_memoized_dispatch_call('abs-math',[X],V,'beta-abs-hook'(X,V)),Hook),
        (process_metta_string("
(: beta-memo-math (-[det]-> Number Number))
(= (beta-memo-math $x) (+ (abs-math $x) 1))
",_),'beta-memo-math'(-4,124)),
        erase(Hook)).

:- end_tests(compiled_inlining).
