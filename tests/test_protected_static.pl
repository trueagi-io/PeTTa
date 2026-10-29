:- ensure_loaded('../src/metta').
% Protect the imports under test after the runtime validates its own registry.
:- set_prolog_flag(protect_static_code,true).

% Under protect_static_code, clause/2 refuses static predicates. Inlining and
% proof shortcuts that read an imported body must then keep the call.
'protected-inc'(X,Y) :- Y is X+1.
'protected-constant'(-1.0Inf).
'protected-low'(X,B) :- (X<0->B=true;B=false).
'protected-spine'(Term,Args) :- Term=..[p|Args].

:- begin_tests(protected_static).
:- retractall(user:silent(_)),assertz(user:silent(true)).
:- process_metta_string("
(: protected-inc (-[det]-> Number Number))
!(import_prolog_function protected-inc)
(: protected-use (-[det]-> Number Number))
(= (protected-use $x) (* (protected-inc $x) 2))
(: protected-constant (-[det]-> Number))
(: protected-low (-[det]-> Number Bool))
!(import_prolog_function protected-constant)
!(import_prolog_function protected-low)
(: protected-constant-use (-[det]-> Number))
(= (protected-constant-use) (protected-constant))
(: protected-low-use (-[det]-> Number Number))
(= (protected-low-use $x) (if (protected-low $x) 0 1))
(: ProtectedArray (Foreign 0))
(: protected-spine (-[det]-> ProtectedArray (List Number)))
!(import_prolog_function protected-spine)
",_).

test(imported_body_is_unreadable) :-
    catch(clause('protected-inc'(_,_),_),error(permission_error(_,_,_),_),Caught=true),
    Caught==true.

test(arithmetic_wrapper_keeps_its_call) :-
    clause('protected-use'(_,_),Body),
    once((sub_term(G,Body),nonvar(G),functor(G,'protected-inc',2))),
    'protected-use'(4,10).

test(constant_and_comparison_keep_unreadable_calls) :-
    'protected-constant-use'(-1.0Inf),'protected-low-use'(-1,0),
    clause('protected-constant-use'(_),C),
    once((sub_term(G,C),compound(G),functor(G,'protected-constant',1))),
    clause('protected-low-use'(_,_),B),
    once((sub_term(H,B),compound(H),functor(H,'protected-low',2))).

% Without the body there is no proof; a det caller relying on one is rejected
% by the determinism check rather than by a permission error.
test(spine_proof_is_unavailable) :-
    catch((proper_list_output('protected-spine',1) -> R=yes ; R=no),E,R=E),
    R==no.

:- end_tests(protected_static).
