:- ensure_loaded('../src/metta').

:- begin_tests(alias_folding).

:- retractall(user:silent(_)), assertz(user:silent(true)).
:- process_metta_string("
(: AliasChoice Type)
(: Chosen (-[det]-> Atom AliasChoice))
(: Unchosen (-[det]-> Atom AliasChoice))
(: alias-branch (-[det]-> Bool Atom AliasChoice))
(= (alias-branch $flag $value)
   (if $flag (let $local $value (Chosen $local)) (Unchosen $local)))
(: alias-arithmetic (-[det]-> Number Number))
(= (alias-arithmetic $x) (let $local $x (+ $local 1)))
", _).

% The generated private alias disappears; the source equation remains an
% equation with two distinct variables, available for later recompilation.
test(source_equation_is_not_aliased) :-
    Input=[=,['alias-arithmetic',X],[let,Local,X,[+,Local,1]]],
    translate_clause(Input,Clause,false,_),
    X \== Local, copy_term_nat(Clause,Plain),
    Plain = (Head :- Body), Head =.. ['alias-arithmetic',A,R],
    Body =@= (!, R is A+1).

test(untaken_branch_variable_remains_fresh) :-
    'alias-branch'(false,payload,['Unchosen',Local]), var(Local).

test(taken_branch_keeps_its_value) :-
    'alias-branch'(true,payload,['Chosen',payload]).

test(head_constraint_is_preserved) :-
    Before=pair(X,Y), Goal=(X=Y, X=one),
    fold_clause_bindings(Goal,Before,Folded),
    X \== Y, Folded == Goal,
    \+ (X=one,Y=two,call(Folded)).

test(earlier_identity_observation_is_preserved) :-
    Goal=(X\==Y, X=Y),
    fold_clause_bindings(Goal,none,Folded),
    X \== Y, call(Folded).

test(disjunction_does_not_hoist_shared_aliases) :-
    Goal=((Local=X,X=left);(Local=right,X=Local)),
    fold_clause_bindings(Goal,result(X),Folded),
    X \== Local, findall(X,call(Folded),Values), Values == [left,right].

test(condition_does_not_bind_else_variables) :-
    Goal=(fail -> Local=X ; var(Local)),
    fold_clause_bindings(Goal,result(X),Folded),
    X \== Local, X=payload, call(Folded).

test(soft_cut_does_not_bind_else_variables) :-
    Goal=(fail *-> Local=X ; var(Local)),
    fold_clause_bindings(Goal,result(X),Folded),
    X \== Local, X=payload, call(Folded).

test(unused_ground_binding_disappears) :-
    fold_clause_bindings((_Unused=true,member(a,[a])),none,Folded),
    Folded == member(a,[a]).

test(used_ground_binding_is_kept) :-
    Goal=(X=ready,nonvar(X)),
    fold_clause_bindings(Goal,none,Folded),
    var(X), Folded == Goal, call(Folded).

test(compound_alias_keeps_runtime_sharing) :-
    Goal=(Local=Source,nb_setarg(1,Local,changed),arg(1,Source,V)),
    fold_clause_bindings(Goal,result(Source,V),Folded),
    Source=box(initial),call(Folded),V == changed.

test(cyclic_binding_is_not_discarded) :-
    Goal=(X=loop(X),cyclic_term(X)),
    fold_clause_bindings(Goal,none,Folded),
    var(X), Folded == Goal, call(Folded).

test(meta_call_is_an_optimization_boundary) :-
    Goal=once((Local=Source,var(Local))),
    fold_clause_bindings(Goal,result(Source),Folded),
    Local \== Source, Folded == Goal.

test(branch_cut_remains_in_its_scope) :-
    Goal=((Local=X,!,X=first);X=second),
    fold_clause_bindings(Goal,result(X),Folded),
    findall(X,call(Folded),Values), Values == [first], var(Local).

:- end_tests(alias_folding).
