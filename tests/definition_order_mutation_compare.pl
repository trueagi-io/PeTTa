% Compare retained forwarders with fresh translations after small arity edits.
:- ensure_loaded('../src/metta').
:- use_module(library(random)).

target(def_mut_target).

equation(N,Id,[=,[F|Args],[def_mut_answer,N,Id|Args]]) :- target(F),length(Args,N).
add_equation(N,Id) :- equation(N,Id,Term),'add-atom'('&self',Term,_).
remove_equation(N,Id) :- equation(N,Id,Term),'remove-atom'('&self',Term,_).
remove_arity(N) :- target(F),length(Args,N),'remove-atom'('&self',[=,[F|Args],_],_).

mutate(Step,Rows,Next,Operation) :-
    random_between(0,5,Kind),mutation(Kind,Step,Rows,Next,Operation).
mutation(0,I,Rows,[N-I|Rows],add(N,I)) :- random_between(0,3,N),add_equation(N,I).
mutation(1,I,Rows,Next,Op) :- mutation(0,I,Rows,Next,Op).
mutation(2,I,[],Next,Op) :- !,mutation(0,I,[],Next,Op).
mutation(2,_,Rows,[N-Id|Rows],duplicate(N,Id)) :- random_member(N-Id,Rows),add_equation(N,Id).
mutation(3,I,[],Next,Op) :- !,mutation(0,I,[],Next,Op).
mutation(3,_,Rows,Next,remove(N,Id)) :-
    random_member(N-Id,Rows),remove_equation(N,Id),exclude(==(N-Id),Rows,Next).
mutation(4,_,Rows,Next,remove_arity(N)) :-
    random_between(0,3,N),remove_arity(N),exclude(same_arity(N),Rows,Next).
mutation(5,_,_,[],clear) :- forall(between(0,3,N),remove_arity(N)).
same_arity(N,N-_).

forwarder(Name,N) :-
    target(F),(N==0->Args=[];numlist(1,N,Args)),
    'add-atom'('&self',[=,[Name],[F|Args]],_).

applied(Expr,[],Expr).
applied(Expr,[X|Xs],Result) :- applied([Expr,X],Xs,Result).

normalize_answer(partial(Name,Bound),Caller,partial('$forwarder',Bound)) :- Name==Caller,!.
normalize_answer(error(domain_error(function_input_arities(Name,As),N)),Caller,
                 error(domain_error(function_input_arities('$forwarder',As),N))) :- Name==Caller,!.
normalize_answer(Value,_,Value).

observe(Expr,Caller,observation(Answers,Status)) :-
    Box=answers([]),
    catch((eval(Expr,V),normalize_answer(V,Caller,Normal),arg(1,Box,Old),
           nb_setarg(1,Box,[Normal|Old]),fail;true),error(Form,_),Fault=Form),
    arg(1,Box,Backwards),reverse(Backwards,Answers),
    (var(Fault)->Status=ok
    ;normalize_answer(error(Fault),Caller,Error),Status=fault(Error)).

compare_state(Step,History) :-
    forall(between(0,3,N),
      (atomic_list_concat([def_mut_stored_,N],Stored),
       atomic_list_concat([def_mut_fresh_,Step,'_',N],Fresh),forwarder(Fresh,N),
       forall(between(0,3,K),
         (length(Extras,K),append(Extras,_,[7,8,9]),
          applied([Stored],Extras,OldExpr),applied([Fresh],Extras,NewExpr),
          observe(OldExpr,Stored,Old),observe(NewExpr,Fresh,New),
          (Old==New->true
          ;reverse(History,Trace),throw(arity_mutation_mismatch(Step,N,Extras,Trace,Old,New))))))).

steps(I,Limit,_,_) :- I>Limit,!.
steps(I,Limit,Rows,History) :-
    mutate(I,Rows,Next,Operation),compare_state(I,[Operation|History]),
    J is I+1,steps(J,Limit,Next,[Operation|History]).

main :-
    current_prolog_flag(argv,Args),
    (member(A,Args),atom_number(A,Seed)->true;Seed=20261031),set_random(seed(Seed)),
    forall(between(0,3,N),(atomic_list_concat([def_mut_stored_,N],Name),forwarder(Name,N))),
    compare_state(0,[]),steps(1,60,[],[]),
    format('seed=~d: 976 retained/fresh observations agree~n',[Seed]),halt.

:- initialization(main,main).
