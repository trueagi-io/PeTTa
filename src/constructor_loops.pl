:- dynamic ho_fresh_result/2.

% A deterministic constructor loop may build its result on entry only when
% the caller's output is fresh and cannot be observed by its arguments or any
% earlier goal. The ordinary specialization keeps its original binding order.
fresh_result_calls(Goal,Before,Out,Deps) :-
    ( nonvar(Goal),Goal=(A,B)
      -> fresh_result_calls(A,Before,A1,D1),
         fresh_result_calls(B,(Before,A1),B1,D2),Out=(A1,B1),append(D1,D2,Deps)
    ; nonvar(Goal),Goal=(If->Then;Else)
      -> fresh_result_calls(If,Before,I1,D1),
         fresh_result_calls(Then,(Before,succeeded(I1)),T1,D2),
         fresh_result_calls(Else,(Before,failed(I1)),E1,D3),
         Out=(I1->T1;E1),append([D1,D2,D3],Deps)
    ; compound(Goal),compound_name_arguments(Goal,Op,[A,B]),memberchk(Op,[;,->,*->])
      -> % The other side still counts for freshness. Only a condition that
         % succeeded before its then-branch contributes shape evidence.
         (Op == (;) -> Done=unexecuted(A) ; Done=succeeded(A)),
         fresh_result_calls(A,(Before,unexecuted(B)),A1,D1),
         fresh_result_calls(B,(Before,Done),B1,D2),
         compound_name_arguments(Out,Op,[A1,B1]),append(D1,D2,Deps)
    ; nonvar(Goal),Goal=once(G)
      -> fresh_result_calls(G,Before,G1,Deps),Out=once(G1)
    ; compound(Goal),Goal=..[F|Args],append(Inputs,[Result],Args),
      var(Result),\+contains_var(Result,Before-Inputs),
      ho_capture_layout(F,_),seen_proper_input(F,Inputs,Before),
      oracle_mode(false),oracle_det_mode(false),
      \+suppress_det_cut(true),
      fresh_result_worker(F,Worker)
      -> Out=..[Worker|Args],
         length(Inputs,N),Deps=[clause_set(Worker/N),decl(Worker/N),clause_set(F/N)]
    ; Out=Goal,Deps=[] ).

% Only successful earlier goals contribute shape evidence. A failed if
% condition or a goal on another branch can expose variables, but is not
% evidence that is_list succeeded.
seen_proper_input(F,Inputs,Before) :-
    findall(Ts,declared_fn_type(F,Ts,_,_),[Types]),
    findall(I,(nth0(I,Types,T),nonvar(T),T=['List',_]),[Index]),
    nth0(Index,Inputs,Input),seen_proper_list(Input,Before),!.
seen_proper_list(V,_) :- is_list(V),!.
seen_proper_list(V,Goal) :-
    nonvar(Goal),
    (Goal=failed(_) -> fail
    ; Goal=unexecuted(_) -> fail
    ; Goal=succeeded(G) -> seen_proper_list(V,G)
    ; Goal=(A,B) -> (seen_proper_list(V,A);seen_proper_list(V,B))
    ; Goal=is_list(X) -> V==X
    ; compound(Goal),compound_name_arguments(Goal,F,Args),append(_,[Result],Args),V==Result,
      length(Args,A),N is A-1,proper_list_output(F,N)).

fresh_result_worker(F,Worker) :-
    ho_capture_layout(F,captures(_,Fn,_)),
    findall(D,declared_fn_type(Fn,_,_,D),[Level]),memberchk(Level,[det,semidet]),
    ho_fresh_result(Worker,F),
    \+ (compiled_deps(R,Worker/_,_,_),compiled_body_stale(R)),!.
fresh_result_worker(F,Worker) :-
    findall(ATs-OT-D,declared_fn_type(F,ATs,OT,D),[Types-['List',_]-Level]),
    memberchk(Level,[det,semidet,nondet]),
    ho_capture_layout(F,captures(_,Fn,_)),
    findall(D,declared_fn_type(Fn,_,_,D),[Callback]),memberchk(Callback,[det,semidet]),
    length(Types,N),A is N+1,
    functor(H,F,A),predicate_property(H,number_of_clauses(2)),
    findall(source(Source,File),(clause(H,B,R),\+compiled_body_stale(R),translated_from(R,Source),
             compiled_dependency_origin(R,File),copy_term_nat(H-B,CH-CB),
             fresh_result_clause(F,CH,CB,_,_)),Sources),
    length(Sources,2),
    atom_concat(F,'_fresh_result',Worker),\+fun(Worker),\+current_predicate(Worker/_),
    (nb_current('$spec_created',_) -> remember_specialization_symbol(Worker) ; true),
    register_fun(Worker),assertz(ho_specialization(F,Worker)),
    assertz(ho_fresh_result(Worker,F)),assertz(arity(Worker,A)),
    ( catch((findall(T,match('&self',[':',F,T],T,T),Ts),
             forall(member(T,Ts),add_sexp('&self',[':',Worker,T])),
             maplist({Worker}/[source(S,File)]>>
                 (S=[=,[_|Ps],Body],Input=[=,[Worker|Ps],Body],
                  translate_clause(Input,Clause,false,Ds),
                  assert_compiled_clause(assertz,Clause,Ref),
                  assertz(translated_from(Ref,Input)),
                  length(Ps,PN),record_compiled_dependencies(Ref,Worker/PN,File,Ds),
                  add_sexp('&self',Input)),Sources)),
            Error,(forget_symbol(Worker),throw(Error)))
      -> true ; forget_symbol(Worker),fail ).

% The two allowed clauses are the empty result and one recursive step whose
% final operation publishes a cons cell. The recursive call advances exactly
% one input list and passes every other input unchanged.
fresh_result_clause(_,Head,Body,Head,Body) :-
    Head=..[_|Args],append(Inputs,[Result],Args),Result==[],
    member(X,Inputs),X==[],(Body==true;Body==(!)),!.
fresh_result_clause(Parent,Head,Body,NewHead,NewBody) :-
    Head=..[Name|Args],append(Inputs,[Out],Args),var(Out),
    goal_sequence(Body,Goals),append(Prefix,[Call,Binding],Goals),
    nonvar(Binding),Binding=(Left=[Value|Tail]),Left==Out,var(Tail),
    (nonvar(Call),Call=once(Direct) -> true ; Direct=Call),
    nonvar(Direct),Direct=..[Parent|Next],append(Rest,[End],Next),End==Tail,
    same_length(Inputs,Rest),
    findall(I,(nth0(I,Inputs,X),nth0(I,Rest,Y),X\==Y),[Index]),
    declared_fn_type(Parent,Types,_,_),nth0(Index,Types,['List',_]),
    nth0(Index,Inputs,List),nonvar(List),List=[_|Xs],nth0(Index,Rest,Ys),Xs==Ys,
    \+contains_var(Out,Inputs-Prefix-Call),\+contains_var(Tail,Inputs-Prefix),
    maplist(constructor_prefix,Prefix),
    append(Inputs,[[Value|Tail]],NewArgs),NewHead=..[Name|NewArgs],
    NewCall=..[Name|Next],append(Prefix,[NewCall],Work),goals_list_to_conj(Work,NewBody).

% Building a cell before a nondeterministic prefix would share it between
% answers. An if commits its condition; both selected arms must still be
% single-solution, as must every ordinary call in the prefix.
constructor_prefix(G) :-
    nonvar(G),
    ( G==true;G==(!) ),!.
constructor_prefix(G) :-
    nonvar(G),
    ( G=(A,B) -> constructor_prefix(A),constructor_prefix(B)
    ; G=(_->T;E) -> constructor_prefix(T),constructor_prefix(E)
    ; G=once(_) -> true
    ; compound(G),functor(G,F,A),
      (memberchk(F/A,[(=)/2,is/2,(<)/2,(=<)/2,(>)/2,(>=)/2,(==)/2,(\==)/2,
                       var/1,nonvar/1,number/1,is_list/1])
       ; N is A-1,findall(D,(declared_fn_type(F,Ts,_,D),length(Ts,N)),[Level]),
         memberchk(Level,[det,semidet])) ).

goal_sequence(Goal,Goals) :-
    (nonvar(Goal),Goal=(A,B) -> goal_sequence(A,As),goal_sequence(B,Bs),append(As,Bs,Goals)
    ; Goals=[Goal]).
