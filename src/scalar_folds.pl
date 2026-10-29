:- dynamic ho_scalar_fold/2.

% Scalarize both the private result tuple and the accumulator of a producer
% ending in a specialized numeric fold. Keep its checks/effects inside their
% original predicate boundary and preserve final result binding order.
scalar_fold_calls(Goal,Before,Out,Deps) :-
    ( nonvar(Goal),Goal=(A,B)
      -> (scalar_fold_use(A,B,Before,Replaced,Added)
           -> scalar_fold_calls(Replaced,Before,Out,Rest),append(Added,Rest,Deps)
          ; scalar_fold_calls(A,(Before,B),X,D1),
            scalar_fold_calls(B,(Before,X),Y,D2),Out=(X,Y),append(D1,D2,Deps))
    ; compound(Goal),compound_name_arguments(Goal,Op,[A,B]),memberchk(Op,[;,->,*->])
      -> scalar_fold_calls(A,(Before,B),X,D1),scalar_fold_calls(B,(Before,A),Y,D2),
         compound_name_arguments(Out,Op,[X,Y]),append(D1,D2,Deps)
    ; nonvar(Goal),Goal=once(G)
      -> scalar_fold_calls(G,Before,Changed,Deps),Out=once(Changed)
    ; Out=Goal,Deps=[] ).

scalar_fold_use(Binding,Body,Before,Out,Deps) :-
    oracle_mode(false),oracle_det_mode(false),\+suppress_det_cut(true),
    nonvar(Binding),Binding=fresh_data_list(Fields,Pair),
    is_list(Fields),Fields=[_,_|_],maplist(var,Fields),var(Pair),
    term_variables([Pair|Fields],Fresh),\+ (member(V,Fresh),contains_var(V,Before)),
    (nonvar(Body),Body=(Call,Rest)->true;Call=Body,Rest=true),
    nonvar(Call),Call=..[F|Args],append(Inputs,[Output],Args),Output==Pair,
    occurrences_of_var(Pair,Body,1),\+ (member(V,Fresh),contains_var(V,Inputs)),
    findall(T,declared_fn_type(F,_,T,_),[Result]),is_list(Result),same_length(Fields,Result),
    scalar_fold_worker(F,Worker,Deps),
    append(Inputs,Fields,Flat),Direct=..[Worker|Flat],Out=(Direct,Rest).

scalar_fold_worker(F,Worker,Deps) :-
    ho_scalar_fold(Worker,F),
    \+ (compiled_deps(R,Worker/_,_,_),compiled_body_stale(R)),!,
    once(compiled_deps(_,Worker/_,_,Deps)).
scalar_fold_worker(F,Worker,Deps) :-
    findall(Ts-OT-D,declared_fn_type(F,Ts,OT,D),[Types-Result-Level]),
    memberchk(Level,[det,semidet]),is_list(Result),Result=[_,_|_],maplist(==('Number'),Result),
    length(Types,N),A is N+1,functor(H,F,A),predicate_property(H,number_of_clauses(1)),
    clause(H,Body,R),\+compiled_body_stale(R),compiled_deps(R,_,_,ProducerDeps),
    copy_term_nat(H-Body,Head-Copy),Head=..[_|Args],append(Inputs,[Output],Args),var(Output),
    goal_sequence(Copy,Goals),append(Prefix,[Tail],Goals),
    (nonvar(Tail),Tail=once(Call)->Once=true;Call=Tail,Once=false),
    compound(Call),Call=..[Fold|FoldArgs],ho_capture_layout(Fold,_),
    append(FoldInputs,[End],FoldArgs),End==Output,\+contains_var(Output,Inputs-Prefix),
    findall(FTs-OT,declared_fn_type(Fold,FTs,OT,_),[FoldTypes-Result]),
    findall(I,(nth0(I,FoldTypes,T),T==Result),[Index]),nth0(Index,FoldInputs,Initial),
    select(Pack,Prefix,Kept),nonvar(Pack),Pack=(Pair=InitialFields),Pair==Initial,var(Initial),
    is_list(InitialFields),same_length(InitialFields,Result),
    \+contains_var(Initial,InitialFields),
    length(Before,Index),append(Before,[_|After],FoldInputs),
    \+contains_var(Initial,Inputs-Kept-Before-After),
    length(FoldTypes,FN),FA is FN+1,functor(FH,Fold,FA),
    predicate_property(FH,number_of_clauses(2)),
    findall(source(FH,B,Ds),(clause(FH,B,FR),\+compiled_body_stale(FR),
                            compiled_deps(FR,_,_,Ds)),[Base,Step]),
    scalar_fold_shape(Fold,Index,Base,Step,Shape),
    Shape=shape(BaseArgs,BaseBody,StepArgs,Work,Next,State,Values),
    length(State,K),WorkerA is N+K,LoopA is FA+2*K-2,
    atom_concat(F,'_scalar_fold',Worker),atom_concat(Worker,'_loop',Loop),
    scalar_fold_available(Worker,WorkerA),scalar_fold_available(Loop,LoopA),
    length(Results,K),append(Inputs,Results,WorkerArgs),WorkerHead=..[Worker|WorkerArgs],
    scalar_fold_args(Index,InitialFields,Results,FoldArgs,EntryArgs),Entry=..[Loop|EntryArgs],
    (Once==true->Invoke=once(Entry);Invoke=Entry),
    append(Kept,[Invoke],EntryGoals),goals_list_to_conj(EntryGoals,WorkerBody),
    length(BaseState,K),scalar_fold_args(Index,BaseState,BaseState,BaseArgs,ScalarBase),
    LoopBase=..[Loop|ScalarBase],
    scalar_fold_args(Index,State,Results,StepArgs,ScalarStep),LoopStep=..[Loop|ScalarStep],
    scalar_fold_args(Index,Values,Results,Next,RecursiveArgs),Recur=..[Loop|RecursiveArgs],
    append(Work,[Recur],LoopGoals),goals_list_to_conj(LoopGoals,LoopBody),
    Base=source(_,_,D1),Step=source(_,_,D2),
    append([ProducerDeps,D1,D2,[clause_set(F/N),decl(F/N),clause_set(Fold/FN),decl(Fold/FN)]],All),
    sort(All,Deps),WN is WorkerA-1,LN is LoopA-1,
    forall(member(Name/Arity,[Worker/WorkerA,Loop/LoopA]),
      ((nb_current('$spec_created',_)->remember_specialization_symbol(Name);true),
       register_fun(Name),assertz(arity(Name,Arity)))),
    assertz(ho_specialization(F,Worker)),assertz(ho_specialization(Fold,Worker)),
    assertz(ho_specialization(Worker,Loop)),assertz(ho_scalar_fold(Worker,F)),
    forall(member(Key-Clause,[Worker/WN-(WorkerHead:-WorkerBody),
                             Loop/LN-(LoopBase:-BaseBody),Loop/LN-(LoopStep:-LoopBody)]),
      (assert_compiled_clause(assertz,Clause,Ref),
       % Owners regenerate this code. The marker preserves native arithmetic
       % in load-cache replay without inventing an independent source equation.
       assertz(translated_from(Ref,scalar_fold(F))),record_compiled_dependencies(Ref,Key,Deps))),!.

% Erasure leaves an empty dynamic predicate behind. Reuse that declaration,
% but never overwrite a live or foreign symbol.
scalar_fold_available(F,A) :-
    \+fun(F),
    forall(current_predicate(F/N),
      (N=:=A,functor(H,F,N),predicate_property(H,dynamic),predicate_property(H,number_of_clauses(0)))).

scalar_fold_args(Index,State,Output,Args,Flat) :-
    length(Before,Index),append(Before,[_|After],Args),append(Rest,[_],After),
    State=[First|More],append([Before,[First],Rest,More,Output],Flat).

scalar_fold_shape(F,Index,source(BH,BB,_),source(SH,SB,_),
                  shape(BA,BB,SA,Prefix,Next,State,Values)) :-
    BH=..[_|BA],append(BI,[BO],BA),nth0(Index,BI,Initial),var(Initial),BO==Initial,
    length(Before,Index),append(Before,[Initial|After],BI),\+contains_var(Initial,Before-After),
    (BB==true;BB==(!)),
    SH=..[_|SA],append(SI,[Output],SA),var(Output),nth0(Index,SI,State),
    goal_sequence(SB,Goals0),
    % A rebuilt callback can retain its private head match as the first goal.
    (var(State),Goals0=[Match|Rest],nonvar(Match),Match=(V=Fields),V==State,is_list(Fields),
     length(SBefore,Index),append(SBefore,[_|SAfter],SI),\+contains_var(State,SBefore-SAfter-Rest)
      -> State=Fields,Goals=Rest
    ; Goals=Goals0),
    is_list(State),State=[_,_|_],maplist(numeric_tuple_field,State),
    append(Work,[Call],Goals),nonvar(Call),Call=..[F|Next],append(NI,[NO],Next),NO==Output,
    nth0(Index,NI,NextState),var(NextState),
    findall(J,(nth0(J,SI,X),J=\=Index,nth0(J,NI,Y),X\==Y),[ListIndex]),
    nth0(ListIndex,BI,Empty),Empty==[],nth0(ListIndex,SI,List),nonvar(List),List=[_|Tail],
    nth0(ListIndex,NI,NextList),NextList==Tail,
    select(Binding,Work,Prefix),nonvar(Binding),Binding=(Pair=Values),Pair==NextState,
    is_list(Values),same_length(State,Values),maplist(numeric_tuple_field,Values),
    \+contains_var(NextState,SH-Prefix),\+contains_var(Output,SI-Prefix),
    \+ (sub_term(T,Prefix),nonvar(T),(T==State;T==Values)),
    scalar_fold_numbers(Prefix,[],Numbers),
    forall(member(V,Values),(number(V);memberchk_eq(V,Numbers))).

% Each field must be numerical on every successful path before recursion.
scalar_fold_numbers([],Known,Known).
scalar_fold_numbers([G|Gs],Known,Out) :-
    ( nonvar(G),G=(X is _) -> Next=[X|Known]
    ; nonvar(G),G=(_->T;E)
      -> goal_sequence(T,Ts),goal_sequence(E,Es),
         scalar_fold_numbers(Ts,Known,TN),scalar_fold_numbers(Es,Known,EN),
         include({EN}/[V]>>memberchk_eq(V,EN),TN,Next)
    ; Next=Known ),
    scalar_fold_numbers(Gs,Next,Out).
