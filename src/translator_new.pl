%Pattern matching, structural and functional/relational constraints on arguments:
constrain_args(X, X, []) :- (var(X); atomic(X)), !.
constrain_args([F, A, B], Out, Goals) :- nonvar(F),
                                         F == cons,
                                         constrain_args(A, A1, G1),
                                         constrain_args(B, B1, G2),
                                         Out = [A1|B1],
                                         append(G1, G2, Goals), !.
constrain_args([F|Args], Var, Goals) :- atom(F),
                                        fun(F), !,
                                        translate_expr([F|Args], true, GoalsExpr, Var),
                                        flatten(GoalsExpr, Goals).
constrain_args(In, Out, Goals) :- maplist(constrain_args, In, Out, NestedGoalsList),
                                  flatten(NestedGoalsList, Goals), !.

%Flatten (= Head Body) MeTTa function into Prolog Clause:
translate_clause(Input, (Head :- BodyConj)) :- translate_clause(Input, (Head :- BodyConj), true).
translate_clause(Input, (Head :- BodyConj), ConstrainArgs) :-
                                               Input = [=, [F|Args0], BodyExpr],
                                               ( ConstrainArgs -> maplist(constrain_args, Args0, Args1, GoalsA),
                                                                  flatten(GoalsA,GoalsPrefix)
                                                                ; Args1 = Args0, GoalsPrefix = [] ),
                                               catch(nb_getval(F, Prev), _, Prev = []),
                                               nb_setval(F, [fun_meta(Args1, BodyExpr) | Prev]),
                                               translate_expr(BodyExpr, true, GoalsBody, ExpOut),
                                               (  nonvar(ExpOut) , ExpOut = partial(Base,Bound)
                                               -> current_predicate(Base/Arity), length(Bound, N), M is (Arity - N) - 1,
                                                  length(ExtraArgs, M), append([Bound,ExtraArgs,[Out]],CallArgs), Goal =.. [Base|CallArgs],
                                                  append(GoalsBody,[Goal],FinalGoals), append(Args1,ExtraArgs,HeadArgs)
                                               ; FinalGoals= GoalsBody , HeadArgs = Args1, Out = ExpOut ),
                                               append(HeadArgs, [Out], FinalArgs),
                                               Head =.. [F|FinalArgs],
                                               append(GoalsPrefix, FinalGoals, Goals),
                                               goals_list_to_conj(Goals, BodyConj).

%Print compiled clause:
maybe_print_compiled_clause(_, _, _) :- catch(silent(true), _, true), !.
maybe_print_compiled_clause(Label, FormTerm, Clause) :-
    swrite(FormTerm, FormStr),
    format("\e[33m-->  ~w  -->~n\e[36m~w~n\e[33m--> prolog clause -->~n\e[32m", [Label, FormStr]),
    portray_clause(current_output, Clause),
    format("\e[33m^^^^^^^^^^^^^^^^^^^^^~n\e[0m").

%Conjunction builder, turning goals list to a flat conjunction:
goals_list_to_conj([], true)      :- !.
goals_list_to_conj([G], G)        :- !.
goals_list_to_conj([G|Gs], (G,R)) :- goals_list_to_conj(Gs, R).

% Runtime dispatcher: call F if it's a registered fun/1, else keep as list:
reduce([F|Args], Out) :- nonvar(F), atom(F), fun(F)
                         -> % --- Case 1: callable predicate ---
                            length(Args, N),
                            Arity is N + 1,
                            ( current_predicate(F/Arity) , \+ (current_op(_, _, F), Arity =< 2)
                              -> append(Args,[Out],CallArgs),
                                 Goal =.. [F|CallArgs],
                                 catch(call(Goal),_,fail)
                               ; Out = partial(F,Args) )
                          ; % --- Case 2: partial closure ---
                            compound(F), F = partial(Base, Bound) -> append(Bound, Args, NewArgs),
                                                                     reduce([Base|NewArgs], Out)
                          ; % --- Case 3: leave unevaluated ---
                            Out = [F|Args],
                            \+ cyclic_term(Out).

%Calling reduce from aggregate function foldall needs this argument wrapping
agg_reduce(AF, Acc, Val, NewAcc) :- reduce([AF, Acc, Val], NewAcc).

%Combined expr translation to goals list
% Default wrapper for backward compatibility (interpreter mode)
translate_expr_to_conj(Input, Conj, Out) :- translate_expr_to_conj(Input, true, Conj, Out).
% Version that propagates Execute parameter
translate_expr_to_conj(Input, Execute, Conj, Out) :- translate_expr(Input, Execute, Goals, Out),
                                                     goals_list_to_conj(Goals, Conj).

%Special stream operation rewrite rules before main translation
rewrite_streamops(['trace!', Arg1, Arg2],
                  [progn, ['println!', Arg1], Arg2]).
rewrite_streamops([unique, [superpose|Args]],
                  [call, [superpose, ['unique-atom', [collapse, [superpose|Args]]]]]).
rewrite_streamops([union, [superpose|A], [superpose|B]],
                  [call, [superpose, ['union-atom', [collapse, [superpose|A]],
                                                    [collapse, [superpose|B]]]]]).
rewrite_streamops([intersection, [superpose|A], [superpose|B]],
                  [call, [superpose, ['intersection-atom', [collapse, [superpose|A]],
                                                           [collapse, [superpose|B]]]]]).
rewrite_streamops([subtraction, [superpose|A], [superpose|B]],
                  [call, [superpose, ['subtraction-atom', [collapse, [superpose|A]],
                                                          [collapse, [superpose|B]]]]]).
rewrite_streamops(X, X).

%Guarded stream ops rewrite rule application, successfully avoiding copy_term:
safe_rewrite_streamops(In, Out) :- ( compound(In), In = [Op|_], atom(Op) -> rewrite_streamops(In, Out)
                                                                          ; Out = In).

% Wrapper for interpreter mode: translate_expr/3 defaults to Execute=true
translate_expr(X, Goals, Out) :- translate_expr(X, true, Goals, Out).
%Turn MeTTa code S-expression into goals list:
translate_expr(X, _Execute, [], X)          :- ((var(X) ; atomic(X)) ; X = partial(_,_)), !.
translate_expr([H0|T0], Execute, Goals, Out) :-
        safe_rewrite_streamops([H0|T0],[H|T]),
        translate_expr(H, Execute, GsH, HV),
        %--- Translator rules ---:
        ( nonvar(HV), translator_rule(HV) -> ( catch(match('&self', [':', HV, TypeChain], TypeChain, TypeChain), _, fail)
                                               -> TypeChain = [->|Xs],
                                                  append(ArgTypes, [_], Xs),
                                                  translate_args_by_type(T, ArgTypes, GsT, T1)
                                                ; translate_args(T, GsT, T1) ),
                                              append(T1,[Gs],Args),
                                              HookCall =.. [HV|Args],
                                              call(HookCall),
                                              translate_expr(Gs, Execute, GsE, Out),
                                              append([GsH,GsT,GsE],Goals)
        %--- Non-determinism ---:
        ; HV == superpose, T = [Args], is_list(Args) -> build_superpose_branches(Args, Out, Branches),
                                                        disj_list(Branches, Disj),
                                                        append(GsH, [Disj], Goals)
        ; HV == collapse, T = [E] -> translate_expr_to_conj(E, Execute, Conj, EV),
                                     % Always just collect results with findall
                                     % (result printing is handled at higher level in filereader.pl)
                                     append(GsH, [(findall(EV, Conj, Out))], Goals)
        ; HV == cut, T = [] -> append(GsH, [(!)], Goals),
                               Out = true
        ; HV == test, T = [Expr, Expected] -> translate_expr_to_conj(Expr, Execute, Conj, Val),
                                              translate_expr(Expected, Execute, GsE, ExpVal),
                                              Goal1 = ( findall(Val, Conj, Results),
                                                        (Results = [Actual] -> true
                                                                             ; Actual = Results ) ),
                                              append(GsH, [Goal1], G1),
                                              append(G1, GsE, G2),
                                              append(G2, [test(Actual, ExpVal, Out)], Goals)
        ; HV == once, T = [X] -> translate_expr_to_conj(X, Execute, Conj, Out),
                                 append(GsH, [once(Conj)], Goals)
        ; HV == hyperpose, T = [L] -> build_hyperpose_branches(L, Branches),
                                      append(GsH, [concurrent_and(member((Goal,Res), Branches), (call(Goal), Out = Res))], Goals)
        ; HV == with_mutex, T = [M,X] -> translate_expr_to_conj(X, Execute, Conj, Out),
                                         append(GsH, [with_mutex(M,Conj)], Goals)
        ; HV == transaction, T = [X] -> translate_expr_to_conj(X, Execute, Conj, Out),
                                        append(GsH, [transaction(Conj)], Goals)
        %--- Sequential execution ---:
        ; HV == progn, T = Exprs -> translate_args(Exprs, Execute, GsList, Outs),
                                    append(GsH, GsList, Tmp),
                                    last(Outs, Out),
                                    Goals = Tmp
        ; HV == prog1, T = Exprs -> Exprs = [First|Rest],
                                    translate_expr(First, Execute, GsF, Out),
                                    translate_args(Rest, Execute, GsRest, _),
                                    append(GsH, GsF, Tmp1),
                                    append(Tmp1, GsRest, Goals)
        %--- Conditionals ---:
        ; HV == if, T = [Cond, Then] -> translate_expr_to_conj(Cond, Execute, ConC, Cv),
                                        translate_expr_to_conj(Then, Execute, ConT, Tv),
                                        build_branch(ConT, Tv, Out, BT),
                                        ( ConC == true -> append(GsH, [ ( Cv == true -> BT ) ], Goals)
                                                        ; append(GsH, [ ( ConC, ( Cv == true -> BT ) ) ], Goals) )
        ; HV == if, T = [Cond, Then, Else] -> translate_expr_to_conj(Cond, Execute, ConC, Cv),
                                              translate_expr_to_conj(Then, Execute, ConT, Tv),
                                              translate_expr_to_conj(Else, Execute, ConE, Ev),
                                              build_branch(ConT, Tv, Out, BT),
                                              build_branch(ConE, Ev, Out, BE),
                                              ( ConC == true -> append(GsH, [ (Cv == true -> BT ; BE) ], Goals)
                                                              ; append(GsH, [ (ConC, (Cv == true -> BT ; BE)) ], Goals) )
        ; HV == case, T = [KeyExpr, PairsExpr] -> ( select(Found0, PairsExpr, Rest0),
                                                    subsumes_term(['Empty', _], Found0),
                                                    Found0 = ['Empty', DefaultExpr],
                                                    NormalCases = Rest0
                                                    -> translate_expr_to_conj(KeyExpr, Execute, GkConj, Kv),
                                                       translate_case(NormalCases, Kv, Out, CaseGoal, KeyGoal),
                                                       translate_expr_to_conj(DefaultExpr, Execute, ConD, DOut),
                                                       build_branch(ConD, DOut, Out, DefaultThen),
                                                       Combined = ( (GkConj, CaseGoal) ;
                                                                    \+ GkConj, DefaultThen ),
                                                       append([GsH, KeyGoal, [Combined]], Goals)
                                                     ; translate_expr(KeyExpr, Execute, Gk, Kv),
                                                       translate_case(PairsExpr, Kv, Out, IfGoal, KeyGoal),
                                                       append([GsH, Gk, KeyGoal, [IfGoal]], Goals) )
        %--- Unification constructs ---:
        ; (HV == let ; HV == chain), T = [Pat, Val, In] -> translate_expr(Pat, Execute, Gp, Pv),
                                                           translate_expr(Val, Execute, Gv, V),
                                                           translate_expr(In,  Execute, Gi, Out),
                                                           append([GsH,[(Pv=V)],Gp,Gv,Gi], Goals)
        ; HV == 'let*', T = [Binds, Body] -> letstar_to_rec_let(Binds,Body,RecLet),
                                             translate_expr(RecLet, Execute, Goals, Out)
        ; HV == sealed, T = [Vars, Expr] -> translate_expr_to_conj(Expr, Execute, Con, Val),
                                            Goals = [copy_term(Vars,[Con,Val],_,[Ncon,Out]),Ncon]
        %--- Iterating over non-deterministic generators without reification ---:
        ; HV == 'forall', T = [GF, TF]
          -> ( is_list(GF) -> GF = [GFH|GFA],
                              translate_expr(GFH, Execute, GsGFH, GFHV),
                              translate_args(GFA, Execute, GsGFA, GFAv),
                              append(GsGFH, GsGFA, GsGF),
                              GenList = [GFHV|GFAv]
                            ; translate_expr(GF, Execute, GsGF, GFHV),
                              GenList = [GFHV] ),
             translate_expr(TF, Execute, GsTF, TFHV),
             TestList = [TFHV, V],
             goals_list_to_conj(GsGF, GPre),
             GenGoal = (GPre, reduce(GenList, V)),
             append(GsH, GsTF, Tmp0),
             append(Tmp0, [( forall(GenGoal, ( reduce(TestList, Truth), Truth == true )) -> Out = true ; Out = false )], Goals)
        ; HV == 'foldall', T = [AF, GF, InitS]
          -> translate_expr_to_conj(InitS, Execute, ConjInit, Init),
             translate_expr(AF, Execute, GsAF, AFV),
             ( GF = [M|_], (M==match ; M==let ; M=='let*') -> LambdaGF = ['|->', [], GF],
                                                              translate_expr(LambdaGF, Execute, GsGF, GFHV),
                                                              GenList = [GFHV]
             ; is_list(GF) -> GF = [GFH|GFA],
                              translate_expr(GFH, Execute, GsGFH, GFHV),
                              translate_args(GFA, Execute, GsGFA, GFAv),
                              append(GsGFH, GsGFA, GsGF),
                              GenList = [GFHV|GFAv]
                            ; translate_expr(GF, Execute, GsGF, GFHV),
                              GenList = [GFHV] ),
             append(GsH, GsAF, Tmp1),
             append(Tmp1, GsGF, Tmp2),
             append(Tmp2, [ConjInit, foldall(agg_reduce(AFV, V), reduce(GenList, V), Init, Out)], Goals)
        %--- Higher-order functions with pseudo-lambdas and lambdas ---:
        ; HV == 'foldl-atom', T = [List, Init, AccVar, XVar, Body]
          -> translate_expr_to_conj(List, Execute, ConjList, L),
             translate_expr_to_conj(Init, Execute, ConjInit, InitV),
             translate_expr_to_conj(Body, Execute, BodyConj, BG),
             exclude(==(true), [ConjList, ConjInit], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [foldl([XVar, AccVar, NewAcc]>>(BodyConj, ( number(BG) -> NewAcc is BG ; NewAcc = BG )), L, InitV, Out)], Goals)
        ; HV == 'map-atom', T = [List, XVar, Body]
          -> translate_expr_to_conj(List, Execute, ConjList, L),
             translate_expr_to_conj(Body, Execute, BodyCallConj, BodyCall),
             exclude(==(true), [ConjList], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [maplist([XVar, Y]>>(BodyCallConj, ( number(BodyCall) -> Y is BodyCall ; Y = BodyCall )), L, Out)], Goals)
        ; HV == 'filter-atom', T = [List, XVar, Cond]
          -> translate_expr_to_conj(List, Execute, ConjList, L),
             translate_expr_to_conj(Cond, Execute, CondConj, CondGoal),
             exclude(==(true), [ConjList], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [include([XVar]>>(CondConj, CondGoal), L, Out)], Goals)
         ; HV == '|->', T = [Args, Body], Execute == true
           -> next_lambda_name(F),
              term_variables(Body, AllVars),
              term_variables(Args, ArgVars),
              exclude({ArgVars}/[V]>>memberchk_eq(V, ArgVars), AllVars, FreeVars),
              append(FreeVars, Args, FullArgs),
              translate_clause([=, [F|FullArgs], Body], Clause),
              register_fun(F),
              assertz(Clause),
              format(atom(Label), "metta lambda (~w)", [F]),
              maybe_print_compiled_clause(Label, ['|->', Args, Body], Clause),
              length(FullArgs, N),
              Arity is N + 1,
              assertz(arity(F, Arity)),
              ( FreeVars == [] -> Out = F, Goals = GsH
                                ; Out = partial(F, FreeVars), Goals = GsH )
         ; HV == '|->', T = [Args, Body], Execute == false
           -> next_lambda_name(F),
              % find free (non-argument) variables in Body
              term_variables(Body, AllVars),
              term_variables(Args, ArgVars),
              exclude({ArgVars}/[V]>>memberchk_eq(V, ArgVars), AllVars, FreeVars),
              append(FreeVars, Args, FullArgs),
              translate_clause([=, [F|FullArgs], Body], Clause),
              % Copy the clause so it doesn't share variables with FreeVars
              % This ensures assertz gets a fresh template when FreeVars are bound
              copy_term(Clause, ClauseCopy),
              format(atom(Label), "metta lambda (~w)", [F]),
              maybe_print_compiled_clause(Label, ['|->', Args, Body], Clause),
              length(FullArgs, N),
              Arity is N + 1,
              ( FreeVars == [] -> OutValue = F
                                ; OutValue = partial(F, FreeVars) ),
              LambdaConstructionGoals = [
                  register_fun(F),
                  assertz(ClauseCopy),
                  assertz(arity(F, Arity)),
                  Out = OutValue
              ],
              append(GsH, LambdaConstructionGoals, Goals)
        %--- Spaces ---:
        ; ( HV == 'add-atom' ; HV == 'remove-atom' ), T = [_,_] -> append(T, [Out], RawArgs),
                                                                   Goal =.. [HV|RawArgs],
                                                                   append(GsH, [Goal], Goals)
        ; HV == match, T = [Space, Pattern, Body] -> translate_expr(Space, Execute, G1, S),
                                                     translate_expr(Body, Execute, GsB, Out),
                                                     append(G1, [match(S, Pattern, Out, Out)], G2),
                                                     append(G2, GsB, Goals)
        %--- Predicate to compiled goal ---:
        ; HV == translatePredicate, T = [Expr] -> Expr = [S|Args],
                                                  translate_args(Args, Execute, GsArgs, ArgsOut),
                                                  Goal =.. [S|ArgsOut],
                                                  append(GsH, GsArgs, Inner),
                                                  append(Inner, [Goal], Goals)
        %--- Manual dispatch options: ---
        %Generate a predicate call on compilation, translating Args for nesting:
        ; HV == call,  T = [Expr] -> Expr = [F|Args],
                                     translate_args(Args, Execute, GsArgs, ArgsOut),
                                     append(GsH, GsArgs, Inner),
                                     append(ArgsOut, [Out], CallArgs),
                                     Goal =.. [F|CallArgs],
                                     append(Inner, [Goal], Goals)
        %Produce a dynamic dispatch, translating Args for nesting:
        ; HV == reduce, T = [Expr] -> ( var(Expr) -> translate_expr(Expr, Execute, GsH, ExprOut),
                                                     Goals = [reduce(ExprOut, Out)|GsH]
                                                   ; Expr = [F|Args],
                                                     translate_args(Args, Execute, GsArgs, ArgsOut),
                                                     append(GsH, GsArgs, Inner),
                                                     ExprOut = [F|ArgsOut],
                                                     append(Inner, [reduce(ExprOut, Out)], Goals) )
        %Invoke translator to evaluate MeTTa code as data/list:
        ; HV == eval, T = [Arg] -> append(GsH, [], Inner),
                                   Goal = eval(Arg, Out),
                                   append(Inner, [Goal], Goals)
        %Force arg to remain data/list:
        ; HV == quote, T = [Expr] -> append(GsH, [], Inner),
                                     Out = Expr,
                                     Goals = Inner
        ; HV == 'catch', T = [Expr] ->
          translate_expr(Expr, Execute, GsExpr, ExprOut),
          append(GsH, [], Inner),
          goals_list_to_conj(GsExpr, Conj),
          Goal = catch((Conj, Out = ExprOut),
                       Exception,
                       (Exception = error(Type, Ctx) -> Out = ['Error', Type, Ctx]
                                                      ; Out = ['Error', Exception])),
          append(Inner, [Goal], Goals)
        %--- Automatic 'smart' dispatch, translator deciding when to create a predicate call, data list, or dynamic dispatch: ---
        ; ( ( atom(HV), fun(HV), Fun = HV, IsPartial = false
            ; compound(HV), HV = partial(Fun, Bound), IsPartial = true
            )
            % Check for type definition FIRST (before translating args!)
            -> findall(TypeChain, catch(match('&self', [':', Fun, TypeChain], TypeChain, TypeChain), _, fail), TypeChains),
               ( TypeChains \= []
                 -> % HAS TYPES - use typed translation (same as before)
                    maplist({Execute,Fun,T,GsH,IsPartial,Bound,Out}/[TypeChain,BranchGoal]>>(
                            typed_functioncall_branch(Fun, TypeChain, T, GsH, IsPartial, Bound, Out, BranchGoal)), TypeChains, Branches),
                    disj_list(Branches, Disj),
                    Goals = [Disj]
               ; % NO TYPES - translate args normally and dispatch
                 translate_args(T, Execute, GsT, AVs),
                 ( IsPartial -> append(Bound, AVs, AllAVs) ; AllAVs = AVs ),
                 append(GsH, GsT, Inner),
                 (Execute
                   -> build_call_or_partial(Fun, AllAVs, Out, Inner, [], Goals)
                   ; Goals = [runtime_call(Fun, T, Out)]  % Pass expressions, not values!
                 )
               )
          ; % Not a known function - translate args for remaining branches
            translate_args(T, Execute, GsT, AVs),
            append(GsH, GsT, Inner),
            ( % Literals (numbers, strings, etc.)
              ( atomic(HV), \+ atom(HV) ) -> Out = [HV|AVs], Goals = Inner
            ; % Non-function atom
              atom(HV), \+ fun(HV) -> ( Execute
                                         -> Out = [HV|AVs], Goals = Inner
                                         ; Goals = [runtime_call(HV, T, Out)]  % Pass expressions!
                                       )
            ; % Data list
              is_list(HV) -> eval_data_term(Execute, HV, Gd, HV1),
                             append(Inner, Gd, Goals),
                             Out = [HV1|AVs]
            ; % Unknown head (var/compound) => runtime dispatch
              append(Inner, [reduce([HV|AVs], Out)], Goals)
            )
          )).

%Generate actual function call or partial if arity not complete:
build_call_or_partial(Fun, AVs, Out, Inner, Extra, Goals) :- 
    length(AVs, N),
    Arity is N + 1,
    ( maybe_specialize_call(Fun, AVs, Out, Goal)
      -> % Specialization succeeded during execution - use it
         append(Inner, [Goal|Extra], Goals)
    ; ( current_predicate(Fun/Arity) ; catch(arity(Fun, Arity), _, fail) ),
      \+ ( current_op(_, _, Fun), Arity =< 2 )
      -> % Direct call during execution
         append(AVs, [Out], Args),
         Goal =.. [Fun|Args],
         append(Inner, [Goal|Extra], Goals)
    ; Out = partial(Fun, AVs),
      append(Inner, Extra, Goals)
    ).

% Runtime call helper: accepts unevaluated expressions and evaluates them according to type declarations
% This allows compiled programs to respect Expression types and other type annotations
% Falls back to reduce for edge cases (partial applications, non-callables, etc.)
runtime_call(Fun, ArgExprs, Out) :-
    % Query for type declaration at runtime (when atom space is populated)
    findall(TypeChain, catch(match('&self', [':', Fun, TypeChain], TypeChain, TypeChain), _, fail), TypeChains),
    ( TypeChains = [TypeChain|_],
      TypeChain = [->|Xs],
      append(ArgTypes, [OutType], Xs)
      -> % HAS TYPE DECLARATION - evaluate args according to their types
         maplist({ArgExprs}/[Type,Expr,AV]>>(
             ( Type == 'Expression'
               -> AV = Expr  % Keep as unevaluated expression
               ; % Evaluate the expression
                 translate_expr(Expr, true, Goals, AVTmp),
                 call_goals(Goals),
                 % Type check if not %Undefined% or Atom
                 ( (Type == '%Undefined%' ; Type == 'Atom')
                   -> AV = AVTmp
                   ; ( ('get-type'(AVTmp, Type) *-> true ; 'get-metatype'(AVTmp, Type))
                       -> AV = AVTmp
                       ; fail  % Type check failed
                     )
                 )
             )
         ), ArgTypes, ArgExprs, AVs),
         % Now call the function with properly evaluated arguments
         length(AVs, N),
         Arity is N + 1,
         ( maybe_specialize_call(Fun, AVs, OutTmp, Goal)
           -> writeln("specialization path."), call(Goal)
         ; fun(Fun),
           (current_predicate(Fun/Arity) ; catch(arity(Fun, Arity), _, fail)),
           \+ ( current_op(_, _, Fun), Arity =< 2 )
           -> append(AVs, [OutTmp], Args), Goal =.. [Fun|Args], call(Goal)
         ; reduce([Fun|AVs], OutTmp)
         ),
         % Type check output if needed
         ( (OutType == '%Undefined%' ; OutType == 'Atom')
           -> Out = OutTmp
           ; ( ('get-type'(OutTmp, OutType) *-> true ; 'get-metatype'(OutTmp, OutType))
               -> Out = OutTmp
               ; fail  % Output type check failed
             )
         )
    ; % NO TYPE DECLARATION - evaluate all args (default behavior)
      maplist({ArgExprs}/[Expr,AV]>>(
          translate_expr(Expr, true, Goals, AV),
          call_goals(Goals)
      ), ArgExprs, AVs),
      % Call function (same logic as typed path)
      length(AVs, N),
      Arity is N + 1,
      ( maybe_specialize_call(Fun, AVs, Out, Goal)
        -> writeln("specialization path."), call(Goal)
      ; fun(Fun),
        (current_predicate(Fun/Arity) ; catch(arity(Fun, Arity), _, fail)),
        \+ ( current_op(_, _, Fun), Arity =< 2 )
        -> append(AVs, [Out], Args), Goal =.. [Fun|Args], call(Goal)
      ; reduce([Fun|AVs], Out)
      )
    ).

%Type function call generation, returns function call plus typechecks for input and output:
typed_functioncall_branch(Fun, TypeChain, T, GsH, IsPartial, Bound, Out, BranchGoal) :-
    TypeChain = [->|Xs],
    append(ArgTypes, [OutType], Xs),
    translate_args_by_type(T, ArgTypes, GsT2, AVsTmp0),
    ( IsPartial -> append(Bound, AVsTmp0, AVsTmp) ; AVsTmp = AVsTmp0 ),
    append(GsH, GsT2, InnerTmp),
    ( (OutType == '%Undefined%' ; OutType == 'Atom')
       -> Extra = [] ; Extra = [('get-type'(Out, OutType) *-> true ; 'get-metatype'(Out, OutType))] ),
    build_call_or_partial(Fun, AVsTmp, Out, InnerTmp, Extra, GoalsList),
    goals_list_to_conj(GoalsList, BranchGoal).


%Selectively apply translate_args for non-Expression args while Expression args stay as data input:
translate_args_by_type([], _, [], []) :- !.
translate_args_by_type([A|As], [T|Ts], GsOut, [AV|AVs]) :-
                      ( T == 'Expression' -> AV = A, GsA = []
                                           ; translate_expr(A, true, GsA1, AV),
                                             ( (T == '%Undefined%' ; T == 'Atom')
                                               -> GsA = GsA1
                                                ; append(GsA1, [('get-type'(AV, T) *-> true ; 'get-metatype'(AV, T))], GsA))),
                                             translate_args_by_type(As, Ts, GsRest, AVs),
                                             append(GsA, GsRest, GsOut).

%Handle data list:
eval_data_term(_Execute, X, [], X) :- (var(X); atomic(X)), !.
eval_data_term(Execute, [F|As], Goals, Val) :- 
    % When Execute=true (interpreter), evaluate functions in data lists
    % When Execute=false (compiling), treat everything as pure data for runtime flexibility
    ( Execute, atom(F), fun(F) 
      -> translate_expr([F|As], Execute, Goals, Val)
      ; eval_data_list(Execute, [F|As], Goals, Val) ).

%Handle data list entry:
eval_data_list(_Execute, [], [], []).
eval_data_list(Execute, [E|Es], Goals, [V|Vs]) :- 
    ( is_list(E) -> eval_data_term(Execute, E, G1, V) ; V = E, G1 = [] ),
    eval_data_list(Execute, Es, G2, Vs),
    append(G1, G2, Goals).


%Convert let* to recusrive let:
letstar_to_rec_let([[Pat,Val]],Body,[let,Pat,Val,Body]).
letstar_to_rec_let([[Pat,Val]|Rest],Body,[let,Pat,Val,Out]) :- letstar_to_rec_let(Rest,Body,Out).

%Patterns: variables, atoms, numbers, lists:
translate_pattern(X, X) :- var(X), !.
translate_pattern(X, X) :- atomic(X), !.
translate_pattern([H|T], [P|Ps]) :- !, translate_pattern(H, P),
                                       translate_pattern(T, Ps).

% Constructs the goal for a single branch of an if-then-else/case.
build_branch(true, Val, Out, (Out = Val)) :- !.
build_branch(Con, Val, Out, Goal) :- var(Val) -> Val = Out, Goal = Con
                                               ; Goal = (Val = Out, Con).

%Translate case expression recursively into nested if:
translate_case([[K,VExpr]|Rs], Kv, Out, Goal, KGo) :- translate_expr_to_conj(VExpr, true, ConV, VOut),
                                                      constrain_args(K, Kc, Gc),
                                                      build_branch(ConV, VOut, Out, Then),
                                                      ( Rs == [] -> Goal = ((Kv = Kc) -> Then), KGi=[]
                                                                  ; translate_case(Rs, Kv, Out, Next, KGi),
                                                                    Goal = ((Kv = Kc) -> Then ; Next) ),
                                                      append([Gc,KGi], KGo).

% Wrapper for interpreter mode
translate_args(Xs, Goals, Vs) :- translate_args(Xs, true, Goals, Vs).
%Translate arguments recursively:
translate_args([], _Execute, [], []).
translate_args([X|Xs], Execute, Goals, [V|Vs]) :- 
    translate_expr(X, Execute, G1, V),
    translate_args(Xs, Execute, G2, Vs),
    append(G1, G2, Goals).

%Build A ; B ; C ... from a list:
disj_list([G], G).
disj_list([G|Gs], (G ; R)) :- disj_list(Gs, R).

%Build one disjunct per branch: (Conj, Out = Val):
build_superpose_branches([], _, []).
build_superpose_branches([E|Es], Out, [B|Bs]) :- translate_expr_to_conj(E, true, Conj, Val),
                                                 build_branch(Conj, Val, Out, B),
                                                 build_superpose_branches(Es, Out, Bs).

%Build hyperpose branch as a goal list for concurrent_maplist to consume:
build_hyperpose_branches([], []).
build_hyperpose_branches([E|Es], [(Goal, Res)|Bs]) :- translate_expr_to_conj(E, true, Goal, Res),
                                                      build_hyperpose_branches(Es, Bs).

%Like membercheck but with direct equality rather than unification
memberchk_eq(V, [H|_]) :- V == H, !.
memberchk_eq(V, [_|T]) :- memberchk_eq(V, T).

%Generate readable lambda name:
next_lambda_name(Name) :- ( catch(nb_getval(lambda_counter, Prev), _, Prev = 0) ),
                          N is Prev + 1,
                          nb_setval(lambda_counter, N),
                          format(atom(Name), 'lambda_~d', [N]).
