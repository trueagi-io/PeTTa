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
                                        translate_expr([F|Args], GoalsExpr, Var),
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
                                               retractall(det_analysis_cache(_, _, _)),  %clause set changed
                                               clause_param_types(F, Args1, DeclOut),
                                               %Specialized clause copies (ConstrainArgs == false) are instances of
                                               %already-validated clauses: bind their (more specific) param types for
                                               %guard elimination, but skip the determinism/strict/output checks.
                                               %Determinism runs after param binding so closure params carry their
                                               %declared arrow types into the body analysis.
                                               ( ConstrainArgs == false -> true
                                                                         ; validate_function_determinism(F, Args1, BodyExpr, Prev) ),
                                               begin_clause_inference(F, Args1, Assume, SavedInf),
                                               translate_expr(BodyExpr, GoalsBody, ExpOut),
                                               (  nonvar(ExpOut) , ExpOut = partial(Base,Bound)
                                               -> arity(Base, Arity), length(Bound, N), M is (Arity - N) - 1,
                                                  length(ExtraArgs, M), append([Bound,ExtraArgs,[Out]],CallArgs), Goal =.. [Base|CallArgs],
                                                  append(GoalsBody,[Goal],FinalGoals), append(Args1,ExtraArgs,HeadArgs),
                                                  OutChecks = [],
                                                  end_clause_inference(F, Args1, none, none, SavedInf)
                                               ; FinalGoals= GoalsBody , HeadArgs = Args1, Out = ExpOut,
                                                 end_clause_inference(F, Args1, ExpOut, Assume, SavedInf),
                                                 ( ConstrainArgs == false -> OutChecks = []
                                                 ; clause_output_goals(F, DeclOut, ExpOut, BodyExpr, OutChecks) ) ),
                                               ( ConstrainArgs == false -> true
                                                                         ; strict_check_function_typed(F, Args1) ),
                                               append(HeadArgs, [Out], FinalArgs),
                                               Head =.. [F|FinalArgs],
                                               length(FinalArgs, CompiledArity),
                                               (arity(F, CompiledArity) -> true ; assertz(arity(F, CompiledArity))),
                                               %declared-deterministic functions commit to the first matching
                                               %clause (non-overlap is validated), guaranteeing no choicepoints
                                               %and enabling last-call optimization:
                                               ( clause_commit_cut(F, Args1) -> Commit = [!] ; Commit = [] ),
                                               append([GoalsPrefix, Commit, FinalGoals, OutChecks], Goals),
                                               goals_list_to_conj(Goals, BodyConj).

clause_commit_cut(F, Args) :- length(Args, N),
                              catch(fn_determinism(F, N, det), _, fail).

%%% Late binding across files. A call to a declared function whose definition
%%% has not arrived yet compiles as data (which is also what keeps declared
%%% constructors literal). Clauses embedding such a symbol are recorded, and
%%% when the definition arrives they are retranslated - now as real calls.
:- dynamic late_symbol_use/3.   % late_symbol_use(F, ClauseRef, SourceTerm)

note_late_symbol_uses(Term, Ref) :- Term = [=, _, Body],
                                    forall(( declared_undefined_atom(Body, F),
                                             \+ late_symbol_use(F, Ref, _) ),
                                           assertz(late_symbol_use(F, Ref, Term))).

declared_undefined_atom(T, F) :- ( atom(T) -> F = T, \+ fun(F), fn_decl_arity(F, _, _, _)
                                 ; is_list(T), member(E, T), declared_undefined_atom(E, F) ).

%The definition of F arrived: retranslate every clause that saw F as data.
%The stale fun_meta entry is dropped first so the clause is not re-validated
%against itself; its specializations are invalidated like any redefinition.
recompile_late_uses(F) :- ( late_symbol_use(F, _, _)
                            -> findall(Ref-Term, late_symbol_use(F, Ref, Term), Us),
                               retractall(late_symbol_use(F, _, _)),
                               forall(member(Ref-Term, Us), recompile_clause(Ref, Term))
                             ; true ).

recompile_clause(Ref, Term) :- ( clause(_, _, Ref)
                                 -> erase(Ref),
                                    retractall(translated_from(Ref, _)),
                                    Term = [=, [G|_], Body],
                                    drop_stale_fun_meta(G, Body),
                                    translate_clause(Term, Clause),
                                    assertz(Clause, NewRef),
                                    assertz(translated_from(NewRef, Term)),
                                    note_late_symbol_uses(Term, NewRef),
                                    invalidate_specializations(G)
                                  ; true ).

drop_stale_fun_meta(G, Body) :- catch(nb_getval(G, Metas), _, fail),
                                select(fun_meta(_, B), Metas, Rest), B =@= Body, !,
                                nb_setval(G, Rest).
drop_stale_fun_meta(_, _).

%Print compiled clause:
maybe_print_compiled_clause(_, _, _) :- silent(true), !.
maybe_print_compiled_clause(Label, FormTerm, Clause) :-
    swrite(FormTerm, FormStr),
    format("\e[33m-->  ~w  -->~n\e[36m~w~n\e[33m--> prolog clause -->~n\e[32m", [Label, FormStr]),
    portray_clause(current_output, Clause),
    format("\e[33m^^^^^^^^^^^^^^^^^^^^^~n\e[0m").

%Conjunction builder, turning goals list to a flat conjunction:
goals_list_to_conj([], true)      :- !.
goals_list_to_conj([G], G)        :- !.
goals_list_to_conj([G|Gs], (G,R)) :- goals_list_to_conj(Gs, R).

incomplete_application_kind(Fun, Arity, partial) :- ( arity(Fun, KnownArity), KnownArity >= Arity
                                                     ; \+ arity(Fun, _) ), !.
incomplete_application_kind(_, _, overapplied).

throw_function_overapplication(Fun, ActualInputArity) :-
    findall(InputArity, (arity(Fun, Arity), InputArity is Arity - 1), InputArities),
    sort(InputArities, KnownInputArities),
    throw(error(domain_error(function_input_arities(Fun, KnownInputArities), ActualInputArity), none)).

% Runtime dispatcher: call F if it's a registered fun/1, else keep as list:
reduce([F|Args], Out) :- nonvar(F), atom(F), fun(F)
                         -> % --- Case 1: callable predicate ---
                            length(Args, N),
                            Arity is N + 1,
                            ( current_predicate(F/Arity) , \+ (current_op(_, _, F), Arity =< 2)
                              -> append(Args,[Out],CallArgs),
                                 Goal =.. [F|CallArgs],
                                 catch(call(Goal),_,fail)
                            ; incomplete_application_kind(F, Arity, partial)
                              -> Out = partial(F,Args)
                               ; throw_function_overapplication(F, N) )
                          ; % --- Case 2: partial closure ---
                            compound(F), F = partial(Base, Bound) -> append(Bound, Args, NewArgs),
                                                                     reduce([Base|NewArgs], Out)
                          ; % --- Case 3: leave unevaluated ---
                            Out = [F|Args],
                            \+ cyclic_term(Out).

%Calling reduce from aggregate function foldall needs this argument wrapping
agg_reduce(AF, Acc, Val, NewAcc) :- reduce([AF, Acc, Val], NewAcc).

%Combined expr translation to goals list
translate_expr_to_conj(Input, Conj, Out) :- translate_expr(Input, Goals, Out),
                                            goals_list_to_conj(Goals, Conj).

%Special stream operation rewrite rules before main translation
rewrite_streamops(['trace!', Arg1, Arg2],
                  [progn, ['println!', Arg1], Arg2]).
rewrite_streamops([unique, Arg],
                  [call, [superpose, ['unique-atom', [collapse, Arg]]]]).
rewrite_streamops(['alpha-unique', Arg],
                  [call, [superpose, ['alpha-unique-atom', [collapse, Arg]]]]).
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

%Turn MeTTa code S-expression into goals list:
translate_expr(X, [], X)          :- ((var(X) ; atomic(X)) ; X = partial(_,_)), !.
translate_expr([H0|T0], Goals, Out) :-
        safe_rewrite_streamops([H0|T0],[H|T]),
        translate_expr(H, GsH, HV),
        %--- Translator rules ---:
        ( nonvar(HV), translator_rule(HV) -> length(T, NHook),
                                             ( once(fn_decl_arity(HV, NHook, ArgTypes, _))
                                               -> translate_args_by_type(T, ArgTypes, GsT, T1)
                                                ; translate_args(T, GsT, T1) ),
                                             append(T1,[Gs],Args),
                                             HookCall =.. [HV|Args],
                                             call(HookCall),
                                             translate_expr(Gs, GsE, Out),
                                             append([GsH,GsT,GsE],Goals)
        %--- Non-determinism ---:
        ; HV == superpose, T = [Args], is_list(Args) -> build_superpose_branches(Args, Out, Branches),
                                                        disj_list(Branches, Disj),
                                                        append(GsH, [Disj], Goals)
        ; HV == collapse, T = [E] -> translate_expr_to_conj(E, Conj, EV),
                                     %always a list; a single element type is carried, several
                                     %become a union (an open variable would later unify with a
                                     %concrete requirement and wrongly certify a mixed list):
                                     collapse_elem_type(EV, ET),
                                     set_out_type(Out, ['List', ET]),
                                     append(GsH, [findall(EV, Conj, Out)], Goals)
        ; HV == cut, T = [] -> append(GsH, [(!)], Goals),
                               Out = true
        ; HV == test, T = [Expr, Expected] -> translate_expr_to_conj(Expr, Conj, Val),
                                              translate_expr(Expected, GsE, ExpVal),
                                              Goal1 = ( findall(Val, Conj, Results),
                                                        (Results = [Actual] -> true
                                                                             ; Actual = Results ) ),
                                              append(GsH, [Goal1], G1),
                                              append(G1, GsE, G2),
                                              append(G2, [test(Actual, ExpVal, Out)], Goals)
        ; HV == once, T = [X] -> translate_expr_to_conj(X, Conj, Out),
                                 append(GsH, [once(Conj)], Goals)
        ; HV == hyperpose, T = [L]
          -> ( nonvar(L), is_list(L)
               -> build_hyperpose_branches(L, Branches),
                  maplist({Out}/[(_,Res)]>>note_candidates(Out, Res), Branches),
                  append(GsH, [concurrent_and(member((Goal,Res), Branches), (call(Goal), Out = Res))], Goals)
               ; translate_expr(L, GsL, LV),
                 append(GsH, GsL, Inner),
                 append(Inner, [hyperpose_runtime(LV, Out)], Goals) )
        ; HV == with_mutex, T = [M,X] -> translate_expr_to_conj(X, Conj, Out),
                                         append(GsH, [with_mutex(M,Conj)], Goals)
        ; HV == transaction, T = [X] -> translate_expr_to_conj(X, Conj, Out),
                                        append(GsH, [transaction(Conj)], Goals)
        %--- Sequential execution ---:
        ; HV == progn, T = Exprs -> translate_args(Exprs, GsList, Outs),
                                    append(GsH, GsList, Tmp),
                                    last(Outs, Out),
                                    Goals = Tmp
        ; HV == prog1, T = Exprs -> Exprs = [First|Rest],
                                    translate_expr(First, GsF, Out),
                                    translate_args(Rest, GsRest, _),
                                    append(GsH, GsF, Tmp1),
                                    append(Tmp1, GsRest, Goals)
        %--- Conditionals ---:
        ; HV == if, T = [Cond, Then] -> translate_if_cond(Cond, ConC, CondGoal),
                                        translate_expr_to_conj(Then, ConT, Tv),
                                        build_branch(ConT, Tv, Out, BT),
                                        ( ConC == true -> append(GsH, [ ( CondGoal -> BT ) ], Goals)
                                                        ; append(GsH, [ ( ConC, ( CondGoal -> BT ) ) ], Goals) )
        ; HV == if, T = [Cond, Then, Else] -> translate_if_cond(Cond, ConC, CondGoal),
                                              translate_expr_to_conj(Then, ConT, Tv),
                                              translate_expr_to_conj(Else, ConE, Ev),
                                              build_branch(ConT, Tv, Out, BT),
                                              build_branch(ConE, Ev, Out, BE),
                                              ( ConC == true -> append(GsH, [ (CondGoal -> BT ; BE) ], Goals)
                                                              ; append(GsH, [ (ConC, (CondGoal -> BT ; BE)) ], Goals) )
        ; HV == case, T = [KeyExpr, PairsExpr] -> ( select(Found0, PairsExpr, Rest0),
                                                    subsumes_term(['Empty', _], Found0),
                                                    Found0 = ['Empty', DefaultExpr],
                                                    NormalCases = Rest0
                                                    -> translate_expr_to_conj(KeyExpr, GkConj, Kv),
                                                       translate_case(NormalCases, Kv, Out, CaseGoal, KeyGoal),
                                                       translate_expr_to_conj(DefaultExpr, ConD, DOut),
                                                       build_branch(ConD, DOut, Out, DefaultThen),
                                                       Combined = ( (GkConj, CaseGoal) ;
                                                                    \+ GkConj, DefaultThen ),
                                                       append([GsH, KeyGoal, [Combined]], Goals)
                                                     ; translate_expr(KeyExpr, Gk, Kv),
                                                       translate_case(PairsExpr, Kv, Out, IfGoal, KeyGoal),
                                                       append([GsH, Gk, KeyGoal, [IfGoal]], Goals) )
        %--- Short-circuit boolean operators ---:
        ; HV == 'and-then', T = [A, B] -> translate_expr_to_conj(A, ConjA, Av),
                                           translate_expr_to_conj(B, ConjB, Bv),
                                           check_call_arg(declared, 'and-then', Av, 'Bool', GsA),
                                           check_call_arg(declared, 'and-then', Bv, 'Bool', GsB),
                                           goals_list_to_conj(GsA, GA), goals_list_to_conj(GsB, GB),
                                           set_out_type(Out, 'Bool'),
                                           append(GsH, [(ConjA, GA, (Av == true -> (ConjB, GB, Out = Bv) ; Out = false))], Goals)
        ; HV == 'or-else', T = [A, B] -> translate_expr_to_conj(A, ConjA, Av),
                                          translate_expr_to_conj(B, ConjB, Bv),
                                          check_call_arg(declared, 'or-else', Av, 'Bool', GsA),
                                          check_call_arg(declared, 'or-else', Bv, 'Bool', GsB),
                                          goals_list_to_conj(GsA, GA), goals_list_to_conj(GsB, GB),
                                          set_out_type(Out, 'Bool'),
                                          append(GsH, [(ConjA, GA, (Av == true -> Out = true ; (ConjB, GB, Out = Bv)))], Goals)
        %--- Unification constructs ---:
        ; (HV == let ; HV == chain), T = [Pat, Val, In] -> translate_expr(Pat, Gp, Pv),
                                                           translate_expr(Val, Gv, V),
                                                           note_candidates(Pv, V),        %the bound variable gets the value's type
                                                           bind_pattern_from(Pat, V),     %destructuring patterns type their fields
                                                           translate_expr(In,  Gi, Out),
                                                           append([GsH,[(Pv=V)],Gp,Gv,Gi], Goals)
        ; HV == 'let*', T = [Binds, Body] -> letstar_to_rec_let(Binds,Body,RecLet),
                                             translate_expr(RecLet,  Goals, Out)
        ; HV == sealed, T = [Vars, Expr] -> translate_expr_to_conj(Expr, Con, Val),
                                            note_candidates(Out, Val),
                                            Goals = [copy_term(Vars,[Con,Val],_,[Ncon,Out]),Ncon]
        %--- Iterating over non-deterministic generators without reification ---:
        ; HV == 'forall', T = [GF, TF]
          -> ( is_list(GF) -> GF = [GFH|GFA],
                              translate_expr(GFH, GsGFH, GFHV),
                              translate_args(GFA, GsGFA, GFAv),
                              append(GsGFH, GsGFA, GsGF),
                              GenList = [GFHV|GFAv]
                            ; translate_expr(GF, GsGF, GFHV),
                              GenList = [GFHV] ),
             translate_expr(TF, GsTF, TFHV),
             TestList = [TFHV, V],
             goals_list_to_conj(GsGF, GPre),
             GenGoal = (GPre, reduce(GenList, V)),
             append(GsH, GsTF, Tmp0),
             set_out_type(Out, 'Bool'),
             append(Tmp0, [( forall(GenGoal, ( reduce(TestList, Truth), Truth == true )) -> Out = true ; Out = false )], Goals)
        ; HV == 'foldall', T = [AF, GF, InitS]
          -> translate_expr_to_conj(InitS, ConjInit, Init),
             translate_expr(AF, GsAF, AFV),
             ( GF = [M|_], (M==match ; M==let ; M=='let*') -> LambdaGF = ['|->', [], GF],
                                                              translate_expr(LambdaGF, GsGF, GFHV),
                                                              GenList = [GFHV]
             ; is_list(GF) -> GF = [GFH|GFA],
                              translate_expr(GFH, GsGFH, GFHV),
                              translate_args(GFA, GsGFA, GFAv),
                              append(GsGFH, GsGFA, GsGF),
                              GenList = [GFHV|GFAv]
                            ; translate_expr(GF, GsGF, GFHV),
                              GenList = [GFHV] ),
             append(GsH, GsAF, Tmp1),
             append(Tmp1, GsGF, Tmp2),
             foldall_out_type(AFV, Init, Out),
             append(Tmp2, [ConjInit, foldall(agg_reduce(AFV, V), reduce(GenList, V), Init, Out)], Goals)
        %--- Higher-order functions with pseudo-lambdas and lambdas ---:
        ; HV == 'foldl-atom', T = [List, Init, AccVar, XVar, Body]
          -> translate_expr_to_conj(List, ConjList, L),
             translate_expr_to_conj(Init, ConjInit, InitV),
             note_list_elem_type(XVar, L),
             translate_expr_to_conj(Body, BodyConj, BG),
             exclude(==(true), [ConjList, ConjInit], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [foldl([XVar, AccVar, NewAcc]>>(BodyConj, ( number(BG) -> NewAcc is BG ; NewAcc = BG )), L, InitV, Out)], Goals)
        ; HV == 'map-atom', T = [List, XVar, Body]
          -> translate_expr_to_conj(List, ConjList, L),
             note_list_elem_type(XVar, L),
             translate_expr_to_conj(Body, BodyCallConj, BodyCall),
             ( value_single_type(BodyCall, BT) -> set_out_type(Out, ['List', BT]) ; true ),
             exclude(==(true), [ConjList], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [maplist([XVar, Y]>>(BodyCallConj, ( number(BodyCall) -> Y is BodyCall ; Y = BodyCall )), L, Out)], Goals)
        ; HV == 'filter-atom', T = [List, XVar, Cond]
          -> translate_expr_to_conj(List, ConjList, L),
             note_list_elem_type(XVar, L),
             translate_expr_to_conj(Cond, CondConj, CondGoal),
             exclude(==(true), [ConjList], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [include([XVar]>>(CondConj, CondGoal), L, Out)], Goals)
        ; HV == '|->', T = [Args, Body] -> next_lambda_name(F),
                                           % find free (non-argument) variables in Body
                                           term_variables(Body, AllVars),
                                           term_variables(Args, ArgVars),
                                           exclude({ArgVars}/[V]>>memberchk_eq(V, ArgVars), AllVars, FreeVars),
                                           append(FreeVars, Args, FullArgs),
                                           % compile clause with all bound + free vars
                                           translate_clause([=, [F|FullArgs], Body], Clause),
                                           register_fun(F),
                                           assertz(Clause),
                                           format(atom(Label), "metta lambda (~w)", [F]),
                                           maybe_print_compiled_clause(Label, ['|->', Args, Body], Clause),
                                           length(FullArgs, N),
                                           Arity is N + 1,
                                           (arity(F, Arity) -> true ; assertz(arity(F, Arity))),
                                           % emit closure capturing the environment (free vars)
                                           ( FreeVars == [] -> Out = F
                                                             ; Out = partial(F, FreeVars) )
        %--- Spaces ---:
        ; ( HV == 'add-atom' ; HV == 'remove-atom' ), T = [Space,Atom] ->
                                                                   translate_expr(Space, G1, S),
                                                                   Goal =.. [HV,S,Atom,Out],
                                                                   set_out_type(Out, 'Bool'),
                                                                   append([GsH,G1,[Goal]], Goals)
        ; HV == match, T = [Space, Pattern, Body] -> translate_expr(Space, G1, S),
                                                     type_match_pattern(Pattern),
                                                     translate_expr(Body, GsB, Out),
                                                     append(G1, [match(S, Pattern, Out, Out)], G2),
                                                     append(G2, GsB, Goals)
        %--- Predicate to compiled goal ---:
        ; HV == translatePredicate, T = [Expr] -> Expr = [S|Args],
                                                  translate_args(Args, GsArgs, ArgsOut),
                                                  Goal =.. [S|ArgsOut],
                                                  append(GsH, GsArgs, Inner),
                                                  append(Inner, [Goal], Goals)
        %--- Manual dispatch options: ---
        %Generate a predicate call on compilation, translating Args for nesting:
        ; HV == call,  T = [Expr] -> Expr = [F|Args],
                                     translate_args(Args, GsArgs, ArgsOut),
                                     append(GsH, GsArgs, Inner),
                                     append(ArgsOut, [Out], CallArgs),
                                     Goal =.. [F|CallArgs],
                                     length(Args, NC),
                                     manual_dispatch_arg_checks(F, NC, ArgsOut, GuardGs),
                                     set_unique_decl_out(F, NC, Out),
                                     append(Inner, GuardGs, Inner1),
                                     append(Inner1, [Goal], Goals)
        %Produce a dynamic dispatch, translating Args for nesting:
        ; HV == reduce, T = [Expr] -> ( var(Expr) -> translate_expr(Expr, GsH, ExprOut),
                                                     Goals = [reduce(ExprOut, Out)|GsH]
                                                   ; Expr = [F|Args],
                                                     translate_args(Args, GsArgs, ArgsOut),
                                                     append(GsH, GsArgs, Inner),
                                                     ExprOut = [F|ArgsOut],
                                                     length(Args, NR),
                                                     manual_dispatch_arg_checks(F, NR, ArgsOut, GuardGs),
                                                     set_unique_decl_out(F, NR, Out),
                                                     append(Inner, GuardGs, Inner1),
                                                     append(Inner1, [reduce(ExprOut, Out)], Goals) )
        %Invoke translator to evaluate MeTTa code as data/list:
        ; HV == eval, T = [Arg] -> ( nonvar(Arg), Arg = [Q, Quoted], Q == quote
                                     -> translate_expr(Quoted, GsQ, Out),           %(eval (quote E)) == E
                                        append(GsH, GsQ, Goals)
                                      ; append(GsH, [eval(Arg, Out)], Goals) )
        %Explicit type ascription for dynamically typed values:
        ; HV == the, T = [TypeExpr, Expr] -> translate_expr(Expr, GsE, Out0),
                                             normalize_type(TypeExpr, TN),
                                             ascribe_type(Out0, TN, GsA),
                                             %An ascribed literal keeps the author's type when it is
                                             %more specific than the value's own (e.g. (the (List Item) ())):
                                             ( nonvar(Out0), nonvar(TN), \+ wildcard_type_t(TN),
                                               \+ ( value_single_type(Out0, VT), VT == TN )
                                               -> add_known_type(Out, TN),
                                                  append([GsH, GsE, GsA, [Out = Out0]], Goals)
                                                ; Out = Out0,
                                                  append([GsH, GsE, GsA], Goals) )
        %Force arg to remain data/list:
        ; HV == quote, T = [Expr] -> append(GsH, [], Inner),
                                     Out = Expr,
                                     Goals = Inner
        ; HV == 'catch', T = [Expr] ->
          translate_expr(Expr, GsExpr, ExprOut),
          append(GsH, [], Inner),
          goals_list_to_conj(GsExpr, Conj),
          Goal = catch((Conj, Out = ExprOut),
                       Exception,
                       (Exception = error(Type, Ctx) -> Out = ['Error', Type, Ctx]
                                                      ; Out = ['Error', Exception])),
          append(Inner, [Goal], Goals)
        %--- Automatic 'smart' dispatch, translator deciding when to create a predicate call, data list, or dynamic dispatch: ---
        %Known function or closure => type-directed call:
        ; ( atom(HV), fun(HV) -> Fun = HV, Bound = []
          ; compound(HV), HV = partial(Fun, Bound) )
          -> translate_typed_call(Fun, Bound, T, GsH, Goals, Out)
        %Literals (numbers, strings, etc.), known non-function atom => data:
        ; ( atomic(HV), \+ atom(HV) ; atom(HV), \+ fun(HV) )
          -> translate_args(T, GsT, AVs),
             append(GsH, GsT, Goals),
             Out = [HV|AVs]
        %Plain data list: evaluate inner fun-sublists
        ; is_list(HV) -> translate_args(T, GsT, AVs),
                         append(GsH, GsT, Inner),
                         eval_data_term(HV, Gd, HV1),
                         append(Inner, Gd, Goals),
                         Out = [HV1|AVs]
        %Unknown head (var/compound) => runtime dispatch, with a lean closure
        %application when the head's arrow type is known or assumed. A head
        %whose known type is provably not a function (e.g. Number) makes the
        %expression data construction, compiled and typed as such:
        ; translate_args(T, GsT, AVs),
          append(GsH, GsT, Inner),
<<<<<<< HEAD
          ( var(HV), AVs == []               %singleton ($x) is data, never an application
            -> Out = [HV],
               Goals = Inner
          ; translate_closure_call(HV, AVs, Inner, Goals, Out) -> true
          ; var(HV), known_singleton(HV, K), nonfunction_type(K)
            -> Out = [HV|AVs],
               Goals = Inner
          ; append(Inner, [reduce([HV|AVs], Out)], Goals) ) ).

%Values of these types can never dispatch as functions in reduce/2:
nonfunction_type(K) :- nonvar(K), ( primitive_type(K)
                                  ; is_list(K), \+ is_arrow_type(K) ).

%A variable head with a known arrow type of matching arity is a closure call:
%check the args against the arrow, dispatch through apply_fn (skipping reduce's
%per-call bookkeeping), and propagate the output type. Applying a parameter
%whose type is still an unbound assumption tells us it is a function.
translate_closure_call(HV, AVs, Inner, Goals, Out) :- var(HV), AVs \== [], known_singleton(HV, K),
                                                      length(AVs, N), N1 is N + 1,
                                                      ( var(K) -> length(Xs, N1), K = [->|Xs]
                                                      ; K = [H|Xs], ( H == (->) ; H == '-[nondet]->' ),
                                                        length(Xs, N1) ),
                                                      append(ArgTs, [OutT], Xs),
                                                      apply_call_args(declared, closure, AVs, ArgTs, GuardGs),
                                                      append(Inner, GuardGs, Inner1),
                                                      closure_apply_goal(HV, AVs, Out, Goal),
                                                      append(Inner1, [Goal], Goals),
                                                      %a variable arrow output is knowledge too: it is the
                                                      %declaration-instance type var shared with the context
                                                      %(e.g. map-flat's element type), not an unknown:
                                                      ( var(Out), var(OutT) -> add_known_type(Out, OutT)
                                                                             ; set_out_type(Out, OutT) ).
%Underapplying a typed closure parameter builds a partial: the used argument
%positions are checked and the result carries the remaining arrow:
translate_closure_call(HV, AVs, Inner, Goals, Out) :- var(HV), AVs \== [], known_singleton(HV, K),
                                                      nonvar(K), K = [H|Xs],
                                                      ( H == (->) ; H == '-[nondet]->' ),
                                                      length(AVs, N), length(Xs, LX), N < LX - 1,
                                                      append(ArgTs, [OutT], Xs),
                                                      length(UsedTs, N), append(UsedTs, RestTs, ArgTs),
                                                      apply_call_args(declared, closure, AVs, UsedTs, GuardGs),
                                                      append(Inner, GuardGs, Inner1),
                                                      append(Inner1, [reduce([HV|AVs], Out)], Goals),
                                                      append(RestTs, [OutT], RXs),
                                                      ( var(Out) -> add_known_type(Out, [H|RXs]) ; true ).

closure_apply_goal(HV, [A], Out, apply_fn1(HV, A, Out)) :- !.
closure_apply_goal(HV, [A, B], Out, apply_fn2(HV, A, B, Out)) :- !.
closure_apply_goal(HV, [A, B, C], Out, apply_fn3(HV, A, B, C, Out)) :- !.
closure_apply_goal(HV, AVs, Out, apply_fnN(HV, AVs, Out)).

%Runtime closure application; the last clause preserves reduce/2 semantics for
%values (including unbound heads used symbolically) that are not callable.
%A missing predicate (e.g. an arity the arrow type did not predict) fails like
%it always did - errors raised inside the callee propagate:
apply_fn1(F, A, Out) :- atom(F), fun(F), !, safe_apply(call(F, A, Out)).
apply_fn1(P, A, Out) :- compound(P), P = partial(F, Bs), !,
                        append(Bs, [A, Out], CallArgs),
                        Goal =.. [F|CallArgs], safe_apply(Goal).
apply_fn1(F, A, Out) :- reduce([F, A], Out).

apply_fn2(F, A, B, Out) :- atom(F), fun(F), !, safe_apply(call(F, A, B, Out)).
apply_fn2(P, A, B, Out) :- compound(P), P = partial(F, Bs), !,
                           append(Bs, [A, B, Out], CallArgs),
                           Goal =.. [F|CallArgs], safe_apply(Goal).
apply_fn2(F, A, B, Out) :- reduce([F, A, B], Out).

apply_fn3(F, A, B, C, Out) :- atom(F), fun(F), !, safe_apply(call(F, A, B, C, Out)).
apply_fn3(P, A, B, C, Out) :- compound(P), P = partial(F, Bs), !,
                              append(Bs, [A, B, C, Out], CallArgs),
                              Goal =.. [F|CallArgs], safe_apply(Goal).
apply_fn3(F, A, B, C, Out) :- reduce([F, A, B, C], Out).

apply_fnN(F, Args, Out) :- atom(F), fun(F), !, append(Args, [Out], CallArgs),
                           Goal =.. [F|CallArgs], safe_apply(Goal).
apply_fnN(P, Args, Out) :- compound(P), P = partial(F, Bs), !,
                           append(Bs, Args, All), append(All, [Out], CallArgs),
                           Goal =.. [F|CallArgs], safe_apply(Goal).
apply_fnN(F, Args, Out) :- reduce([F|Args], Out).

safe_apply(Goal) :- catch(Goal, error(existence_error(procedure, _), _), fail).

%Type-directed function call: check declared types at compile time, resolve
%overloads statically when possible, and emit runtime guards only where types
%stay unresolved (see AGENTS.md).
translate_typed_call(Fun, Bound, Args, GsH, Goals, Out) :-
        length(Args, NProv), length(Bound, NB), NTotal is NProv + NB,
        findall(ft(ATs, OT), fn_decl_arity(Fun, NTotal, ATs, OT), FullDecls),
        ( FullDecls \== []
          -> eff_arg_types(FullDecls, NB, NProv, EffTs),
             translate_args_by_type(Args, EffTs, GsT, AVs0),
             append(Bound, AVs0, AVs),
             ( FullDecls = [Single] -> Chosen = Single, MultiDecl = false
             ; MultiDecl = true,
               include(decl_survives(AVs), FullDecls, Survivors),
               ( Survivors == [] -> throw(error(no_matching_overload(Fun), typecheck))
               ; Survivors = [OneLeft] -> Chosen = OneLeft
               ; Chosen = multi(Survivors) ) ),
             ( Chosen = ft(ATs, OT)
               -> apply_call_args(declared, Fun, AVs, ATs, GuardGs),
                  append([GsH, GsT, GuardGs], Inner),
                  %overloaded functions: clauses were not output-checked against a
                  %single declaration, so the call filters on the output type:
                  overload_out_guard(MultiDecl, Fun, Out, OT, Extra),
                  ( MultiDecl == false, arith_inline(Fun, AVs, Out, ArithGs)
                    -> append(Inner, ArithGs, Goals)
                     ; build_call_or_partial(Fun, AVs, Out, Inner, Extra, Goals) ),
                  set_call_out_type(Out, ATs, OT)
                ; Chosen = multi(Survs),
                  maplist(overload_branch(Fun, AVs, Out), Survs, Branches),
                  disj_list(Branches, Disj),
                  append(GsH, GsT, Pre),
                  append(Pre, [goal_or_throw(Disj, error(no_matching_overload(Fun), typecheck))], Goals) )
        ; findall(pt(PTs, RTs, OT), fn_decl_partial(Fun, NTotal, PTs, RTs, OT), PartDecls),
          PartDecls = [pt(PTs, _, _)]
          -> translate_args(Args, GsT, AVs0),                      %typed partial application
             append(Bound, AVs0, AVs),
             apply_call_args(declared, Fun, AVs, PTs, GuardGs),
             append([GsH, GsT, GuardGs], Inner),
             build_direct_call(Fun, AVs, Out, Inner, [], Goals)
        ; assumed_self_decl(Fun, NTotal, PTs, OutTv)
          -> translate_args(Args, GsT, AVs0),                      %self-recursion under the provisional type
             append(Bound, AVs0, AVs),                             %(before the store: earlier clauses' inference
             apply_call_args(inferred, Fun, AVs, PTs, GuardGs),    %is stale while later clauses widen it)
             append([GsH, GsT, GuardGs], Inner),
             build_call_or_partial(Fun, AVs, Out, Inner, [], Goals),
             ( var(Out) -> add_known_type(Out, OutTv) ; true )
        ; findall(it(IATs, IOT), inferred_decl_arity(Fun, NTotal, IATs, IOT), [it(IATs, IOT)])
          -> translate_args(Args, GsT, AVs0),                      %inferred type: knowledge only, never rejects
             append(Bound, AVs0, AVs),
             apply_call_args(inferred, Fun, AVs, IATs, GuardGs),
             append([GsH, GsT, GuardGs], Inner),
             build_call_or_partial(Fun, AVs, Out, Inner, [], Goals),
             set_out_type(Out, IOT)
        ; translate_args(Args, GsT, AVs0),                         %no type information
          append(Bound, AVs0, AVs),
          append(GsH, GsT, Inner),
          build_call_or_partial(Fun, AVs, Out, Inner, [], Goals),
          ( untyped_call_out(Fun, AVs, Out) -> true ; true ) ).

%A provided arg position stays untranslated data iff every declaration types it
%Expression; the effective type feeds translate_args_by_type, which only ever
%distinguishes 'Expression' from everything else:
eff_arg_types(FullDecls, NB, NProv, Ts) :- NEnd is NB + NProv - 1,
                                           ( NProv =:= 0 -> Ts = []
                                           ; numlist(NB, NEnd, Is),
                                             maplist(eff_arg_type(FullDecls), Is, Ts) ).
eff_arg_type(FullDecls, I, T) :- ( forall(member(ft(ATs, _), FullDecls),
                                          ( nth0(I, ATs, Ty), Ty == 'Expression' ))
                                   -> T = 'Expression' ; true ).

%Expression-typed args stay unevaluated data, except underapplied callable
%expressions representable as a goal-free closure. Only expressions that can
%actually become a closure are translated, so plain data is never re-translated:
expression_arg_value(A, AV) :- ( maybe_closure_expr(A),
                                 catch(( translate_expr(A, GsExpr, AVExpr),
                                         trivial_goals(GsExpr),
                                         callable_expression_value(AVExpr) ),
                                       error(_, typecheck), fail)
                                 -> AV = AVExpr
                                  ; AV = A ).

%An underapplied call to a known function (would compile to partial(...)):
maybe_closure_expr([F|Args]) :- atom(F), fun(F), is_list(Args),
                                length(Args, N), Arity is N + 1,
                                \+ ( ( current_predicate(F/Arity) ; catch(arity(F, Arity), _, fail) ),
                                     \+ ( current_op(_, _, F), Arity =< 2 ) ).

trivial_goals([]).
trivial_goals([true|Gs]) :- trivial_goals(Gs).

callable_expression_value(AV) :- atom(AV), fun(AV).
callable_expression_value(partial(Fun, Bound)) :- atom(Fun), ground(Bound).

%One dispatch branch per surviving overload: non-throwing guards, then the call:
overload_branch(Fun, AVs, Out, ft(ATs, OT), Branch) :- maplist(overload_branch_guard(Fun), AVs, ATs, Gss),
                                                       append(Gss, GuardGs),
                                                       overload_out_guard(true, Fun, Out, OT, Extra),
                                                       build_direct_call(Fun, AVs, Out, GuardGs, Extra, BranchGoals),
                                                       goals_list_to_conj(BranchGoals, Branch).

overload_out_guard(MultiDecl, Fun, Out, OT, Extra) :- ( MultiDecl == true, ground(OT), \+ wildcard_type_t(OT)
                                                        -> ( strict_mode(true)
                                                             -> throw(error(strict_runtime_typecheck(Fun, typecheck_match(Out, OT)), typecheck))
                                                              ; Extra = [typecheck_match(Out, OT)] )
                                                         ; Extra = [] ).

overload_branch_guard(Fun, AV, T, G) :- ( arg_statically_ok(AV, T) -> G = []
                                        ; strict_mode(true)
                                          -> throw(error(strict_runtime_typecheck(Fun, typecheck_match(AV, T)), typecheck))
                                           ; G = [typecheck_match(AV, T)] ).

%Type-resolved builtin arithmetic compiles to native is/2, constant-folded when
%both operands are literals. Only while the builtin definition is untouched:
arith_inline(Fun, [A, B], Out, Gs) :- arith_op(Fun, A, B, Expr),
                                      builtin_untouched(Fun),
                                      ( number(A), number(B)
                                        -> catch((Out is Expr, Gs = []), _, Gs = [Out is Expr])
                                         ; Gs = [Out is Expr] ).

arith_op('+', A, B, A + B).
arith_op('-', A, B, A - B).
arith_op('*', A, B, A * B).
arith_op('/', A, B, A / B).
arith_op('%', A, B, A mod B).
arith_op(min, A, B, min(A, B)).
arith_op(max, A, B, max(A, B)).

builtin_untouched(F) :- functor(H, F, 3), predicate_property(H, number_of_clauses(1)).

%Reified comparisons whose result only feeds an if-condition compile to the
%native comparison, skipping the true/false round-trip:
translate_if_cond(Cond, PreConj, CondGoal) :- translate_expr(Cond, GsC, Cv),
                                              ( var(Cv), append(Pre, [Last], GsC), reified_cond(Last, Cv, Native)
                                                -> goals_list_to_conj(Pre, PreConj), CondGoal = Native
                                                 ; goals_list_to_conj(GsC, PreConj), CondGoal = (Cv == true) ).

reified_cond(G, Cv, Native) :- nonvar(G), G =.. [F, A, B, R], R == Cv,
                               cmp_native(F, A, B, Native),
                               builtin_untouched(F).

cmp_native('<', A, B, (A < B)).
cmp_native('<=', A, B, (A =< B)).
cmp_native('>', A, B, (A > B)).
cmp_native('>=', A, B, (A >= B)).
cmp_native('==', A, B, (A == B)).
cmp_native('!=', A, B, (A \== B)).
=======
          %Known function => direct call:
          ( is_list(AVs), 
            ( atom(HV), fun(HV), Fun = HV, AllAVs = AVs, IsPartial = false
            ; compound(HV), HV = partial(Fun, Bound), append(Bound,AVs,AllAVs), IsPartial = true
            ) % Check for type definition [:,HV,TypeChain]
            -> findall(TypeChain, catch(match('&self', [':', Fun, TypeChain], TypeChain, TypeChain), _, fail), TypeChains),
               list_to_set(TypeChains, UniqueTypeChains),
               ( UniqueTypeChains \= []
                 -> length(AllAVs, InputArity),
                    Arity is InputArity + 1,
                    ( incomplete_application_kind(Fun, Arity, ApplicationKind), ApplicationKind == overapplied
                      -> append(GsH, [throw_function_overapplication(Fun, InputArity)], Goals)
                       ; maplist({Fun,T,GsH,IsPartial,Bound,Out}/[TypeChain,BranchGoal]>>(
                                 typed_functioncall_branch(Fun, TypeChain, T, GsH, IsPartial, Bound, Out, BranchGoal)), UniqueTypeChains, Branches),
                         disj_list(Branches, Disj),
                         Goals = [Disj] )
              ; build_call_or_partial(Fun, AllAVs, Out, Inner, [], Goals))
          %Literals (numbers, strings, etc.), known non-function atom => data:
          ; ( atomic(HV), \+ atom(HV) ; atom(HV), \+ fun(HV) ) -> Out = [HV|AVs],
                                                                  Goals = Inner
          %Plain data list: evaluate inner fun-sublists
          ; is_list(HV) -> eval_data_term(HV, Gd, HV1),
                           append(Inner, Gd, Goals),
                           Out = [HV1|AVs]
          %Unknown head (var/compound) => runtime dispatch:
          ; append(Inner, [reduce([HV|AVs], Out)], Goals) )).
>>>>>>> origin/main

%Generate actual function call or partial if arity not complete:
build_call_or_partial(Fun, AVs, Out, Inner, Extra, Goals) :- ( maybe_specialize_call(Fun, AVs, Out, Goal)
                                                               -> append(Inner, [Goal|Extra], Goals)
<<<<<<< HEAD
                                                                ; build_direct_call(Fun, AVs, Out, Inner, Extra, Goals) ).
=======
                                                                ; arity(Fun, Arity)
                                                                  -> append(AVs, [Out], Args),
                                                                     Goal =.. [Fun|Args],
                                                                     append(Inner, [Goal|Extra], Goals)
                                                                ; incomplete_application_kind(Fun, Arity, partial)
                                                                  -> Out = partial(Fun, AVs),
                                                                     append(Inner, Extra, Goals)
                                                                   ; append(Inner, [throw_function_overapplication(Fun, N)], Goals) ).

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
>>>>>>> origin/main

build_direct_call(Fun, AVs, Out, Inner, Extra, Goals) :- length(AVs, N),
                                                         Arity is N + 1,
                                                         ( ( current_predicate(Fun/Arity) ; catch(arity(Fun, Arity), _, fail) ),
                                                           \+ ( current_op(_, _, Fun), Arity =< 2 )
                                                           -> append(AVs, [Out], CallArgs),
                                                              Goal =.. [Fun|CallArgs],
                                                              append(Inner, [Goal|Extra], Goals)
                                                            ; Out = partial(Fun, AVs),
                                                              append(Inner, Extra, Goals) ).

%Selectively apply translate_args for non-Expression args while Expression args stay as data input:
translate_args_by_type([], _, [], []) :- !.
translate_args_by_type([A|As], [T|Ts], GsOut, [AV|AVs]) :-
                      ( T == 'Expression' -> expression_arg_value(A, AV), GsA = []
                                           ; translate_expr(A, GsA, AV) ),
                      translate_args_by_type(As, Ts, GsRest, AVs),
                      append(GsA, GsRest, GsOut).

%Handle data list:
eval_data_term(X, [], X) :- (var(X); atomic(X)), !.
eval_data_term([F|As], Goals, Val) :- ( atom(F), fun(F) -> translate_expr([F|As], Goals, Val)
                                                         ; eval_data_list([F|As], Goals, Val) ).

%Handle data list entry:
eval_data_list([], [], []).
eval_data_list([E|Es], Goals, [V|Vs]) :- ( is_list(E) -> eval_data_term(E, G1, V) ; V = E, G1 = [] ),
                                         eval_data_list(Es, G2, Vs),
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
build_branch(true, Val, Out, (Out = Val)) :- !, note_candidates(Out, Val).
build_branch(Con, Val, Out, Goal) :- var(Val) -> Val = Out, Goal = Con
                                               ; note_candidates(Out, Val),
                                                 Goal = (Val = Out, Con).

%Translate case expression recursively into nested if:
translate_case([[K,VExpr]|Rs], Kv, Out, Goal, KGo) :- ( var(Kv), known_singleton(Kv, KT)
                                                        -> bind_pattern_typed(K, KT) ; true ),
                                                      translate_expr_to_conj(VExpr, ConV, VOut),
                                                      constrain_args(K, Kc, Gc),
                                                      build_branch(ConV, VOut, Out, Then),
                                                      ( Rs == [] -> Goal = ((Kv = Kc) -> Then), KGi=[]
                                                                  ; translate_case(Rs, Kv, Out, Next, KGi),
                                                                    Goal = ((Kv = Kc) -> Then ; Next) ),
                                                      append([Gc,KGi], KGo).

%Translate arguments recursively:
translate_args([], [], []).
translate_args([X|Xs], Goals, [V|Vs]) :- translate_expr(X, G1, V),
                                         translate_args(Xs, G2, Vs),
                                         append(G1, G2, Goals).

%foldall's result type is the accumulator function's output type when that is
%uniquely declared or inferred. The initial value's type is deliberately NOT
%used as a fallback: the result comes from the accumulator function, and the
%init only surfaces when the generator is empty.
%foldall returns Init when the generator is empty, so the accumulator's
%output type is only trustworthy when the initial value fits it too:
foldall_out_type(AFV, Init, Out) :- ( atom(AFV),
                                      findall(OT, ( fn_decl_arity(AFV, 2, _, OT)
                                                  ; inferred_decl_arity(AFV, 2, _, OT) ), [OT1]),
                                      ( var(Init) -> ( known_singleton(Init, IT) -> \+ \+ type_unify(IT, OT1) ; true )
                                      ; value_single_type(Init, IT) -> \+ \+ type_unify(IT, OT1)
                                      ; true )
                                      -> set_out_type(Out, OT1)
                                       ; true ).

collapse_elem_type(EV, ET) :- ( var(EV), known_candidates(EV, Cs)
                                -> ( Cs = [C1] -> ET = C1 ; ET = ['|'|Cs] )
                              ; nonvar(EV), value_single_type(EV, ET0) -> ET = ET0
                              ; true ).

%Build A ; B ; C ... from a list:
disj_list([G], G).
disj_list([G|Gs], (G ; R)) :- disj_list(Gs, R).

%Build one disjunct per branch: (Conj, Out = Val):
build_superpose_branches([], _, []).
build_superpose_branches([E|Es], Out, [B|Bs]) :- translate_expr_to_conj(E, Conj, Val),
                                                 build_branch(Conj, Val, Out, B),
                                                 build_superpose_branches(Es, Out, Bs).

%Build hyperpose branch as a goal list for concurrent_maplist to consume:
build_hyperpose_branches([], []).
build_hyperpose_branches([E|Es], [(Goal, Res)|Bs]) :- translate_expr_to_conj(E, Goal, Res),
                                                      build_hyperpose_branches(Es, Bs).

%Runtime hyperpose path for variable/computed list arguments.
hyperpose_runtime(Exprs, Out) :- is_list(Exprs),
                                 concurrent_and(member(Expr, Exprs), eval(Expr, Out)).

%Like membercheck but with direct equality rather than unification
memberchk_eq(V, [H|_]) :- V == H, !.
memberchk_eq(V, [_|T]) :- memberchk_eq(V, T).

%Generate readable lambda name:
next_lambda_name(Name) :- ( catch(nb_getval(lambda_counter, Prev), _, Prev = 0) ),
                          N is Prev + 1,
                          nb_setval(lambda_counter, N),
                          format(atom(Name), 'lambda_~d', [N]).
