:- dynamic translated_from/2.


%Pattern matching, structural and functional/relational constraints on arguments:
constrain_args(X, X, []) :- (var(X); atomic(X)), !.
constrain_args([F, A, B], Out, Goals) :- nonvar(F),
                                         F == cons,
                                         constrain_args(A, A1, G1),
                                         constrain_args(B, B1, G2),
                                         Out = [A1|B1],
                                         append(G1, G2, Goals), !.
constrain_args(Pattern, Var, Goals) :-
                                        functional_pattern_application(Pattern, F, Args), !,
                                        translate_expr([F|Args], GoalsExpr, Var),
                                        flatten(GoalsExpr, Goals).
constrain_args(In, Out, Goals) :- maplist(constrain_args, In, Out, NestedGoalsList),
                                  flatten(NestedGoalsList, Goals), !.

%The single definition of a functional pattern position, matching
%constrain_args/3's dispatch: registered functions elaborate to calls, while
%cons and declaration-only constructors stay literal. Args must be a proper
%list; specialization may produce an open pattern [F|Tail] of no fixed arity.
functional_pattern_application([F|Args], F, Args) :-
    atom(F), F \== cons, fun(F),
    is_list(Args).

%Flatten (= Head Body) MeTTa function into a Prolog clause. Dependencies are
%those observed by all nested analyses; assertion sites attach them to the real
%clause reference with record_compiled_dependencies/3.
translate_clause(Input, Clause, ConstrainArgs, Dependencies) :-
        Input = [=, [F|Args], BodyExpr],
        atom(F),
        length(Args, N),
        copy_term_nat(BodyExpr, SourceBody),
        with_unified_clause_analysis(
            Input,
            analysis_collect(
                with_compiling_caller(
                    F, N,
                    translate_clause_core(Input, Clause, ConstrainArgs)),
                Events)),
        analysis_term_dependencies(SourceBody, TermDependencies),
        analysis_function_decl_dependencies(F, DeclDependencies),
        append([[decl(F/N), clause_set(F/N)],
                TermDependencies, DeclDependencies],
               ExtraDependencies),
        analysis_make_proof(compiled_clause(F/N), translated, Events,
                            ExtraDependencies, Proof),
        analysis_proof_dependencies(Proof, Dependencies),
        analysis_reemit_proof(Proof).

with_compiling_caller(F, N, Goal) :-
    ( catch(b_getval('$compiling_caller', Previous), _, fail) -> true ; Previous = none ),
    setup_call_cleanup(b_setval('$compiling_caller', F/N),
                       Goal,
                       b_setval('$compiling_caller', Previous)).

current_compiling_caller(F, N) :-
    catch(b_getval('$compiling_caller', F/N), _, fail).
translate_clause_core(Input, (Head :- BodyConj), ConstrainArgs) :-
                                               Input = [=, [F|Args0], BodyExpr],
                                               length(Args0, SourceArity),
                                               %The clause-set memo is stale before
                                               %this very clause is validated:
                                               analysis_cache_invalidate_event(
                                                   clause_changed(F/SourceArity,
                                                                  compiling)),
                                               ( ConstrainArgs -> maplist(constrain_args, Args0, Args1, GoalsA),
                                                                  flatten(GoalsA,GoalsPrefix)
                                                                ; Args1 = Args0, GoalsPrefix = [] ),
                                               %A head that elaborated to goals cannot
                                               %support selection or coverage proofs:
                                               ( GoalsPrefix == [] -> HeadForm = clean
                                               ; HeadForm = head_goals ),
                                               catch(nb_getval(F, Prev), _, Prev = []),
                                               nb_setval(F, [fun_meta(Args1, BodyExpr, HeadForm) | Prev]),
                                               clause_param_types(F, Args1, DeclOut),
                                               %Snapshot the declared arg positions that stay bare type variables after
                                               %head binding; checked below to enforce their claimed universality:
                                               parametric_param_snapshot(DeclOut, ParamVars),
                                               %...and publish them for the body: they are promises
                                               %to the caller, not knowledge (a specialized copy is an
                                               %instance and promises nothing new):
                                               ( ConstrainArgs == false -> Promises = [] ; Promises = ParamVars ),
                                               param_promises_scope(Promises, OuterPromises),
                                               %Specialized copies (ConstrainArgs == false) are instances
                                               %of validated clauses: bind their param types for guard
                                               %elimination but skip the determinism/strict checks.
                                               %Determinism runs after param binding so closure params
                                               %carry their declared arrows into the body analysis.
                                               ( ConstrainArgs == false -> true
                                                                         ; validate_function_determinism(F, Args1, BodyExpr,
                                                                                                         Prev, HeadForm) ),
                                               %Explicit -[det]->/-[semidet]-> arrows promise bound
                                               %arguments for the parameters the determinism proof
                                               %consumed: emit those checks before the commit cut.
                                               det_boundness_checks(F, Args1, DetChecks),
                                               begin_clause_inference(F, Args1, Assume, SavedInf),
                                               translate_declared_body(F, DeclOut, BodyExpr, GoalsBody, ExpOut),
                                               %A body that forced a declared parametric parameter to a concrete type
                                               %broke the universality its declaration claims (skip for specialized copies):
                                               ( ConstrainArgs == false -> true
                                                                         ; parametric_param_check(F, ParamVars) ),
                                               (  nonvar(ExpOut) , ExpOut = partial(Base,Bound)
                                               -> arity(Base, Arity), length(Bound, N), M is (Arity - N) - 1,
                                                  length(ExtraArgs, M), append(Bound, ExtraArgs, CallInArgs),
                                                  resolve_memoization(Base, CallInArgs, Out, Goal),
                                                  append(GoalsBody,[Goal],FinalGoals), append(Args1,ExtraArgs,HeadArgs),
                                                  OutChecks = [],
                                                  end_clause_inference(F, Args1, none, none, SavedInf)
                                               ; FinalGoals= GoalsBody , HeadArgs = Args1, Out = ExpOut,
                                                 end_clause_inference(F, Args1, ExpOut, Assume, SavedInf),
                                                 %A specialized copy is still output-certified: it can
                                                 %only discharge, reproduce or contradict the general
                                                 %clause's guard, and a contradiction means "do not
                                                 %specialize". Skipping it would drop a guard.
                                                 clause_output_goals(F, DeclOut, Args1, ExpOut,
                                                                     BodyExpr, OutChecks0),
                                                 oracle_output_check(DeclOut, Out, OutChecks0, OutChecks) ),
                                               ( ConstrainArgs == false -> true
                                                                         ; strict_check_function_typed(F, Args1) ),
                                               append(HeadArgs, [Out], FinalArgs),
                                               Head =.. [F|FinalArgs],
                                               length(FinalArgs, CompiledArity),
                                               (arity(F, CompiledArity) -> true ; assertz(arity(F, CompiledArity))),
                                               %Committed functions commit to the first matching
                                               %clause (non-overlap is validated): no choicepoints,
                                               %last-call optimization, semidet as cheap as det:
                                               ( clause_commit_cut(F, Args1) -> Commit = [!] ; Commit = [] ),
                                               param_promises_restore(OuterPromises),
                                               append([DetChecks, GoalsPrefix, Commit, FinalGoals, OutChecks], Goals),
                                               goals_list_to_conj(Goals, BodyConj).

%%% Committed-arrow boundness enforcement. A parameter is checked nonvar on
%%% entry only when the clause set's determinism proof consumed its boundness
%%% (enforced_bound_param/1 during a call-site strengthening), so a pure data
%%% constructor ((= (pair-up $x $y) ($x $y))) gets no check. The positions are
%%% the per-function union det_bound_proviso(F, N, Pos, Kind) filled by
%%% validation; a position consumed by any clause is checked in every clause.
%%% Only direct parameters are provisos: proof terms legitimately carry
%%% unbound variables inside bound data, and is-var-exempt parameters are never
%%% consumed. Specialized copies read the same union.
%The positions are collected first (ground, so findall's copy is harmless) and
%the goals built outside findall, so each guard shares the real head variable.
det_boundness_checks(F, Args, Checks) :-
    ( length(Args, N), boundary_commitment(F, N, Det)
      -> findall(Pos, det_bound_proviso(F, N, Pos, _), Ps0),
         sort(Ps0, Ps),
         det_pos_checks(Ps, F, N, Det, Args, Checks)
       ; Checks = [] ).

det_pos_checks([], _, _, _, _, []).
det_pos_checks([Pos|Ps], F, N, Det, Args, Checks) :-
    nth1(Pos, Args, A),
    ( var(A) -> det_param_check(F, N, Pos, Det, A, Chk), Checks = [Chk|Rest]
              ; Checks = Rest ),
    det_pos_checks(Ps, F, N, Det, Args, Rest).

det_param_check(F, N, Pos, Det, A, Check) :-
    ( det_bound_proviso(F, N, Pos, proper_list)
      -> Check = ( is_list(A) -> true
                 ; throw(error(det_argument_not_proper_list(F, Det), determinism)) )
    ; Check = ( nonvar(A) -> true
              ; throw(error(unbound_det_argument(F, Det), determinism)) ) ).

clause_commit_cut(F, Args) :- \+ suppress_det_cut(true),
                              length(Args, N),
                              \+ function_has_conditional_commit(F, N),
                              catch(( fn_determinism(F, N, D), committed_det(D) ), _, fail).

function_has_conditional_commit(F, N) :-
    catch(nb_getval(F, Metas), _, fail),
    member(Meta, Metas),
    Meta = fun_meta(Args, Body, _),
    length(Args, N),
    body_conditionally_commits(Body), !.

%%% Recompile one graph node. Clauses are redone together and in source order,
%%% since overlap validation compares each clause with its predecessors.
%%% Translation and validation are staged while the old clauses stay live, and
%%% only a complete stage swaps them, so an exception cannot leave the function
%%% half-recompiled.
recompile_function_clauses(F) :-
    function_source_clauses(F, Us),
    ( Us == [] -> true
    ; snapshot_recompile_state(F, Snapshot),
      prepare_recompile_stage(F),
      ( catch(stage_function_clauses(F, Us, Staged), Error,
              ( restore_recompile_state(F, Snapshot), throw(Error) ))
        -> swap_staged_function(F, Us, Staged)
      ; restore_recompile_state(F, Snapshot),
        fail ) ).

snapshot_recompile_state(F, recompile_state(Metas, Provisos, Inferred)) :-
    ( catch(nb_getval(F, Metas0), _, fail) -> Metas = Metas0 ; Metas = '$absent' ),
    findall(proviso(N, Pos, Kind),
            det_bound_proviso(F, N, Pos, Kind),
            Provisos),
    findall(inferred(ATs, OT), inferred_fn_type(F, ATs, OT), Inferred).

prepare_recompile_stage(F) :-
    retractall(det_bound_proviso(F, _, _, _)),
    retractall(inferred_fn_type(F, _, _)),
    nb_setval(F, []).

restore_recompile_state(F, recompile_state(Metas, Provisos, Inferred)) :-
    ( Metas == '$absent' -> catch(nb_delete(F), _, true)
    ; nb_setval(F, Metas) ),
    retractall(det_bound_proviso(F, _, _, _)),
    forall(member(proviso(N, Pos, Kind), Provisos),
           assertz(det_bound_proviso(F, N, Pos, Kind))),
    retractall(inferred_fn_type(F, _, _)),
    forall(member(inferred(ATs, OT), Inferred),
           assertz(inferred_fn_type(F, ATs, OT))).

stage_function_clauses(_, [], []).
stage_function_clauses(F, [Ref-Term|Us],
                       [staged(Ref, Term, OriginFile, Clause, Dependencies)|Ss]) :-
    clause(_, _, Ref),
    compiled_dependency_origin(Ref, OriginFile),
    ( ho_specialization(_, F) -> ConstrainArgs = false ; ConstrainArgs = true ),
    translate_clause(Term, Clause, ConstrainArgs, Dependencies),
    stage_function_clauses(F, Us, Ss).

swap_staged_function(F, Us, Staged) :-
    forall(member(Ref-_, Us),
           ( clause(_, _, Ref) -> erase(Ref) ; true )),
    forall(member(Ref-_, Us),
           ( retractall(translated_from(Ref, _)),
             forget_compiled_dependencies(Ref) )),
    assert_staged_clauses(Staged),
    invalidate_specializations(F).

assert_staged_clauses([]).
assert_staged_clauses([staged(_, Term, OriginFile, Clause, Dependencies)|Ss]) :-
    assertz(Clause, NewRef),
    assertz(translated_from(NewRef, Term)),
    Term = [=, [G|SourceArgs], _],
    length(SourceArgs, N),
    record_compiled_dependencies(NewRef, G/N, OriginFile, Dependencies),
    assert_staged_clauses(Ss).

%Every compiled clause of F, in the order it was asserted (which is source
%order - process_form/3 records translated_from/2 as it goes):
function_source_clauses(F, Us) :- findall(Ref-Term,
                                          ( translated_from(Ref, Term), nonvar(Term),
                                            Term = [=, Head, _], nonvar(Head), Head = [F0|_], F0 == F,
                                            clause(_, _, Ref) ),
                                          Us).

%Record atoms compiled as plain symbol heads, so late function registrations can be flagged:
:- dynamic symbol_head/1.
note_symbol_head(HV) :- atom(HV), \+ symbol_head(HV), !, assertz(symbol_head(HV)).
note_symbol_head(_).

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

resolve_memoization(Fun, Args, Out, Goal) :-
    ( metta_memoized_dispatch_call(Fun, Args, Out, Goal)
    -> true
    ; append(Args, [Out], DirectArgs),
      Goal =.. [Fun|DirectArgs]
    ).
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
                              -> resolve_memoization(F, Args, Out, Goal),
                                 catch(call(Goal), _, fail)
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
translate_expr_to_conj(Input, Conj, Out) :-
        translate_expr(Input, none, Goals, Out),
        goals_list_to_conj(Goals, Conj).
translate_expr_to_conj(Input, Expectation, Conj, Out) :-
        translate_expr(Input, Expectation, Goals, Out),
        goals_list_to_conj(Goals, Conj).

%%% Narrow bidirectional typing for declared product/list results. At a
%%% declared function boundary the expected result type is known, so positional
%%% tuples and runtime lists are checked element by element while constructed.
%%% The expectation follows result positions through if/case, match and
%%% let/let*, and no farther. Tagged structural types keep the ordinary
%%% bottom-up constructor check.
%A function declared to return Atom returns its body as source data:
translate_declared_body(F, out('Atom', _), Expr, [], Expr) :-
        declared_output_type(F, 'Atom'), !.
translate_declared_body(_, out(OT, _), Expr, Goals, Out) :-
        contextual_expected_type(OT), !,
        translate_expr(Expr, expected(OT), Goals, Out).
translate_declared_body(_, _, Expr, Goals, Out) :-
        translate_expr(Expr, none, Goals, Out).

contextual_product_type(T) :- nonvar(T), is_list(T),
                              \+ special_compound_type(T),
                              \+ tagged_tuple_type(T, _, _).

%The exact constructed shapes eligible to receive a top-down expectation.
%Keeping this separate preserves contextual_product_type/1's tuple meaning.
contextual_expected_type(T) :- ( contextual_product_type(T)
                               ; nonvar(T), list_type(T, _) ).

%A brand is erased only after its payload has been checked: a representation
%with a contextual shape feeds the same expectation spine.
brand_payload_expectation(T, expected(Rep)) :-
    atom(T),
    declared_newtype(T, Rep),
    contextual_expected_type(Rep), !.
brand_payload_expectation(_, none).

explicit_data_expectation(expected(Expected), Fields, expected(Expected)) :-
        contextual_product_type(Expected),
        same_length(Fields, Expected), !.
explicit_data_expectation(_, _, none).

explicit_list_expectation(expected(Expected), expected(ET)) :-
        list_type(Expected, ET), !.
explicit_list_expectation(_, none).

%`data` is an erased, non-callable expression constructor. With an expected
%positional shape every field is a declared obligation; without one, known
%field types still give a positional type.
translate_explicit_data(Fields, expected(FieldTs), Goals, Out) :- !,
        translate_expected_fields(Fields, FieldTs, Gs, Values),
        set_out_type(Out, FieldTs),
        append(Gs, [Out = Values], Goals).
translate_explicit_data(Fields, none, Goals, Out) :-
        translate_args(Fields, Gs, Values),
        ( maplist(value_single_type, Values, FieldTs)
          -> set_out_type(Out, FieldTs) ; true ),
        append(Gs, [Out = Values], Goals).

%`make-list` is the erased, explicitly non-callable runtime-list constructor.
%An expected element type controls each element; Expression keeps its raw
%source term, while other nested products/lists recurse through expectations.
translate_explicit_list(Elements, expected(ET), Goals, Out) :- !,
        translate_expected_list_elements(Elements, ET, Gs, Values),
        set_out_type(Out, ['List', ET]),
        append(Gs, [Out = Values], Goals).
translate_explicit_list(Elements, none, Goals, Out) :-
        translate_args(Elements, Gs, Values),
        ( maplist(value_single_type, Values, [T|Ts]),
          forall(member(T2, Ts), T2 =@= T)
          -> set_out_type(Out, ['List', T]) ; true ),
        append(Gs, [Out = Values], Goals).

translate_expected_list_elements([], _, [], []).
translate_expected_list_elements([E|Es], ET, Goals, [V|Vs]) :-
        ( expression_typed(ET)
          -> expression_arg_value(E, V), G1 = []
        ; translate_expected_element(E, ET, 'make-list', G1, V) ),
        translate_expected_list_elements(Es, ET, G2, Vs),
        append(G1, G2, Goals).

translate_expected_element(E, T, Context, Goals, V) :-
        ( contextual_expected_type(T)
          -> translate_expr(E, expected(T), G0, V)
           ; translate_expr(E, none, G0, V) ),
        check_call_arg(declared, Context, V, T, Checks),
        append(G0, Checks, Goals).

translate_expected_fields([], [], [], []).
translate_expected_fields([E|Es], [T|Ts], Goals, [V|Vs]) :-
        translate_expected_element(E, T, data, G1, V),
        translate_expected_fields(Es, Ts, G2, Vs),
        append(G1, G2, Goals).

%Special stream operation rewrite rules before main translation
rewrite_streamops(['trace!', Arg1, Arg2],
                  [progn, ['println!', Arg1], Arg2]).
rewrite_streamops([unique, Arg],
                  [call, [superpose, ['unique-atom', [collapse, Arg]]]]).
rewrite_streamops(['alpha-unique', Arg],
                  [call, [superpose, ['alpha-unique-atom', [collapse, Arg]]]]).
%For a variable of known union or positional-product type, unification against
%inert case structure is a committed two-branch pattern match, so lower it to
%case and reuse its constructor/field and fallthrough-union narrowing. Other
%shapes, fun-headed or compiler-form subterms, and identity-only == keep the
%eager equality path.
rewrite_streamops([If, [Eq, V, Pattern], Then, Else],
                  [case, V, [[Pattern, Then], [Fallthrough, NarrowElse]]]) :-
    If == if,
    Eq == (=),
    var(V),
    nonvar(Pattern),
    known_singleton(V, KnownT),
    equality_case_shape(KnownT, Pattern),
    literal_case_unification_pattern(Pattern), !,
    substitute_source_var(V, Fallthrough, Else, NarrowElse).
%Only the one-argument standard-library resolver is curated. Two-argument
%library paths can come from git-import!, and ordinary file imports remain
%user-origin.
rewrite_streamops(['import!', Space, [library, Name]],
                  ['library-import!', Space, Name]).
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

equality_case_shape(T, _) :-
    is_union(T), !.
equality_case_shape(T, Pattern) :-
    contextual_product_type(T),
    is_list(Pattern),
    same_length(T, Pattern).

%Guarded stream ops rewrite rule application, successfully avoiding copy_term:
safe_rewrite_streamops(In, Out) :- ( compound(In), In = [Op|_], atom(Op) -> rewrite_streamops(In, Out)
                                                                          ; Out = In).

literal_case_unification_pattern(Pattern) :-
    ( var(Pattern) ; atomic(Pattern) ), !.
literal_case_unification_pattern([C, H, T]) :-
    C == cons, !,
    literal_case_unification_pattern(H),
    literal_case_unification_pattern(T).
literal_case_unification_pattern(Pattern) :-
    Pattern = [H|T],
    \+ ( atom(H), special_builtin_form(H, T, _) ),
    \+ functional_pattern_application(Pattern, _, _),
    maplist(literal_case_unification_pattern, Pattern).

%Identity-preserving substitution for the equality-if fallthrough. The fresh
%case binder denotes the same runtime value as V, but unlike V it can carry
%the case branch's reduced union without retaining V's whole-union attribute.
substitute_source_var(Old, New, Term, Out) :-
    ( var(Term)
      -> ( Term == Old -> Out = New ; Out = Term )
    ; atomic(Term)
      -> Out = Term
    ; compound_name_arguments(Term, F, Args),
      maplist(substitute_source_var(Old, New), Args, OutArgs),
      compound_name_arguments(Out, F, OutArgs) ).

%Only literal, declared source-space names opt in. Raw space payloads and
%patterns are never evaluated here: reject a definite contradiction, trust
%unknown runtime-filled fields, and let the existing binder narrow unions.
note_source_space_consultation(Space) :-
    atom(Space),
    analysis_emit(dependency(declaration(space, Space))).

check_typed_space_value(Space, Value) :-
    ignore(note_source_space_consultation(Space)),
    typed_space_runtime_value_ok(Space, Value).

%A row the compiler proves against the declared schema needs no runtime
%re-check, which would walk the whole row on every update. The proof is
%arg_statically_ok/2 inside a declared function only: in an undeclared one the
%parameter types are inference assumptions, which bind no caller, and a field
%typed by a variable the declaration lets the caller pick proves nothing.
typed_space_update_goal(HV, Space, Value, Update) :-
    ( atom(Space), declared_space_type(Space, RowT),
      \+ assumed_self_decl(_, _, _, _),
      term_variables(Value, Vs),
      \+ ( member(V, Vs), known_candidates(V, Cs), member(C, Cs), indefinite_candidate(C) ),
      arg_statically_ok(Value, RowT)
      -> proven_space_update(HV, Update)
    ; Update = HV ).

proven_space_update('add-atom', 'add-atom-proven').
proven_space_update('remove-atom', 'remove-atom-proven').

bind_typed_space_pattern(Space, Pattern) :-
    ignore(note_source_space_consultation(Space)),
    ( atom(Space), declared_space_type(Space, RowT)
      -> ( typed_space_pattern_mismatch(Pattern, RowT)
           -> throw(error(literal_type_mismatch(Pattern, RowT), typecheck))
         ; bind_pattern_typed(Pattern, RowT, []) )
    ; true ).

%Functional elaboration belongs to schema-aware space patterns; untyped spaces
%(&self) are also used to inspect source atoms such as (= (f ...) ...).
elaborate_typed_space_pattern(Space, Pattern, RuntimePattern, Goals) :-
    atom(Space), declared_space_type(Space, _), !,
    constrain_typed_space_args(Pattern, RuntimePattern, Goals).
elaborate_typed_space_pattern(_, Pattern, Pattern, []).

%A schema-backed functional pattern is an exact-one inverse constraint.
%Registered nondet functions can also appear as literal payload tags, and
%executing them would turn inspection into enumeration, so only a unique
%declared det arrow elaborates.
constrain_typed_space_args(X, X, []) :- (var(X); atomic(X)), !.
constrain_typed_space_args([F, A, B], Out, Goals) :-
    nonvar(F), F == cons, !,
    constrain_typed_space_args(A, A1, G1),
    constrain_typed_space_args(B, B1, G2),
    Out = [A1|B1],
    append(G1, G2, Goals).
constrain_typed_space_args(Pattern, Var, Goals) :-
    functional_pattern_application(Pattern, F, Args),
    length(Args, N),
    findall(Det,
            ( declared_fn_type(F, ArgTypes, _, Det),
              length(ArgTypes, N) ),
            [det]), !,
    translate_expr([F|Args], GoalsExpr, Var),
    flatten(GoalsExpr, Goals).
constrain_typed_space_args(In, Out, Goals) :-
    maplist(constrain_typed_space_args, In, Out, NestedGoalsList),
    flatten(NestedGoalsList, Goals), !.

%Pattern-only wrappers are not literal row structure. Strip them (recursively,
%so annotated fields remain unknown) before asking the ordinary value checker
%whether the remaining tags, arities, and literals make a match impossible.
typed_space_pattern_mismatch(Pattern, RowT) :-
    functional_pattern_application(Pattern, _, _), !,
    ( functional_pattern_signature(Pattern, RowT, PatternArgs, ArgTypes)
      -> typed_space_function_args_mismatch(PatternArgs, ArgTypes)
    ; fail ).
typed_space_pattern_mismatch(Pattern, RowT) :-
    pattern_value_shape(Pattern, Shape),
    value_definitely_mismatch(Shape, RowT).

typed_space_function_args_mismatch([Arg|Args], [Type|Types]) :-
    ( typed_space_pattern_mismatch(Arg, Type)
    ; typed_space_function_args_mismatch(Args, Types) ).

pattern_value_shape(P, P) :- var(P), !.
pattern_value_shape([C, V, _Ty], V) :- C == (:), !.
pattern_value_shape([C, H, T], [C, HS, TS]) :- C == cons, !,
                                               pattern_value_shape(H, HS),
                                               pattern_value_shape(T, TS).
%A function application does not survive as literal runtime pattern
%structure: constrain_args/3 replaces the whole position by its output
%variable.  Prevalidation therefore treats that position as unknown.
pattern_value_shape(P, _) :- functional_pattern_application(P, _, _), !.
pattern_value_shape(P, Shape) :- is_list(P), !, maplist(pattern_value_shape, P, Shape).
pattern_value_shape(P, P).

%Turn a MeTTa S-expression into goals. The four-argument traversal is the one
%syntax walk; only the result positions documented above pass expected(T) on.
translate_expr(Expr, Goals, Out) :-
        translate_expr(Expr, none, Goals, Out).

%An empty runtime list is the one atomic value whose contextual list type must
%be retained.
translate_expr(X, expected(Expected), [], X) :-
        nonvar(X), X == [],
        list_type(Expected, _), !,
        set_out_type(X, Expected).
translate_expr(X, _, [], X) :-
        ((var(X) ; atomic(X)) ; X = partial(_,_)), !.

%A compound with a source-variable head is data, so an expectation never sends
%it into special-form dispatch:
translate_expr(Expr, expected(_), Goals, Out) :-
        nonvar(Expr), Expr = [H|_], \+ atom(H), !,
        translate_expr(Expr, none, Goals, Out).
translate_expr([H0|T0], Expectation, Goals, Out) :-
        safe_rewrite_streamops([H0|T0],[H|T]),
        translate_expr(H, none, GsH, HV),
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
        %--- Unambiguous expression-data construction. `data` is erased: its
        %arguments become the fields of the resulting expression, and its
        %first field is never interpreted as a function/closure to call.
        ; special_builtin_form(HV, T, explicit_data)
          -> explicit_data_expectation(Expectation, T, DataExpectation),
             translate_explicit_data(T, DataExpectation, GsD, Out),
                        append(GsH, GsD, Goals)
        %--- Explicit runtime-list construction. Like `data`, `make-list`
        %erases and never dispatches its first element as a callable head.
        ; special_builtin_form(HV, T, explicit_list)
          -> explicit_list_expectation(Expectation, ListExpectation),
             translate_explicit_list(T, ListExpectation, GsL, Out),
                               append(GsH, GsL, Goals)
        %The explicit cons constructors receive list expectations at their
        %element and tail positions.  All other calls remain bottom-up.
        ; Expectation = expected(Expected),
          list_type(Expected, ET),
          ( HV == cons ; HV == 'cons-atom' ),
          T = [HeadExpr, TailExpr]
          -> translate_expected_element(HeadExpr, ET, HV, GsHead, Head),
             translate_expr(TailExpr, expected(Expected), GsTail, Tail),
             check_call_arg(declared, HV, Tail, Expected, TailChecks),
             append([GsH, GsHead, GsTail, TailChecks], Inner),
             build_direct_call(HV, [Head, Tail], Out, Inner, [], Goals),
             set_out_type(Out, Expected)
        %--- Non-determinism ---:
        ; special_builtin_form(HV, T, superpose_literal),
          T = [Args], is_list(Args) -> build_superpose_branches(Args, Out, Branches),
                                                        disj_list(Branches, Disj),
                                                        append(GsH, [Disj], Goals)
        ; special_builtin_form(HV, T, collapse_all), T = [E]
          -> translate_expr_to_conj(E, Conj, EV),
                                     %always a list: one element type is carried, several
                                     %become a union, and an uncertain element type is
                                     %recorded as unknown, since an open ET would unify with
                                     %any required element type:
                                     collapse_elem_type(EV, ET, ElemKnown),
                                     set_out_type(Out, ['List', ET]),
                                     ( ElemKnown == true -> true ; note_unknown_candidate(Out) ),
                                     append(GsH, [findall(EV, Conj, Out)], Goals)
        ; special_builtin_form(HV, T, cut), T = []
          -> append(GsH, [(!)], Goals),
                               Out = true
        ; special_builtin_form(HV, T, test_collect), T = [Expr, Expected]
          -> translate_expr_to_conj(Expr, Conj, Val),
                                              translate_expr(Expected, GsE, ExpVal),
                                              Goal1 = ( findall(Val, Conj, Results),
                                                        (Results = [Actual] -> true
                                                                             ; Actual = Results ) ),
                                              append(GsH, [Goal1], G1),
                                              append(G1, GsE, G2),
                                              append(G2, [test(Actual, ExpVal, Out)], Goals)
        ; special_builtin_form(HV, T, once), T = [X]
          -> translate_expr_to_conj(X, Conj, Out),
                                 append(GsH, [once(Conj)], Goals)
        ; special_builtin_form(HV, T, hyperpose), T = [L]
          -> ( nonvar(L), is_list(L)
               -> build_hyperpose_branches(L, Branches),
                  maplist({Out}/[(_,Res)]>>note_candidates(Out, Res), Branches),
                  append(GsH, [concurrent_and(member((Goal,Res), Branches), (call(Goal), Out = Res))], Goals)
               ; translate_expr(L, GsL, LV),
                 append(GsH, GsL, Inner),
                 append(Inner, [hyperpose_runtime(LV, Out)], Goals) )
        ; special_builtin_form(HV, T, with_mutex), T = [M,X]
          -> translate_expr_to_conj(X, Conj, Out),
                                         append(GsH, [with_mutex(M,Conj)], Goals)
        ; special_builtin_form(HV, T, transaction), T = [X]
          -> translate_expr_to_conj(X, Conj, Out),
                                        append(GsH, [transaction(Conj)], Goals)
        %--- Sequential execution ---:
        ; special_builtin_form(HV, T, progn), T = Exprs
          -> translate_args(Exprs, GsList, Outs),
                                    append(GsH, GsList, Tmp),
                                    last(Outs, Out),
                                    Goals = Tmp
        ; special_builtin_form(HV, T, prog1), T = Exprs
          -> Exprs = [First|Rest],
                                    translate_expr(First, GsF, Out),
                                    translate_args(Rest, GsRest, _),
                                    append(GsH, GsF, Tmp1),
                                    append(Tmp1, GsRest, Goals)
        %--- Conditionals ---:
        ; special_builtin_form(HV, T, if_then), T = [Cond, Then]
          -> translate_if_cond(Cond, ConC, CondGoal),
                                        with_unified_edge_facts(
                                            Cond, true,
                                            ( translate_expr_to_conj(
                                                  Then, Expectation, ConT, Tv),
                                              build_isolated_branch(
                                                  ConT, Tv, Out, BT) )),
                                        ( ConC == true -> append(GsH, [ ( CondGoal -> BT ) ], Goals)
                                                        ; append(GsH, [ ( ConC, ( CondGoal -> BT ) ) ], Goals) )
        ; special_builtin_form(HV, T, if_then_else), T = [Cond, Then, Else]
          -> translate_if_cond(Cond, ConC, CondGoal),
                                              with_unified_edge_facts(
                                                  Cond, true,
                                                  ( translate_expr_to_conj(
                                                        Then, Expectation, ConT, Tv),
                                                    build_isolated_branch(
                                                        ConT, Tv, Out, BT) )),
                                              with_unified_edge_facts(
                                                  Cond, false,
                                                  ( translate_expr_to_conj(
                                                        Else, Expectation, ConE, Ev),
                                                    build_isolated_branch(
                                                        ConE, Ev, Out, BE) )),
                                              ( ConC == true -> append(GsH, [ (CondGoal -> BT ; BE) ], Goals)
                                                              ; append(GsH, [ (ConC, (CondGoal -> BT ; BE)) ], Goals) )
        %A case whose arguments do not fit (case Key ((Pattern Value) ...))
        %compiles as a data list - warn, because that is rarely intended and
        %the downstream errors do not mention case at all:
        ; builtin_codegen_symbol(HV, case),
          \+ ( T = [_, Ps], is_list(Ps), forall(member(Pr, Ps), (is_list(Pr), length(Pr, 2))) )
          -> format(user_error, "Warning: (case ...) does not match (case Key ((Pattern Value) ...)) and compiles as plain data~n", []),
             eval_data_list([HV|T], GsD, Out),
             append(GsH, GsD, Goals)
        ; special_builtin_form(HV, T, case), T = [KeyExpr, PairsExpr]
          -> ( select(Found0, PairsExpr, Rest0),
                                                    subsumes_term(['Empty', _], Found0),
                                                    Found0 = ['Empty', DefaultExpr],
                                                    NormalCases = Rest0
                                                    -> translate_expr_to_conj(KeyExpr, GkConj, Kv),
                                                       translate_case(NormalCases, Kv, Expectation, Out, CaseGoal, KeyGoal),
                                                       translate_expr_to_conj(DefaultExpr, Expectation, ConD, DOut),
                                                       build_branch(ConD, DOut, Out, DefaultThen),
                                                       Combined = ( (GkConj, CaseGoal) ;
                                                                    \+ GkConj, DefaultThen ),
                                                       append([GsH, KeyGoal, [Combined]], Goals)
                                                     ; translate_expr(KeyExpr, Gk, Kv),
                                                       translate_case(PairsExpr, Kv, Expectation, Out, IfGoal, KeyGoal),
                                                       append([GsH, Gk, KeyGoal, [IfGoal]], Goals) )
        %--- Short-circuit boolean operators ---:
        ; special_builtin_form(HV, T, and_then), T = [A, B]
          -> translate_expr_to_conj(A, ConjA, Av),
                                           with_unified_edge_facts(
                                               A, true,
                                               ( translate_expr_to_conj(B, ConjB, Bv),
                                                 check_call_arg(
                                                     declared, 'and-then', Bv,
                                                     'Bool', GsB),
                                                 goals_list_to_conj(GsB, GB) )),
                                           check_call_arg(declared, 'and-then', Av, 'Bool', GsA),
                                           goals_list_to_conj(GsA, GA),
                                           set_out_type(Out, 'Bool'),
                                           append(GsH, [(ConjA, GA, (Av == true -> (ConjB, GB, Out = Bv) ; Out = false))], Goals)
        ; special_builtin_form(HV, T, or_else), T = [A, B]
          -> translate_expr_to_conj(A, ConjA, Av),
                                          with_unified_edge_facts(
                                              A, false,
                                              ( translate_expr_to_conj(B, ConjB, Bv),
                                                check_call_arg(
                                                    declared, 'or-else', Bv,
                                                    'Bool', GsB),
                                                goals_list_to_conj(GsB, GB) )),
                                          check_call_arg(declared, 'or-else', Av, 'Bool', GsA),
                                          goals_list_to_conj(GsA, GA),
                                          set_out_type(Out, 'Bool'),
                                          append(GsH, [(ConjA, GA, (Av == true -> Out = true ; (ConjB, GB, Out = Bv)))], Goals)
        %--- Unification constructs ---:
        ; special_builtin_form(HV, T, let_bind), T = [Pat, Val, In]
          -> translate_expr(Pat, Gp, Pv),
                                                           translate_let_value(Pat, Val, In, Gv, V),
                                                           note_candidates(Pv, V),        %the bound variable gets the value's type
                                                           bind_pattern_from(Pat, V),     %destructuring patterns type their fields
                                                           translate_expr(In, Expectation, Gi, Out),
                                                           append([GsH,[(Pv=V)],Gp,Gv,Gi], Goals)
        ; special_builtin_form(HV, T, let_star), T = [Binds, Body]
          -> letstar_to_rec_let(Binds,Body,RecLet),
                                             translate_expr(RecLet, Expectation, Goals, Out)
        ; special_builtin_form(HV, T, sealed), T = [Vars, Expr]
          -> translate_expr_to_conj(Expr, Con, Val),
                                            note_candidates(Out, Val),
                                            Goals = [copy_term(Vars,[Con,Val],_,[Ncon,Out]),Ncon]
        %--- Iterating over non-deterministic generators without reification ---:
        ; special_builtin_form(HV, T, forall), T = [GF, TF]
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
        ; special_builtin_form(HV, T, foldall), T = [AF, GF, InitS]
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
        ; special_builtin_form(HV, T, foldl_pseudo_lambda),
          T = [List, Init, AccVar, XVar, Body]
          -> translate_expr_to_conj(List, ConjList, L),
             translate_expr_to_conj(Init, ConjInit, InitV),
             note_list_elem_type(XVar, L),
             translate_expr_to_conj(Body, BodyConj, BG),
             exclude(==(true), [ConjList, ConjInit], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [foldl([XVar, AccVar, NewAcc]>>(BodyConj, ( number(BG) -> NewAcc is BG ; NewAcc = BG )), L, InitV, Out)], Goals)
        ; special_builtin_form(HV, T, map_pseudo_lambda),
          T = [List, XVar, Body]
          -> translate_expr_to_conj(List, ConjList, L),
             note_list_elem_type(XVar, L),
             translate_expr_to_conj(Body, BodyCallConj, BodyCall),
             ( value_single_type(BodyCall, BT) -> set_out_type(Out, ['List', BT]) ; true ),
             exclude(==(true), [ConjList], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [maplist([XVar, Y]>>(BodyCallConj, ( number(BodyCall) -> Y is BodyCall ; Y = BodyCall )), L, Out)], Goals)
        ; special_builtin_form(HV, T, filter_pseudo_lambda),
          T = [List, XVar, Cond]
          -> translate_expr_to_conj(List, ConjList, L),
             note_list_elem_type(XVar, L),
             translate_expr_to_conj(Cond, CondConj, CondGoal),
             exclude(==(true), [ConjList], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [include([XVar]>>(CondConj, CondGoal), L, Out)], Goals)
        ; special_builtin_form(HV, T, lambda), T = [Args, Body]
          -> next_lambda_name(F),
                                           % find free (non-argument) variables in Body
                                           term_variables(Body, AllVars),
                                           term_variables(Args, ArgVars),
                                           exclude({ArgVars}/[V]>>memberchk_eq(V, ArgVars), AllVars, FreeVars),
                                           append(FreeVars, Args, FullArgs),
                                           % compile clause with all bound + free vars
                                           LambdaSource = [=, [F|FullArgs], Body],
                                           % A lambda body is deferred code: compile an attributed copy, so
                                           % facts produced inside it do not reach the enclosing expression.
                                           copy_term(LambdaSource, IsolatedLambdaSource),
                                           with_unified_ad_hoc_clause_analysis(
                                               IsolatedLambdaSource,
                                               translate_clause(
                                                   IsolatedLambdaSource, Clause,
                                                   true, LambdaDependencies)),
                                           register_fun(F),
                                           assertz(Clause, LambdaRef),
                                           assertz(translated_from(LambdaRef, IsolatedLambdaSource)),
                                           length(FullArgs, N),
                                           record_compiled_dependencies(LambdaRef, F/N, LambdaDependencies),
                                           format(atom(Label), "metta lambda (~w)", [F]),
                                           maybe_print_compiled_clause(Label, ['|->', Args, Body], Clause),
                                           Arity is N + 1,
                                           (arity(F, Arity) -> true ; assertz(arity(F, Arity))),
                                           % emit closure capturing the environment (free vars)
                                           ( FreeVars == [] -> Out = F
                                                             ; Out = partial(F, FreeVars) )
        %--- Spaces ---:
        ; special_builtin_form(HV, T, typed_space_update), T = [Space, Atom] ->
                                                                   check_typed_space_value(Space, Atom),
                                                                   translate_expr(Space, G1, S),
                                                                   typed_space_update_goal(HV, Space, Atom, Update),
                                                                   Goal =.. [Update,S,Atom,Out],
                                                                   set_out_type(Out, 'Bool'),
                                                                   append([GsH,G1,[Goal]], Goals)
        ; special_builtin_form(HV, T, typed_space_match),
          T = [Space, Pattern, Body] -> translate_expr(Space, G1, S),
                                                     type_match_pattern(Pattern),
                                                     bind_typed_space_pattern(Space, Pattern),
                                                     elaborate_typed_space_pattern(
                                                         Space, Pattern,
                                                         RuntimePattern,
                                                         PatternGoals),
                                                     translate_expr(Body, Expectation, GsB, Out),
                                                     %Elaborated outputs are bound
                                                     %first, then functional
                                                     %constraints, then the body.
                                                     append([GsH, G1,
                                                             [match(S, RuntimePattern,
                                                                    Out, Out)],
                                                             PatternGoals,
                                                             GsB],
                                                            Goals)
        %--- Predicate to compiled goal ---:
        ; special_builtin_form(HV, T, translate_predicate), T = [Expr]
          -> Expr = [S|Args],
                                                  translate_args(Args, GsArgs, ArgsOut),
                                                  Goal =.. [S|ArgsOut],
                                                  append(GsH, GsArgs, Inner),
                                                  append(Inner, [Goal], Goals)
        %--- Manual dispatch options: ---
        %Generate a predicate call on compilation, translating Args for nesting:
        ; special_builtin_form(HV, T, manual_call), T = [Expr]
          -> Expr = [F|Args],
                                     translate_args(Args, GsArgs, ArgsOut),
                                     append(GsH, GsArgs, Inner),
                                     append(ArgsOut, [Out], CallArgs),
                                     Goal =.. [F|CallArgs],
                                     length(Args, NC),
                                     manual_dispatch_arg_checks_status(F, NC, ArgsOut,
                                                                       GuardGs, ArgStatus),
                                     ( ArgStatus == verified
                                       -> set_unique_decl_out(F, NC, Out)
                                     ; true ),
                                     append(Inner, GuardGs, Inner1),
                                     append(Inner1, [Goal], Goals)
        %Produce a dynamic dispatch, translating Args for nesting:
        ; special_builtin_form(HV, T, dynamic_reduce), T = [Expr]
          -> ( var(Expr) -> translate_expr(Expr, GsH, ExprOut),
                                                     Goals = [reduce(ExprOut, Out)|GsH]
                                                   ; Expr = [F|Args],
                                                     translate_args(Args, GsArgs, ArgsOut),
                                                     append(GsH, GsArgs, Inner),
                                                     ExprOut = [F|ArgsOut],
                                                     length(Args, NR),
                                                     manual_dispatch_arg_checks_status(F, NR, ArgsOut,
                                                                                       GuardGs, ArgStatus),
                                                     ( ArgStatus == verified
                                                       -> set_unique_decl_out(F, NR, Out)
                                                     ; true ),
                                                     append(Inner, GuardGs, Inner1),
                                                     append(Inner1, [reduce(ExprOut, Out)], Goals) )
        %Invoke translator to evaluate MeTTa code as data/list:
        ; special_builtin_form(HV, T, eval_source), T = [Arg]
          -> ( nonvar(Arg), Arg = [Q, Quoted], Q == quote
                                     -> translate_expr(Quoted, GsQ, Out),           %(eval (quote E)) == E
                                        append(GsH, GsQ, Goals)
                                      ; append(GsH, [eval(Arg, Out)], Goals) )
        %Erased branding of a semantic role: knowledge only, no runtime goal:
        ; special_builtin_form(HV, T, brand), T = [TypeExpr, Expr]
          -> normalize_type(TypeExpr, TN),
                                               brand_payload_expectation(TN, PayloadExpectation),
                                               translate_expr(Expr, PayloadExpectation, GsE, Out0),
                                               brand_type(Out0, TN),
                                               ( nonvar(Out0)               %a branded literal keeps its brand
                                                 -> add_known_type(Out, TN),
                                                    append([GsH, GsE, [Out = Out0]], Goals)
                                                  ; Out = Out0,
                                                    append(GsH, GsE, Goals) )
        %Explicit type ascription for dynamically typed values:
        ; special_builtin_form(HV, T, type_ascription),
          T = [TypeExpr, Expr] -> translate_expr(Expr, GsE, Out0),
                                             normalize_type(TypeExpr, TN),
                                             ascribe_type(Out0, TN, GsA),
                                             %An ascribed literal keeps the author's type when it is
                                             %more specific than the value's own (e.g. (the (List Item) ())):
                                             ( nonvar(Out0), nonvar(TN), \+ wildcard_type(TN),
                                               \+ ( value_single_type(Out0, VT), VT == TN )
                                               -> add_known_type(Out, TN),
                                                  append([GsH, GsE, GsA, [Out = Out0]], Goals)
                                                ; Out = Out0,
                                                  append([GsH, GsE, GsA], Goals) )
        %Force arg to remain data/list:
        ; special_builtin_form(HV, T, quote), T = [Expr]
          -> append(GsH, [], Inner),
                                     Out = Expr,
                                     Goals = Inner
        ; special_builtin_form(HV, T, catch), T = [Expr] ->
          translate_expr(Expr, GsExpr, ExprOut),
          append(GsH, [], Inner),
          goals_list_to_conj(GsExpr, Conj),
          Goal = catch((Conj, Out = ExprOut),
                       Exception,
                       (Exception = error(Type, Ctx) -> Out = ['Error', Type, Ctx]
                                                      ; Out = ['Error', Exception, none])),
          %The result is the value or an (Error Detail Ctx) expression: with a
          %known inner type that is a union, so union-typed consumers such as
          %(-> (| Number (Error $d $c)) ...) check statically:
          ( value_single_type(ExprOut, VT)
            -> set_out_type(Out, ['|', VT, ['Error', '%Undefined%', '%Undefined%']])
             ; true ),
          append(Inner, [Goal], Goals)
        %--- Automatic 'smart' dispatch, translator deciding when to create a predicate call, data list, or dynamic dispatch: ---
        %Known function or closure => type-directed call:
        ; ( atom(HV), fun(HV) -> Fun = HV, Bound = []
          ; compound(HV), HV = partial(Fun, Bound) )
          -> translate_typed_call(Fun, Bound, T, GsH, Goals, Out)
        %Literals (numbers, strings, etc.), known non-function atom => data:
        ; ( atomic(HV), \+ atom(HV) ; atom(HV), \+ fun(HV) )
          -> note_symbol_head(HV),
             translate_args(T, GsT, AVs),
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
          %A singleton ($x) calls $x when its type is a nullary arrow, is a
          %one-element list when its type is known otherwise (a declared $a
          %included), and is dispatched at runtime when its type is only an
          %inference assumption:
          ( var(HV), AVs == []
            -> ( known_singleton(HV, K), nonvar(K), K = [H, OutT], arrow_atom(H)
                 -> closure_apply_goal(HV, [], Out, Goal),
                    append(Inner, [Goal], Goals),
                    ( var(Out), var(OutT) -> add_known_type(Out, OutT) ; set_out_type(Out, OutT) )
               ; known_singleton(HV, K), ( nonvar(K) ; param_promise_var(K) )
                 -> Out = [HV],
                    Goals = Inner
               ; append(Inner, [reduce([HV], Out)], Goals) )
          ; translate_closure_call(HV, AVs, Inner, Goals, Out) -> true
          ; var(HV), known_singleton(HV, K), nonfunction_type(K)
            -> Out = [HV|AVs],
               Goals = Inner
          ; append(Inner, [reduce([HV|AVs], Out)], Goals) ) ).

%Values of these types can never dispatch as functions in reduce/2:
nonfunction_type(K) :- nonvar(K), ( primitive_type(K)
                                  ; is_list(K), \+ is_arrow_type(K) ).

%A variable head with a known arrow type of matching arity is a closure call:
%check the args against the arrow, dispatch through apply_fn, and propagate
%the output type. Applying a parameter whose type is an unbound assumption
%tells us it is a function.
translate_closure_call(HV, AVs, Inner, Goals, Out) :- var(HV), AVs \== [], known_singleton(HV, K),
                                                      length(AVs, N), N1 is N + 1,
                                                      %An unbound K makes the arrow shape a guess: recorded
                                                      %on an inference assumption, kept local on a promised
                                                      %variable (param_promise_var/1):
                                                      ( var(K) -> length(Xs, N1),
                                                                  ( param_promise_var(K) -> Guessed = true
                                                                                          ; K = [->|Xs], Guessed = false )
                                                      ; K = [H|Xs], arrow_atom(H),
                                                        length(Xs, N1), Guessed = false ),
                                                      append(ArgTs, [OutT], Xs),
                                                      apply_call_args(declared, closure, AVs, ArgTs, GuardGs),
                                                      append(Inner, GuardGs, Inner1),
                                                      closure_apply_goal(HV, AVs, Out, Goal),
                                                      append(Inner1, [Goal], Goals),
                                                      %a variable arrow output is knowledge too: it is the
                                                      %declaration-instance type var shared with the context
                                                      %(e.g. map-flat's element type), not an unknown:
                                                      ( Guessed == true -> note_unknown_candidate(Out)
                                                      ; var(Out), var(OutT) -> add_known_type(Out, OutT)
                                                                             ; set_out_type(Out, OutT) ).
%Underapplying a typed closure parameter builds a partial: the used argument
%positions are checked and the result carries the remaining arrow:
translate_closure_call(HV, AVs, Inner, Goals, Out) :- var(HV), AVs \== [], known_singleton(HV, K),
                                                      nonvar(K), K = [H|Xs],
                                                      arrow_atom(H),
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

%Runtime closure application; the last clause keeps reduce/2 semantics for
%values that are not callable. Errors raised inside the callee propagate:
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
             translate_typed_args(Fun, FullDecls, Args, EffTs, GsT, AVs0),
             append(Bound, AVs0, AVs),
             ( FullDecls = [Single] -> Chosen = Single, MultiDecl = false
             ; MultiDecl = true,
               include(decl_survives(AVs), FullDecls, Survivors),
               ( Survivors == [] -> throw(error(no_matching_overload(Fun), typecheck))
               ; Survivors = [OneLeft] -> Chosen = OneLeft
               ; Chosen = multi(Survivors) ) ),
             ( Chosen = ft(ATs, OT)
               -> apply_call_args_status(declared, Fun, AVs, ATs, GuardGs,
                                         ArgStatus),
                  append([GsH, GsT, GuardGs], Inner),
                  %overloaded functions: clauses were not output-checked against a
                  %single declaration, so the call filters on the output type:
                  overload_out_guard(MultiDecl, Fun, Out, OT, Extra),
                  ( MultiDecl == false, arith_inline(Fun, AVs, Out, ArithGs)
                    -> append(Inner, ArithGs, Goals)
                     ; build_call_or_partial(Fun, AVs, Out, Inner, Extra, Goals) ),
                  ( ArgStatus == verified
                    -> set_call_out_type(Out, ATs, OT)
                  ; true )
                ; Chosen = multi(Survs),
                  maplist(overload_branch(Fun, AVs, Out), Survs, Branches),
                  disj_list(Branches, Disj),
                  append(GsH, GsT, Pre),
                  append(Pre, [goal_or_throw(Disj, error(no_matching_overload(Fun), typecheck))], Goals) )
        ; findall(pt(PTs, RTs, OT), fn_decl_partial(Fun, NTotal, PTs, RTs, OT, _), PartDecls),
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

%At a uniquely declared call site, closure arguments first resolve shared type
%variables in the declaration, so a remaining product/list argument can get
%contextual construction typing. apply_call_args stays the single enforcement
%site, and goal order stays source order.
translate_typed_args(_, [ft(ATs, _)], Args, EffTs, Goals, Values) :-
        length(ATs, NDecl), length(Args, NProv), NB is NDecl - NProv, NB >= 0,
        length(BoundTs, NB), append(BoundTs, DeclTs, ATs),
        closure_contextual_positions(DeclTs), !,
        translate_declared_arrows(Args, EffTs, DeclTs, Staged),
        translate_contextual_args(Staged, GoalLists, Values),
        append(GoalLists, Goals).
translate_typed_args(_, _, Args, EffTs, Goals, Values) :-
        translate_args_by_type(Args, EffTs, Goals, Values).

closure_contextual_positions(Ts) :-
        nth0(ClosureI, Ts, ClosureT), is_arrow_type(ClosureT),
        nth0(ProductI, Ts, ProductT), ProductI =\= ClosureI,
        ( var(ProductT) ; contextual_expected_type(ProductT) ), !.

translate_declared_arrows([], [], [], []).
translate_declared_arrows([A|As], [ET|ETs], [DT|DTs], [S|Ss]) :-
        ( is_arrow_type(DT)
          -> translate_expr(A, G, V),
             ( argument_resolves_declared_type(V, DT) -> true ; true ),
             S = translated(G, V)
           ; S = pending(A, ET, DT) ),
        translate_declared_arrows(As, ETs, DTs, Ss).

argument_resolves_declared_type(V, DeclT) :-
        ( var(V) -> known_singleton(V, Actual)
                 ; value_single_type(V, Actual) ),
        type_unify(Actual, DeclT).

translate_contextual_args([], [], []).
translate_contextual_args([translated(G, V)|Ss], [G|Gs], [V|Vs]) :-
        translate_contextual_args(Ss, Gs, Vs).
translate_contextual_args([pending(A, ET, DT)|Ss], [G|Gs], [V|Vs]) :-
        ( contextual_expected_type(DT), \+ expression_typed(ET)
          -> translate_expr(A, expected(DT), G, V)
        ; expression_typed(ET)
          -> expression_arg_value(A, V), G = []
           ; translate_expr(A, G, V) ),
        translate_contextual_args(Ss, Gs, Vs).

%A contextual producer may be staged in an earlier let* binding, too early for
%its consumer call. Look ahead through the remaining let/let* spine for a
%uniquely declared call using this exact binder and translate the producer
%under the expectation it implies (a fresh declaration copy).
translate_let_value(Pat, Val, In, Goals, V) :-
        nonvar(Val), Val = [Ctor|_],
        ( Ctor == data ; Ctor == 'make-list' ; Ctor == cons ),
        contextual_bound_initializer_type(Pat, In, Expected),
        contextual_expected_type(Expected), !,
        translate_expr(Val, expected(Expected), Goals, V).
translate_let_value(_, Val, _, Goals, V) :- translate_expr(Val, Goals, V).

contextual_bound_initializer_type(Pat, In, Expected) :-
        var(Pat),
        contextual_initializer_use(In, Pat, Expected).

contextual_initializer_use(Expr, Binder, Expected) :-
        nonvar(Expr), Expr = [Kind, _Pat, Val, In],
        (Kind == let ; Kind == chain), !,
        ( contextual_initializer_use(Val, Binder, Expected)
        ; contextual_initializer_use(In, Binder, Expected) ).
contextual_initializer_use(Expr, Binder, Expected) :-
        nonvar(Expr), Expr = [Kind, Binds, Body], Kind == 'let*', !,
        letstar_to_rec_let(Binds, Body, RecLet),
        contextual_initializer_use(RecLet, Binder, Expected).
contextual_initializer_use(Expr, Binder, Expected) :-
        nonvar(Expr), Expr = [F|CallArgs], atom(F), is_list(CallArgs),
        length(CallArgs, N),
        unique_fn_decl(F, N, ATs, _),
        nth0(BinderI, CallArgs, Use), Use == Binder,
        nth0(BinderI, ATs, Expected),
        resolve_source_arrow_args(CallArgs, ATs, BinderI),
        contextual_expected_type(Expected), !.

resolve_source_arrow_args(Args, ATs, BinderI) :-
        resolve_source_arrow_args(Args, ATs, BinderI, 0).
resolve_source_arrow_args([], [], _, _).
resolve_source_arrow_args([A|As], [T|Ts], BinderI, I) :-
        ( I =\= BinderI, is_arrow_type(T)
          -> ( source_callable_arrow(A, Actual), type_unify(Actual, T)
               -> true ; true )
           ; true ),
        I1 is I + 1,
        resolve_source_arrow_args(As, Ts, BinderI, I1).

source_callable_arrow(G, Arrow) :-
        atom(G),
        findall(ft(ArgTs, OutT, H),
                ( declared_fn_type(G, ArgTs, OutT, Det),
                  length(ArgTs, N),
                  value_arrow_head(G, N, Det, H) ),
                [ft(ArgTs, OutT, H)]),
        append(ArgTs, [OutT], Tail),
        Arrow = [H|Tail].
source_callable_arrow(Source, Arrow) :-
        nonvar(Source), Source = [G|Bound], atom(G), is_list(Bound),
        length(Bound, N),
        findall(pt(RTs, OutT, H),
                ( fn_decl_partial(G, N, _PTs, RTs, OutT, Det),
                  length(RTs, NR), Total is N + NR,
                  value_arrow_head(G, Total, Det, H) ),
                [pt(RTs, OutT, H)]),
        append(RTs, [OutT], Tail),
        Arrow = [H|Tail].

%A provided arg position stays untranslated data iff every declaration types it
%Expression; the effective type feeds translate_args_by_type, which only ever
%distinguishes unevaluated Atom positions from everything else:
eff_arg_types(FullDecls, NB, NProv, Ts) :- NEnd is NB + NProv - 1,
                                           ( NProv =:= 0 -> Ts = []
                                           ; numlist(NB, NEnd, Is),
                                             maplist(eff_arg_type(FullDecls), Is, Ts) ).
eff_arg_type(FullDecls, I, T) :- ( forall(member(ft(ATs, _), FullDecls),
                                          ( nth0(I, ATs, Ty), expression_typed(Ty) ))
                                   -> T = 'Atom' ; true ).

%Atom-typed args stay unevaluated data, except underapplied callable
%expressions representable as a goal-free closure. brand is a checker construct
%and erases here too (quote passes a literal (brand ...) form):
expression_arg_value(A, AV) :- nonvar(A), A = [B, TypeExpr, Inner], B == brand, !,
                               expression_arg_value(Inner, AV),
                               normalize_type(TypeExpr, TN),
                               brand_type(AV, TN).
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

overload_out_guard(MultiDecl, Fun, Out, OT, Extra) :- ( MultiDecl == true, ground(OT), \+ wildcard_type(OT)
                                                        -> ( strict_mode(true)
                                                             -> throw(error(strict_runtime_typecheck(Fun, typecheck_match(Out, OT)), typecheck))
                                                            ; trusted_guard_waiver(Fun)
                                                              -> Extra = []
                                                              ; Extra = [typecheck_match(Out, OT)] )
                                                         ; Extra = [] ).

overload_branch_guard(Fun, AV, T, G) :- ( arg_statically_ok(AV, T) -> G = []
                                        ; strict_mode(true)
                                          -> throw(error(strict_runtime_typecheck(Fun, typecheck_match(AV, T)), typecheck))
                                        ; trusted_guard_waiver(Fun)
                                          -> G = []
                                           ; G = [typecheck_match(AV, T)] ).

%Type-resolved builtin arithmetic compiles to native is/2, constant-folded when
%both operands are literals. Only while the builtin definition is untouched:
arith_inline(Fun, [A, B], Out, Gs) :- builtin_codegen_hook(Fun, 2, arithmetic_native),
                                      arith_op(Fun, A, B, Expr),
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
                               builtin_codegen_hook(F, 2, reified_comparison),
                               cmp_native(F, A, B, Native),
                               builtin_untouched(F).

cmp_native('<', A, B, (A < B)).
cmp_native('<=', A, B, (A =< B)).
cmp_native('>', A, B, (A > B)).
cmp_native('>=', A, B, (A >= B)).
cmp_native('==', A, B, (A == B)).
cmp_native('!=', A, B, (A \== B)).

%Generate actual function call or partial if arity not complete:
build_call_or_partial(Fun, AVs, Out, Inner, Extra, Goals) :- ( maybe_specialize_call(Fun, AVs, Out, Goal)
                                                               -> oracle_det_wrap(Fun, AVs, Out, Goal, Goal1),
                                                                  append(Inner, [catch(call(Goal1), _, fail)|Extra], Goals)
                                                                ; build_direct_call(Fun, AVs, Out, Inner, Extra, Goals) ).

build_direct_call(Fun, AVs, Out, Inner, Extra, Goals) :- length(AVs, N),
                                                         Arity is N + 1,
                                                         ( ( current_predicate(Fun/Arity) ; catch(arity(Fun, Arity), _, fail) ),
                                                           \+ ( current_op(_, _, Fun), Arity =< 2 )
                                                           -> resolve_memoization(Fun, AVs, Out, Goal0),
                                                              %--oracle-det: count this call's solutions
                                                              oracle_det_wrap(Fun, AVs, Out, Goal0, Goal),
                                                              append(Inner, [Goal|Extra], Goals)
                                                         ; incomplete_application_kind(Fun, Arity, partial)
                                                           -> Out = partial(Fun, AVs),
                                                              append(Inner, Extra, Goals)
                                                            ; append(Inner, [throw_function_overapplication(Fun, N)|Extra], Goals) ).

%Selectively translate non-Atom args while Atom args stay as data input:
translate_args_by_type([], _, [], []) :- !.
translate_args_by_type([A|As], [T|Ts], GsOut, [AV|AVs]) :-
                      ( expression_typed(T) -> expression_arg_value(A, AV), GsA = []
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

% Constructs the goal for one branch of an if-then-else/case. An untyped
% branch value aliased onto the shared Out would inherit an earlier branch's
% candidates, so its ignorance is recorded first (note_unknown_candidate/1):
build_branch(true, Val, Out, (Out = Val)) :- !, note_candidates(Out, Val).
build_branch(Con, Val, Out, Goal) :- var(Val) -> ( known_candidates(Val, _) -> true
                                                                             ; Unknown = yes ),
                                                 Val = Out,
                                                 ( Unknown == yes -> note_unknown_candidate(Out)
                                                                   ; true ),
                                                 Goal = Con
                                               ; note_candidates(Out, Val),
                                                 Goal = (Val = Out, Con).

% Edge-scoped translation restores every source variable after an arm, so a
% variable-valued arm is not aliased onto the result at compile time (restoring
% it would erase the joined type): transfer its candidates and emit the
% equality instead.
build_isolated_branch(Con, Val, Out, Goal) :-
    ( var(Val), current_unified_source_variable(Val)
      -> join_isolated_branch_candidates(Out, Val),
         ( Con == true -> Goal = (Out = Val)
         ; Goal = (Con, Out = Val) )
    ; build_branch(Con, Val, Out, Goal) ).

% An if join is a choice flow, not a binding flow (note_candidates/2 would
% specialize Out's open candidate): route the candidates through a fresh
% carrier so tknown's unification hook takes their variant union.
join_isolated_branch_candidates(Out, Val) :-
    ( var(Out)
      -> ( var(Val), known_candidates(Val, Candidates)
           -> put_attr(Carrier, tknown, Candidates)
         ; note_candidates(Carrier, Val) ),
         Carrier = Out
    ; true ).

%Translate case into a nested if-then-else, so case is first-match: Prior
%carries the earlier branches' patterns for union narrowing (narrowing_sound/4).
translate_case(Pairs, Kv, Expectation, Out, Goal, KGo) :-
        translate_case(Pairs, Kv, Expectation, Out, Goal, KGo, []).

translate_case([[K,VExpr]|Rs], Kv, Expectation, Out, Goal, KGo, Prior) :-
                                                      ( case_scrutinee_value_type(Kv, KT)
                                                        -> bind_pattern_typed(K, KT, Prior)
                                                         ; ctor_pattern_field_types(K) ),
                                                      translate_expr_to_conj(VExpr, Expectation, ConV, VOut),
                                                      constrain_args(K, Kc, Gc),
                                                      build_branch(ConV, VOut, Out, Then),
                                                      ( Rs == [] -> Goal = ((Kv = Kc) -> Then), KGi=[]
                                                                  ; translate_case(Rs, Kv, Expectation, Out, Next, KGi, [K|Prior]),
                                                                    Goal = ((Kv = Kc) -> Then ; Next) ),
                                                      append([Gc,KGi], KGo).

%A constructed positional tuple with known field types has a structural product
%type; bind deep patterns against it rather than constructor-only field typing.
case_scrutinee_value_type(Kv, KT) :-
    ( var(Kv) -> known_singleton(Kv, KT0)
              ; value_single_type(Kv, KT0) ),
    nonvar(KT0),
    KT = KT0.

%Translate arguments recursively:
translate_args([], [], []).
translate_args([X|Xs], Goals, [V|Vs]) :- translate_expr(X, G1, V),
                                         translate_args(Xs, G2, Vs),
                                         append(G1, G2, Goals).

%foldall's result is the accumulator function's uniquely declared or inferred
%output type, trusted only when the initial value (returned for an empty
%generator) fits it too:
foldall_out_type(AFV, Init, Out) :- ( atom(AFV),
                                      findall(OT, ( fn_decl_arity(AFV, 2, _, OT)
                                                  ; inferred_decl_arity(AFV, 2, _, OT) ), [OT1]),
                                      ( var(Init) -> known_singleton(Init, IT)
                                      ; value_single_type(Init, IT) ),
                                      \+ \+ type_unify(IT, OT1)
                                      -> set_out_type(Out, OT1)
                                       ; true ).

%A candidate set with the unknown marker leaves the element type unknown, so no
%(List T) claim is made over it:
collapse_elem_type(EV, ET, Known) :- ( var(EV), known_candidates_certain(EV, Cs)
                                       -> ( Cs = [C1] -> ET = C1 ; ET = ['|'|Cs] ),
                                          Known = true
                                     ; nonvar(EV), value_single_type(EV, ET0) -> ET = ET0,
                                                                                 Known = true
                                     ; Known = false ).

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

declared_output_type(F, OutType) :- atom(F),
									nonvar(OutType),
									catch(match('&self', [':', F, TypeChain], TypeChain, TypeChain), _, fail),
									TypeChain = [->|Types],
									append(_, [DeclaredOutType], Types),
									DeclaredOutType == OutType.
