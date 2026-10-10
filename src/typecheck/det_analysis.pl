%%% Argument-aware transitive determinism through higher-order functions:
%%% effect-polymorphic and closure-conditional effect analysis, case coverage
%%% and whole-clause-set exhaustiveness validation.
:- dynamic det_exhaustive_verdict/8.
%A function whose declaration leaves a higher-order parameter uncommitted can
%still be det conditionally on its closure arguments: fold-flat is det when
%the folded closure is. When the actual argument at a call site is det, the
%callee's clauses are re-analyzed with the closure positions treated as det.
%Stored clause metas carry no arrow attribute (they are captured before
%clause_param_types runs), so the det arrow is derived from the unique
%declaration and attached to COPIES of the metas; a plain -> never counts as
%det globally.
%An effect-polymorphic declaration is used only when it is the unique
%declaration at this arity: the walker sees only F/Arity and cannot choose
%between several effect equations.
effect_poly_decl(F, N, Name, ATs, Positions) :-
    findall(decl(As, Effect),
            fn_decl_copy(F, N, scheme(As, _), Effect, _, _),
            [decl(ATs,
                  effect_model(variable(Name),
                               [effect_var(Name, StoredPositions)]))]),
    maplist(stored_effect_position(Name), StoredPositions, Positions),
    Positions \== [].

stored_effect_position(Name, closure_arg(Idx, M), pos(Idx, M, Arrow)) :-
    effect_arrow_atom(Arrow, Name).

%The declaration's intrinsic effect: the clause metas analyzed with only the
%$v closure slots assumed det. An unproved body stays unspecified, so callers
%can report why no proof exists.
effect_body_determinism(F, N, Name, Det) :-
    effect_body_determinism_proof(F, N, Name, Proof),
    analysis_proof_verdict(Proof, Det),
    analysis_reemit_proof(Proof).

effect_body_determinism_proof(F, N, Name, Proof) :-
    analysis_cache_lookup(effect(F, N, Name), Proof), !.
effect_body_determinism_proof(F, N, Name, Proof) :-
    catch(b_getval('$effect_assume_stack', St), _, St = []),
    memberchk(effect(F, N, Name), St), !,
    analysis_make_proof(effect_body(F/N, Name), det, [],
                        [effect(F/N), decl(F/N), clause_set(F/N)], Proof).
effect_body_determinism_proof(F, N, Name, Proof) :-
    effect_poly_decl(F, N, Name, ATs, Positions),
    fun_metas(F, N, Metas),
    maplist(assume_det_meta(ATs, Positions), Metas, Upgraded),
    catch(b_getval('$effect_assume_stack', St), _, St = []),
    setup_call_cleanup(
        b_setval('$effect_assume_stack', [effect(F, N, Name)|St]),
        clause_set_subject_proof(effect_body(F/N, Name), F/N, enforced(F, N),
                                 Upgraded, Proof),
        b_setval('$effect_assume_stack', St)),
    analysis_cache_store(effect(F, N, Name), Proof).


%Instantiate $v from every corresponding closure argument and join it with the
%intrinsic body verdict. A missing closure verdict stays `unspecified`.
effect_poly_call_determinism(F, N, Args, Det) :-
    effect_poly_decl(F, N, Name, _, Positions),
    effect_body_determinism(F, N, Name, BodyDet),
    effect_positions_instantiation(Positions, Args, Inst),
    effect_poly_selection_determinism(F, N, Args, Selection),
    effect_join(BodyDet, Inst, IntrinsicAndClosure),
    effect_join(IntrinsicAndClosure, Selection, Det).

effect_poly_selection_determinism(F, N, Args, Det) :-
    fun_metas(F, N, Metas),
    inferred_selection_determinism(F, N, Args, Metas, Det).

effect_positions_instantiation([], _, det).
effect_positions_instantiation([pos(Idx, M, _)|Ps], Args, Det) :-
    ( nth0(Idx, Args, Arg), closure_effect_level(Arg, M, Here)
      -> true
    ; Here = unspecified ),
    effect_positions_instantiation(Ps, Args, Rest),
    effect_join(Here, Rest, Det).

closure_effect_level(Arg, _, Det) :-
    var(Arg), !,
    known_singleton(Arg, K), arrow_head_level(K, L),
    concrete_effect_level(L, Det).
closure_effect_level(['|->', _, Body], _, Det) :- !,
    deterministic_expr_core(Body, R), det_result_effect(R, Det).
closure_effect_level(partial(F, _), M, Det) :- !,
    named_closure_effect(F, M, Det).
%A partial application of a closure parameter, ($pred $x), is still a closure
%when $pred has arguments left; applying it has the parameter arrow's effect.
closure_effect_level([F|Bound], M, Det) :- var(F), !,
    known_singleton(F, K), arrow_head_level(K, L),
    K = [_|Rest], length(Rest, Len), Total is Len - 1,
    length(Bound, B), M is Total - B,
    concrete_effect_level(L, Det).
closure_effect_level([F|_], _, Det) :- atom(F), !,
    fn_own_arity(F, A), named_closure_effect(F, A, Det).
closure_effect_level(F, M, Det) :- atom(F), !,
    named_closure_effect(F, M, Det).

named_closure_effect(F, M, Det) :-
    catch(function_call_determinism(F, M, D0), _, fail),
    concrete_effect_level(D0, Det).

concrete_effect_level(det, det).
concrete_effect_level(semidet, semidet).
concrete_effect_level(nondet, nondet).

det_result_effect(ok, det).
det_result_effect(may_fail(_), semidet).
det_result_effect(nondeterministic(_), nondet).
det_result_effect(unknown(_), unspecified).

effect_join(unspecified, _, unspecified) :- !.
effect_join(_, unspecified, unspecified) :- !.
effect_join(nondet, _, nondet) :- !.
effect_join(_, nondet, nondet) :- !.
effect_join(semidet, _, semidet) :- !.
effect_join(_, semidet, semidet) :- !.
effect_join(det, det, det).

%Declared arrow parameter positions other than -[nondet]->, as
%pos(Index, InArity, Head). A -[semidet]-> position is only ever upgraded to
%det when the actual argument proves det:
arrow_det_positions(ATs, Positions) :- findall(pos(Idx, M, H),
                                              ( nth0(Idx, ATs, T), is_arrow_type(T),
                                                T = [H|Rest], arrow_atom_det(H, L), L \== nondet,
                                                length(Rest, Len), M is Len - 1 ),
                                              Positions).

%Fun must have a unique arity-N declaration exposing at least one non-nondet
%arrow parameter, and every such position's actual argument must be det:
det_closure_args_ok(Fun, N, Args) :- unique_fn_decl(Fun, N, ATs1, _),
                                     arrow_det_positions(ATs1, Positions),
                                     Positions \== [],
                                     det_closure_positions(Positions, Args).

det_closure_positions([], _).
det_closure_positions([pos(Idx, M, _)|Ps], Args) :- nth0(Idx, Args, Arg),
                                                    det_arg_evidence(Arg, M),
                                                    det_closure_positions(Ps, Args).

%An actual argument carries det evidence when it is a var whose known arrow
%commits to det, a lambda with a det body, or a (partial) application or atom
%naming a function that is det at the relevant arity:
det_arg_evidence(Arg, M) :- closure_effect_level(Arg, M, det).

%The named function's own full arity (declared, else from stored clauses):
fn_own_arity(F2, A) :- fn_decl_arity(F2, A, _, _), !.
fn_own_arity(F2, A) :- catch(nb_getval(F2, Metas), _, fail),
                       member(Meta, Metas), Meta = fun_meta(As, _, _),
                       length(As, A), !.

%body_determinism given det arrow-typed parameters, on copies of the clause
%metas. Its own recursion stack and memo keep the unconditional analysis
%($det_stack) from ever certifying a plain -> as det.
body_determinism_assuming(F, N, Det) :-
    body_determinism_assuming_proof(F, N, Proof),
    analysis_proof_verdict(Proof, Det),
    analysis_reemit_proof(Proof).

body_determinism_assuming_proof(F, N, Proof) :-
    analysis_cache_lookup(assume(F, N), Proof), !.
body_determinism_assuming_proof(F, N, Proof) :-
    catch(b_getval('$det_assume_stack', St), _, St = []),
    memberchk(F/N, St), !,
    analysis_make_proof(conditional_body(F/N), det, [],
                        [effect(F/N), decl(F/N), clause_set(F/N)], Proof).
body_determinism_assuming_proof(F, N, Proof) :-
    fun_metas(F, N, Metas),
    unique_fn_decl(F, N, ATs1, _),
    arrow_det_positions(ATs1, Positions),
    Positions \== [],
    maplist(assume_det_meta(ATs1, Positions), Metas, Upgraded),
    catch(b_getval('$det_assume_stack', St), _, St = []),
    setup_call_cleanup(
        b_setval('$det_assume_stack', [F/N|St]),
        ( det_enforced_flag(F, N, Enf),
          clause_set_subject_proof(conditional_body(F/N), F/N, Enf, Upgraded,
                                   Proof) ),
        b_setval('$det_assume_stack', St)),
    analysis_cache_store(assume(F, N), Proof).

%Attach the det form of each declared arrow to the COPIED head var:
assume_det_meta(ATs1, Positions, Meta, Meta2) :- copy_term(Meta, Meta2),
                                                 Meta2 = fun_meta(Args, _, _),
                                                 maplist(bind_meta_param, Args, ATs1),
                                                 assume_det_positions(Positions, ATs1, Args).

assume_det_positions([], _, _).
assume_det_positions([pos(Idx, _, _)|Ps], ATs1, Args) :- nth0(Idx, ATs1, T), T = [_|Rest],
                                                        copy_term(Rest, Rest2),
                                                        det_arrow_head(det, DetHead),
                                                        DetArrow = [DetHead|Rest2],
                                                        ( nth0(Idx, Args, HeadArg) -> assume_det_param(HeadArg, DetArrow) ; true ),
                                                        assume_det_positions(Ps, ATs1, Args).

%Only a var head parameter is upgraded; any other shape stays conservative:
assume_det_param(V, DetArrow) :- ( var(V),
                                   ( get_attr(V, tknown, [K]) -> ( nonvar(K), is_arrow_type(K) ) ; true )
                                 -> put_attr(V, tknown, [DetArrow])
                                 ; true ).

%Underapplication builds a closure instead of calling: forming it is det (the
%closure is judged at its call site), so only the bound arguments count:
underapplied_closure(Fun, N) :- CallArity is N + 1,
                                \+ arity(Fun, CallArity),
                                arity(Fun, Known), Known > CallArity, !.

%%% Combining determinism verdicts. The lattice is
%%% ok < may_fail(_) < nondeterministic(_) / unknown(_); may_fail keeps scanning
%%% for something worse and the top short-circuits, so the first non-ok reason
%%% is reported.
%(once E) caps the solution count at one, erasing nondeterminism and opacity,
%but it fails exactly when E does, so it keeps may_fail.
once_determinism(Expr, Result) :- deterministic_expr_core(Expr, R),
                                  ( ( R == ok ; R = may_fail(_) ) -> Result = R
                                  ; ( R = nondeterministic(Why) ; R = unknown(Why) )
                                    -> Result = may_fail(once(Why))
                                  ; Result = may_fail(once(R)) ).

det_result_rank(ok, 0).
det_result_rank(may_fail(_), 1).
det_result_rank(nondeterministic(_), 2).
det_result_rank(unknown(_), 2).

det_result_final(R) :- det_result_rank(R, 2).

combine_det_results(A, B, R) :- det_result_rank(A, RA), det_result_rank(B, RB),
                                ( RB > RA -> R = B ; R = A ).

combine_determinism_list([], ok).
combine_determinism_list([Expr|Exprs], Result) :- deterministic_expr_core(Expr, First),
                                                  ( det_result_final(First) -> Result = First
                                                  ; combine_determinism_list(Exprs, Rest),
                                                    combine_det_results(First, Rest, Result) ).

%(let* ((P1 V1) (P2 V2) ...) Body) is nested lets, so each binding gets
%let_determinism/4's refinements:
binds_and_body_determinism([], Body, Result) :- deterministic_expr_core(Body, Result).
binds_and_body_determinism([[Pat, Val]|Rest], Body, Result) :-
    ( Rest == [] -> In = Body ; In = ['let*', Rest, Body] ),
    let_determinism(Pat, Val, In, Result).

case_expr_determinism(KeyExpr, PairsExpr, Result) :- deterministic_expr_core(KeyExpr, KeyResult),
                                                     ( det_result_final(KeyResult) -> Result = KeyResult
                                                     ; case_pairs_determinism(PairsExpr, R2),
                                                       case_coverage_determinism(KeyExpr, PairsExpr, R3),
                                                       combine_det_results(KeyResult, R2, R12),
                                                       combine_det_results(R12, R3, Result) ).

%%% Does the case cover its scrutinee? translate_case/6 compiles no final
%%% else, so an unmatched value makes the whole case fail - a path no branch
%%% shows. Nominal types are open, so only a provably uncovered value yields
%%% may_fail (unmatched_case/5, as for clause heads); an unknown or extensible
%%% scrutinee type stays silent.
case_coverage_determinism(KeyExpr, PairsExpr, Result) :-
    ( case_scrutinee_type(KeyExpr, T0), copy_term(T0, T),
      case_value_patterns(PairsExpr, Heads), Heads \== [],
      catch(unmatched_case([], Heads, 0, T, Missing0), _, fail),
      copy_term(Missing0, Missing)
      -> Result = may_fail(nonexhaustive_case(Missing))
       ; Result = ok ).

%The scrutinee's type when already known: a single known variable type, or the
%one value-typing candidate of a non-variable (literals and declared-output
%calls):
case_scrutinee_type(K, T) :- var(K), !, known_singleton(K, T0), nonvar(T0), T = T0.
case_scrutinee_type(K, T) :- nonvar(K), value_candidate_types(K, [T0]),
                             nonvar(T0), T = T0.

%The branch patterns as one-column heads. The (Empty ...) branch is the
%no-solution fallback, not a value pattern, so it covers nothing:
case_value_patterns(Pairs, Heads) :- is_list(Pairs),
                                     findall([P], ( member(Pair, Pairs), nonvar(Pair),
                                                    Pair = [P, _], P \== 'Empty' ),
                                             Heads).

case_pairs_determinism([], ok).
case_pairs_determinism([[CaseExpr, BranchExpr]|Rest], Result) :-
    pattern_then_exprs(CaseExpr, [BranchExpr], PairResult),
    ( det_result_final(PairResult) -> Result = PairResult
    ; case_pairs_determinism(Rest, R2),
      combine_det_results(PairResult, R2, Result) ).

%Patterns are matched, not executed: a variable head is structure, and only
%fun-headed subterms are embedded calls that the pattern evaluates:
pattern_then_exprs(Pat, Exprs, Result) :- deterministic_pattern(Pat, R0),
                                          ( det_result_final(R0) -> Result = R0
                                          ; combine_determinism_list(Exprs, R2),
                                            combine_det_results(R0, R2, Result) ).

%(let Pat Val In) / (chain Pat Val In). A destructuring pattern's fields whose
%declared types are concrete non-arrow types can never be function symbols, so
%a body headed by such a field ((let ($l $r) (add-pair ..) ($l $r))) is data
%construction, not dynamic dispatch. The field types are bound on COPIES of the
%pattern and body, never the shared source term.
let_determinism(Pat, Val, In, Result) :-
    deterministic_pattern(Pat, R0),
    ( det_result_final(R0) -> Result = R0
    ; deterministic_expr_core(Val, RVal),
      ( det_result_final(RVal) -> Result = RVal
      ; copy_term(Pat-In, PatC-InC),
        ignore(bind_destructured_field_types(PatC, Val)),
        %A plain let variable gets the producer's declared result type in the
        %analysis copy, so call selection can consume List/Bool/nominal
        %evidence; the runtime result check emitted for Val backs it.
        ignore(bind_plain_result_type(PatC, Val)),
        %A plain-var pattern bound to a guaranteed proper list (collapse or a
        %proper_list_output-certified call) lets the (== $v ()) narrowing fire
        %on it. A declared (List _) output does not qualify: the narrowing's
        %coverage leg needs the value to be a list unconditionally.
        ( var(PatC), val_guaranteed_proper_list(Val)
          -> add_known_type(PatC, ['List', '%Undefined%']),
             put_attr(PatC, proper_list_cert, true),
             ProperListVar = proper(PatC)
        ; ProperListVar = none ),
        %Fields of unknown type can arrive bound to functions; mark them as
        %parameters, not fresh locals:
        term_variables(PatC, FVs),
        maplist(mark_field_unless_typed, FVs),
        with_scoped_proper_list_var(
            ProperListVar,
            deterministic_expr_core(InC, RIn)),
        let_pattern_match_result(Pat, Val, RMatch),
        combine_det_results(RVal, RIn, RVI),
        combine_det_results(RMatch, RVI, RVIM),
        combine_det_results(R0, RVIM, Result) ) ).

%Translation places Pattern = Value before the value's goals, a real
%zero-result path unless the source value or its declared product shape
%entails the pattern.
let_pattern_match_result(Pat, Val, ok) :-
    let_pattern_entailed(Pat, Val), !.
%A cut in the value position either fails the unification before the cut or
%commits before producing a result; body_conditionally_commits/1 is the
%clause-set side of the same rule.
let_pattern_match_result(_, Val, ok) :-
    nonvar(Val), Val = [Cut], Cut == cut, !.
let_pattern_match_result(Pat, _, may_fail(let_pattern(Pat))).

let_pattern_entailed(Pat, _) :- var(Pat), !.
let_pattern_entailed(Pat, Val) :-
    manifest_pattern_match(Pat, Val), !.
%A higher-order call can instantiate a parametric result through its closure
%argument; resolve the declaration closure-first, but only for a nonempty
%positional product destructured into distinct variables.
let_pattern_entailed(Pat, Val) :-
    resolved_call_output_type(Val, T),
    fresh_variable_product_pattern(Pat, T), !.
let_pattern_entailed(Pat, Val) :-
    call_output_type(Val, T),
    pattern_entailed_by_type(Pat, T).

resolved_call_output_type([F|Args], OT) :-
    atom(F), length(Args, N),
    findall(call(ATs0, OT0),
            fn_decl_arity(F, N, ATs0, OT0),
            [call(ATs, OT)]),
    %Closure positions first, as in the translator; -1 skips no binder.
    resolve_source_arrow_args(Args, ATs, -1),
    nonvar(OT).

fresh_variable_product_pattern(Pat, T) :-
    is_list(Pat), Pat = [_|_],
    contextual_product_type(T),
    same_length(Pat, T),
    maplist(var, Pat),
    pairwise_distinct_vars(Pat).

pairwise_distinct_vars([]).
pairwise_distinct_vars([V|Vs]) :-
    \+ ( member(W, Vs), V == W ),
    pairwise_distinct_vars(Vs).

manifest_pattern_match(Pat, Val) :-
    atomic(Pat), atomic(Val), Pat == Val, !.
manifest_pattern_match(Pat, Val) :-
    is_list(Pat), is_list(Val),
    same_length(Pat, Val),
    maplist(manifest_pattern_field, Pat, Val).

manifest_pattern_field(P, _) :- var(P), !.
manifest_pattern_field(P, V) :- manifest_pattern_match(P, V).

pattern_entailed_by_type(Pat, _) :- var(Pat), !.
pattern_entailed_by_type(Pat, T) :-
    tagged_tuple_type(T, Tag, FieldTypes), !,
    is_list(Pat), Pat = [PatternTag|Fields],
    PatternTag == Tag,
    same_length(Fields, FieldTypes),
    maplist(pattern_entailed_by_type, Fields, FieldTypes).
pattern_entailed_by_type(Pat, T) :-
    is_list(Pat), contextual_product_type(T),
    same_length(Pat, T),
    maplist(pattern_entailed_by_type, Pat, T).
%A nominal value always matches a constructor pattern only while that
%constructor is the type's sole shape and no bare constants exist; ctor_set
%makes the snapshot invalidatable.
pattern_entailed_by_type(Pat, T) :-
    atom(T), \+ primitive_type(T), \+ wildcard_type(T),
    analysis_emit(dependency(ctor_set(T))),
    findall(Ctor-Arity, member_ctor(T, Arity, Ctor), Keys0),
    sort(Keys0, [Ctor-Arity]),
    \+ ( declared_value_type(Value, ValueType),
         atom(Value), \+ fun(Value), ValueType == T ),
    is_list(Pat), Pat = [PatternCtor|Fields],
    PatternCtor == Ctor,
    length(Fields, Arity),
    findall(FieldTypes,
            ( fn_decl_arity(Ctor, Arity, FieldTypes, OutType),
              type_compat_soft(OutType, T) ),
            [OnlyFieldTypes]),
    maplist(pattern_entailed_by_type, Fields, OnlyFieldTypes).

with_scoped_proper_list_var(none, Goal) :- !, call(Goal).
with_scoped_proper_list_var(proper(V), Goal) :-
    ( catch(b_getval('$proper_list_vars', Saved), _, fail) -> true
    ; Saved = [] ),
    setup_call_cleanup(
        b_setval('$proper_list_vars', [V|Saved]),
        Goal,
        b_setval('$proper_list_vars', Saved)).

scoped_proper_list_var(V) :-
    get_attr(V, proper_list_cert, true), !.
scoped_proper_list_var(V) :-
    catch(b_getval('$proper_list_vars', Vars), _, fail),
    member(Here, Vars), Here == V, !.

%Bind each destructured field to its concrete non-arrow, non-wildcard field
%type from Val's declared tuple output:
bind_destructured_field_types(Pat, Val) :-
    functional_pattern_application(Pat, _, _), !,
    call_output_type(Val, OT),
    bind_pattern_typed(Pat, OT).
bind_destructured_field_types(Pat, Val) :-
    is_list(Pat), Pat = [_|_],
    call_output_type(Val, OT),
    is_list(OT), same_length(Pat, OT),
    bind_pat_field_types(Pat, OT).

bind_pat_field_types([], []).
bind_pat_field_types([P|Ps], [T|Ts]) :- ( var(P), nonvar(T), \+ is_arrow_type(T), \+ wildcard_type(T)
                                          -> add_known_type(P, T) ; true ),
                                        bind_pat_field_types(Ps, Ts).

bind_plain_result_type(Pat, Val) :-
    var(Pat),
    call_output_type(Val, T),
    nonvar(T),
    \+ is_arrow_type(T),
    \+ wildcard_type(T),
    add_known_type(Pat, T).

mark_field_unless_typed(V) :- ( get_attr(V, tknown, _) -> true ; note_unknown_candidate(V) ).

call_output_type([F|Args], OT) :- atom(F), length(Args, N), fn_decl_arity(F, N, _, OT), nonvar(OT).

deterministic_pattern(P, ok) :- ( var(P) ; atomic(P) ; P = partial(_, _) ), !.
deterministic_pattern([H|T], Result) :- atom(H), fun(H), !, deterministic_expr_core([H|T], Result).
deterministic_pattern(P, Result) :- combine_pattern_list(P, Result).

combine_pattern_list([], ok).
combine_pattern_list([E|Es], Result) :- deterministic_pattern(E, R1),
                                        ( det_result_final(R1) -> Result = R1
                                        ; combine_pattern_list(Es, R2),
                                          combine_det_results(R1, R2, Result) ).

%%% Exhaustiveness of explicit -[det]-> functions. A clause set that cannot
%%% match some input of its declared types delivers zero results. Nominal types
%%% are open, so only PROVABLY incomplete is an error; "cannot tell" is
%%% accepted, and a real incompleteness is declared -[semidet]->. The check
%%% runs in every mode, as a per-file prepass over the parsed forms, because
%%% exhaustiveness is a property of the whole clause set and clauses arrive one
%%% form at a time. Clauses compiled by earlier files count too.
revalidate_dependency_consumer(exhaustiveness(F, N), Event) :-
    det_exhaustive_verdict(F, N, StoredHeads, Consts, _, File, Line, Str),
    current_exhaustiveness_heads(F, N, CurrentHeads),
    ( CurrentHeads == [],
      Event = clause_changed(_, runtime)
      -> retractall(det_exhaustive_verdict(F, N, _, _, _, _, _, _)),
         forget_validation_dependencies(exhaustiveness(F, N))
    ; ( CurrentHeads == [] -> Heads = StoredHeads ; Heads = CurrentHeads ),
      in_metta_file(
          File,
          with_form_location(
              Line, Str,
              det_exhaustiveness_proof(Consts, F, N, Heads, Proof))),
      analysis_proof_dependencies(Proof, Dependencies),
      retractall(det_exhaustive_verdict(F, N, _, _, _, _, _, _)),
      assertz(det_exhaustive_verdict(F, N, Heads, Consts, Dependencies,
                                     File, Line, Str)),
      record_validation_dependencies(exhaustiveness(F, N), Dependencies)
    ).

current_exhaustiveness_heads(F, N, Heads) :-
    findall(Args,
            ( translated_from(Ref, [Eq, Head, _]),
              Eq == (=),
              clause(_, _, Ref),
              nonvar(Head),
              Head = [F0|Args],
              F0 == F,
              length(Args, N) ),
            Heads).

det_exhaustiveness_prepass(ParsedForms) :-
    findall(F/N, ( parsed_clause_head(ParsedForms, _, _, F, Args), length(Args, N) ), Keys0),
    sort(Keys0, Keys),
    %Value declarations are not pre-cached, so the file's own nullary
    %constructors are read from its forms:
    findall(C-T, parsed_value_decl(ParsedForms, C, T), Consts),
    forall(member(F/N, Keys), check_det_exhaustive_group(ParsedForms, Consts, F, N)).

parsed_clause_head(ParsedForms, Line, Str, F, Args) :-
    member(parsed(function, Str, Line, Form), ParsedForms),
    nonvar(Form), Form = [Eq, Head, _], Eq == (=),
    nonvar(Head), Head = [F|Args], atom(F), Args \== [].

parsed_value_decl(ParsedForms, C, T) :- member(parsed(expression, _, _, Form), ParsedForms),
                                        nonvar(Form), Form = [Colon, C, T], Colon == (:),
                                        atom(C), atom(T), \+ fun(C).

stored_clause_head(F, N, Args) :- catch(nb_getval(F, Metas), _, fail),
                                  member(Meta, Metas),
                                  Meta = fun_meta(Args, _, _),
                                  length(Args, N).

check_det_exhaustive_group(ParsedForms, Consts, F, N) :-
    ( ( explicit_det_decl(F, N)
      ; effect_det_exhaustiveness_required(ParsedForms, F, N) )
      -> findall(Args, ( parsed_clause_head(ParsedForms, _, _, F, Args), length(Args, N)
                       ; stored_clause_head(F, N, Args) ), Heads),
         once(( parsed_clause_head(ParsedForms, Line, Str, F, A0), length(A0, N) )),
         %The verdict is a snapshot of the constructor sets it consulted; a
         %later constructor re-runs exactly the verdicts its type is in:
         with_form_location(
             Line, Str,
             det_exhaustiveness_proof(Consts, F, N, Heads, ExhaustiveProof)),
         analysis_proof_dependencies(ExhaustiveProof, ExhaustiveDeps),
         current_metta_file(File),
         retractall(det_exhaustive_verdict(F, N, _, _, _, _, _, _)),
         assertz(det_exhaustive_verdict(F, N, Heads, Consts, ExhaustiveDeps,
                                        File, Line, Str)),
         record_validation_dependencies(exhaustiveness(F, N), ExhaustiveDeps)
       ; true ).

det_exhaustiveness_proof(Consts, F, N, Heads, Proof) :-
    analysis_collect(
        check_det_exhaustive(Consts, F, N, Heads), Events),
    analysis_term_dependencies(Heads, TermDeps),
    analysis_function_decl_dependencies(F, DeclDeps),
    append([[decl(F/N), clause_set(F/N)], TermDeps, DeclDeps], Ds0),
    sort(Ds0, Dependencies),
    analysis_make_proof(exhaustiveness(F/N), exhaustive, Events,
                        Dependencies, Proof).

%An effect-polymorphic declaration promises exactly one result at its det
%instantiation only when its intrinsic body verdict is det; derive that from
%the parsed clause group so the exhaustiveness check runs before compiling.
effect_det_exhaustiveness_required(ParsedForms, F, N) :-
    effect_poly_decl(F, N, Name, ATs, Positions),
    findall(fun_meta(Args, Body, clean),
            ( member(parsed(function, _, _, Form), ParsedForms),
              Form = [Eq, Head, Body], Eq == (=),
              Head = [F0|Args], F0 == F, length(Args, N)
            ; catch(nb_getval(F, Stored), _, fail),
              member(Meta, Stored),
              Meta = fun_meta(Args, Body, _),
              length(Args, N) ),
            Metas),
    Metas \== [],
    maplist(assume_det_meta(ATs, Positions), Metas, Upgraded),
    catch(b_getval('$effect_assume_stack', St), _, St = []),
    setup_call_cleanup(
        b_setval('$effect_assume_stack', [effect(F, N, Name)|St]),
        with_det_enforced(enforced(F, N),
                          clause_set_determinism(Upgraded, det)),
        b_setval('$effect_assume_stack', St)).

%The declaration must be unique at this arity: several declarations are typed
%overloads, and the clauses then belong to no single argument-type vector.
check_det_exhaustive(Consts, F, N, Heads) :-
    ( unique_fn_decl(F, N, ATs1, _),
      nth0(Idx, ATs1, T),
      unmatched_case(Consts, Heads, Idx, T, Missing)
      -> Pos is Idx + 1,
         throw(error(det_nonexhaustive(F, Pos, Missing), determinism))
       ; true ).

%One argument position proves incompleteness when every clause pins it to a
%recognizable shape and some value of its declared type matches none. A
%variable in the column, an unenumerable type or an unrecognized pattern makes
%it silent; body guards are ignored (they only make a function match less).
unmatched_case(Consts, Heads, Idx, T, Missing) :-
    nonvar(T), \+ wildcard_type(T),
    findall(P, ( member(H, Heads), nth0(Idx, H, P) ), Col),
    Col \== [],
    maplist(pattern_key, Col, Keys),
    ( uncovered_infinite_domain(T, Keys) -> Missing = other(T)
    ; domain_keys(T, Consts, DKeys), member(Missing, DKeys), \+ memberchk(Missing, Keys) ).

%The value shape a head pattern matches, as key(Name, Arity); a variable,
%computed subterm or () has none:
pattern_key(P, key(P, 0)) :- atomic(P), P \== [], \+ ( atom(P), fun(P) ), !.
pattern_key(P, key(C, K)) :- is_list(P), P = [C|As], atom(C), \+ fun(C),
                             length(As, K), K > 0.

%Number and String have infinitely many values, so a column of literals of
%that domain can never cover them:
uncovered_infinite_domain('Number', Keys) :- forall(member(key(V, A), Keys), ( A =:= 0, number(V) )).
uncovered_infinite_domain('String', Keys) :- forall(member(key(V, A), Keys), ( A =:= 0, string(V) )).

%The complete set of value shapes of a type, when enumerable: Bool, or a
%nominal type's equation-less constructors (member_ctor/3) and constants. It
%is a snapshot of the current declarations; users publish ctor_set/1.
domain_keys('Bool', _, [key(true, 0), key(false, 0)]) :- !.
domain_keys(T, Consts, Keys) :- atom(T), declared_newtype(T, R), !, domain_keys(R, Consts, Keys).
domain_keys(T, Consts, Keys) :- atom(T), \+ wildcard_type(T), \+ primitive_type(T),
                                analysis_emit(dependency(ctor_set(T))),
                                findall(key(C, K), nominal_ctor(T, Consts, C, K), Keys0),
                                sort(Keys0, Keys), Keys \== [].

nominal_ctor(T, _, C, K) :- member_ctor(T, K, C).
nominal_ctor(T, _, C, 0) :- declared_value_type(C, T2), atom(C), T2 == T, \+ fun(C).
nominal_ctor(T, Consts, C, 0) :- member(C-T, Consts).

%Rendered by main.pl's error message for det_nonexhaustive/3:
missing_case_text(other(T), Txt) :- !, format(atom(Txt), "a ~w outside the matched literals", [T]).
missing_case_text(key(C, 0), Txt) :- !, format(atom(Txt), "~w", [C]).
missing_case_text(key(C, K), Txt) :- length(As, K), maplist(=('_'), As),
                                     atomic_list_concat([C|As], ' ', Inner),
                                     format(atom(Txt), "(~w)", [Inner]).
