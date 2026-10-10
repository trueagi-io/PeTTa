%%% Determinism arrows (-[det]->, -[semidet]->, -[nondet]->): committed-effect
%%% validation, overload effect aggregation, overlap checks, and the boundary
%%% provisos a determinism proof consumed.

%The per-function union of head positions whose boundness a determinism proof
%consumed; det_boundness_checks/3 (translator.pl) emits exactly these boundary
%checks. Cleared by forget_symbol_types/1 and recompile_function_clauses/1, so
%a late declaration re-derives it.
:- dynamic det_bound_proviso/4.   % det_bound_proviso(F, N, Pos, nonvar|proper_list)

%Overloads at one arity commit only as weakly as their weakest declaration
%(semidet is weaker than det; a nondet or plain -> overload uncommits the
%function). A det/nondet or semidet/nondet pair is a contradiction:
fn_determinism(F, N, Det) :- findall(D, fn_decl_top_effect(F, N, D), Ds0),
                             sort(Ds0, Ds),
                             ( Ds == [] -> Det = unspecified
                             ; Ds = [D1] -> Det = D1
                             ; Ds == [det, semidet] -> Det = semidet
                             ; Ds == [det, unspecified] -> Det = unspecified
                             ; Ds == [semidet, unspecified] -> Det = unspecified
                             ; Ds == [nondet, unspecified] -> Det = nondet
                             ; fn_decl_locations(F, N, Locations),
                               throw(error(conflicting_determinism_declarations(F, Locations),
                                           determinism)) ).

fn_decl_top_effect(F, N, Det) :-
    fn_decl(F, N, _, Effect, _, _),
    effect_model_det(Effect, Det).

fn_decl_locations(F, N, Locations) :-
    findall(Location,
            fn_decl(F, N, _, _, _, provenance(Location, _)),
            Locations0),
    sort(Locations0, Locations).

validate_function_determinism(F, Args, BodyExpr, PrevClauses, HeadForm) :-
    length(Args, N),
    fn_determinism(F, N, Det),
    ( committed_det(Det) -> ( HeadForm == head_goals
                              -> throw(error(det_functional_head_commitment(F, Args),
                                             determinism))
                            ; true ),
                            det_enforced_flag(F, N, Enf),
                            with_det_enforced(Enf,
                                with_det_head_vars(Args, BodyExpr, ensure_deterministic_expr(Det, BodyExpr, F))),
                            ensure_non_overlapping_clause_heads(F, Args, PrevClauses)
    ; Det = effect(Name) -> effect_body_determinism_proof(F, N, Name, Proof),
                            publish_det_proof_requirements(F, N, Proof),
                            analysis_reemit_proof(Proof)
                          ; true ).

%Publish the clause's head as $det_head_scope = scope(HeadVars, DirectParams,
%Args) while its body is analysed. A head variable is a parameter that can
%arrive bound to anything, and a wildcard-typed one carries no attribute, so
%identity is the only test (unify_head_is_data/1). Args locates a consumed
%direct parameter by position. DirectParams excludes is-var-exempt parameters
%(det_enforced_params/3). The commitment gate $det_enforced has the coarser
%clause-set lifetime and is kept separate.
with_det_head_vars(Args, Body, Goal) :- catch(b_getval('$det_head_scope', Saved), _, Saved = scope([], [], [])),
                                  term_variables(Args, HVs),
                                  det_enforced_params(Args, Body, DPs),
                                  setup_call_cleanup(b_setval('$det_head_scope', scope(HVs, DPs, Args)),
                                                     Goal,
                                                     b_setval('$det_head_scope', Saved)).

%%% The is-var exemption, read by both the check emission and the
%%% strengthenings so they cannot disagree. A clause that applies is-var to a
%%% parameter handles the unbound case itself, so the parameter gets no
%%% boundary check and never counts as enforced-bound. Detection is a syntactic
%%% walk; over-detection only weakens both consumers together.
det_enforced_params(Args, Body, DPs) :- include(var, Args, Vs),
                                        exclude(boundness_exempt_param(Body), Vs, DPs).

boundness_exempt_param(Body, V) :- body_applies_is_var(Body, V).

body_applies_is_var(E, V) :- nonvar(E), E = [H, X], H == 'is-var', X == V, !.
body_applies_is_var(E, V) :- nonvar(E), E = [X|Xs],
                             ( body_applies_is_var(X, V) -> true
                             ; body_applies_is_var(Xs, V) ).

det_head_var(H) :- catch(b_getval('$det_head_scope', scope(HVs, _, _)), _, fail),
                   member(V, HVs), V == H, !.

%A direct parameter: a top-level head argument that is itself a variable, not
%a field of a destructured parameter like (P $u). Only direct parameters get
%the boundness check, so the strengthenings key on this, not det_head_var/1.
det_direct_param(H) :- catch(b_getval('$det_head_scope', scope(_, DPs, _)), _, fail),
                       member(V, DPs), V == H, !.

%The commitment gate: enforced(F, N) while the body of a function with an
%explicit -[det]->/-[semidet]-> arrow is analysed, so its direct parameters are
%bound at runtime and consumptions are recorded against F/N; false otherwise.
with_det_enforced(Bool, Goal) :- catch(b_getval('$det_enforced', Saved), _, Saved = false),
                                 setup_call_cleanup(b_setval('$det_enforced', Bool),
                                                    Goal,
                                                    b_setval('$det_enforced', Saved)).

%The (F, N) of the committed function currently under body analysis, or fail if
%the gate is not raised (value false):
det_enforced_fn(F, N) :- catch(b_getval('$det_enforced', E), _, fail), E = enforced(F, N).

%A direct parameter under an active committed arrow is bound at runtime, which
%an ordinary typed argument is not. Succeeding consumes its boundness; the
%consumption is returned as an analysis event and published by
%ensure_deterministic_expr/3.
enforced_bound_param(V) :- det_direct_param(V), det_enforced_fn(_, _),
                           ignore(note_bound_consumed(V, nonvar)).

%A partial list still enumerates its tail, so a list-enumerating builtin needs
%the stronger proper-list proviso on its direct committed parameter.
enforced_proper_list_param(V) :- det_direct_param(V), det_enforced_fn(_, _),
                                 ignore(note_bound_consumed(V, proper_list)).

%A proper-list boundary on a direct parameter also makes every tail reached by
%matching a cons spine proper; the requirement is published against the
%original parameter position.
enforced_proper_list_value(V) :- enforced_proper_list_param(V), !.
enforced_proper_list_value(V) :- enforced_recursive_proper_list_value(V).

enforced_recursive_proper_list_value(V) :-
    var(V), det_enforced_fn(_, _),
    b_getval('$det_head_scope', scope(_, _, Args)),
    nth1(Pos, Args, Root),
    recursive_list_tail_var(Root, V), !,
    analysis_emit(required_bound(Pos, proper_list)).

recursive_list_tail_var(Pattern, V) :-
    transformed_cons_pattern(Pattern, _, Tail),
    ( Tail == V
    ; nonvar(Tail), recursive_list_tail_var(Tail, V) ).

%Union V's 1-based position among the head Args (by identity) into the proviso
%set for (F, N):
note_bound_consumed(V, Kind) :- b_getval('$det_head_scope', scope(_, _, Args)),
                                nth1(Pos, Args, A), A == V, !,
                                analysis_emit(required_bound(Pos, Kind)).

publish_det_proof_requirements(F, N, Proof) :-
    analysis_proof_requirements(Proof, Bounds),
    forall(member(bound(Pos, Kind), Bounds),
           ( det_bound_proviso(F, N, Pos, Kind) -> true
           ; assertz(det_bound_proviso(F, N, Pos, Kind)) )).

%The gate value for a function about to be analysed:
det_enforced_flag(F, N, Flag) :- ( boundary_commitment(F, N, _) -> Flag = enforced(F, N)
                                                               ; Flag = false ).

%Only explicit commitments are enforced. Plain arrows are uncommitted in the
%modes that accept them and are rejected at declaration time by --strict-det.
boundary_commitment(F, N, Det) :- explicit_committed_decl(F, N, Det).
boundary_commitment(F, N, effect(Name)) :- effect_poly_decl(F, N, Name, _, _).

%A det body must neither branch nor fail; a semidet body may fail, but superpose,
%match and overlapping heads stay rejected for both:
ensure_deterministic_expr(Det, Expr, Fun) :-
    deterministic_expr_proof(Expr, Proof),
    analysis_reemit_proof(Proof),
    analysis_proof_verdict(Proof, R),
    ( det_enforced_fn(Fun, N)
      -> publish_det_proof_requirements(Fun, N, Proof)
      ; true ),
    ( R == ok -> true
    ; Det == semidet, R = may_fail(_) -> true
    ; R = nondeterministic(Reason) -> throw(error(determinism_conflict(Fun, Reason), determinism))
    ; R = may_fail(Reason) -> throw(error(determinism_conflict(Fun, Reason), determinism))
    ; throw(error(determinism_conflict(Fun, unknown(Expr)), determinism)) ).

%A clause whose body commits with (cut) never falls through to a later
%clause, so overlap with it cannot create a choicepoint:
ensure_non_overlapping_clause_heads(_, _, []).
ensure_non_overlapping_clause_heads(F, Args, [Meta|Rest]) :-
    Meta = fun_meta(PrevArgs, PrevBody, _),
    ( clause_heads_overlap(Args, PrevArgs),
      \+ body_commits(PrevBody),
      \+ body_conditionally_commits(PrevBody)
      -> throw(error(overlapping_deterministic_clauses(F, Args, PrevArgs), determinism))
       ; ensure_non_overlapping_clause_heads(F, Args, Rest) ).

%Only a direct cut is guaranteed to run before any failure or choice point;
%let/let* unify their pattern before the value goals.
body_commits(E) :- nonvar(E),
                   E = [C], C == cut.

%A cut as the first let/let* value is a conditional clause-selection commit:
%the pattern unification either fails before it or the cut runs before a value
%is produced. Kept apart from body_commits/1 so code generation never treats it
%as an entry cut.
body_conditionally_commits(E) :- nonvar(E),
    ( E = [L, _, V, _], L == let, nonvar(V), V = [C], C == cut
    ; E = [L, [[_, V]|_], _], L == 'let*',
      nonvar(V), V = [C], C == cut ).

clause_heads_overlap(ArgsA, ArgsB) :- copy_term((ArgsA, ArgsB), (CA, CB)),
                                      unifiable(CA, CB, _).
