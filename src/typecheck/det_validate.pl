%%% Determinism arrows (-[det]->, -[semidet]->, -[nondet]->) %%%
%
% Owns committed-effect validation, overload effect aggregation, overlap
% checks, and publication of proof-required boundary provisos. Consumes
% canonical declarations, clause/body proofs, checker flags, and translator
% metadata. Boundary: det_bound_proviso/4 is published only by this unit and
% cleared through declaration/recompile lifecycle interfaces.

%The per-function UNION of head positions whose boundness a determinism proof
%CONSUMED. Published by the validation boundary from the returned proof and read by
%det_boundness_checks/3 (translator.pl) to emit exactly the boundary checks the
%certificate relied on. Retracted per function in forget_symbol_types/1
%(decl_store.pl) and at the top of recompile_function_clauses/1 (translator.pl),
%so a late declaration re-derives the set from scratch.
:- dynamic det_bound_proviso/4.   % det_bound_proviso(F, N, Pos, nonvar|proper_list)

%Several declarations at one arity are overloads, and the function as a whole
%can only be trusted with the WEAKEST commitment any of them makes: det and
%semidet both commit (semidet is the weaker of the two), while a nondet or an
%uncommitted plain -> overload leaves the whole function uncommitted. A
%det/nondet (or semidet/nondet) pair is a contradiction, not a weakening:
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

%Publish the clause's HEAD variables for the duration of a body determinism
%analysis, as one $det_head_scope term scope(HeadVars, DirectParams, Args). The
%full head-variable set and its direct-parameter subset are captured from the
%same head at the same moment and are always read together, so they live in a
%single published term with a single setup/restore; the whole Args list rides
%along so a consumed direct parameter can be located by its 1-based POSITION
%(enforced_bound_param/1's consumption record). A head variable is a parameter:
%it can arrive bound to anything the caller chose, functions included, whatever
%its declared type says - and a wildcard-typed parameter carries no type
%attribute at all (bind_param_type records nothing for Atom/%Undefined%), so
%identity is the only reliable test. unify_head_is_data/1 consults this to tell a
%parameter from a fresh local. (The commitment gate $det_enforced is deliberately
%NOT folded in here: it has a coarser lifetime - see the context inventory in
%flags_arrows.pl.) The direct-parameter subset excludes is-var-EXEMPT parameters
%(see det_enforced_params/3): a parameter the body tests with is-var gets no
%boundary check, so nothing downstream may treat it as bound either.
with_det_head_vars(Args, Body, Goal) :- catch(b_getval('$det_head_scope', Saved), _, Saved = scope([], [], [])),
                                  term_variables(Args, HVs),
                                  det_enforced_params(Args, Body, DPs),
                                  setup_call_cleanup(b_setval('$det_head_scope', scope(HVs, DPs, Args)),
                                                     Goal,
                                                     b_setval('$det_head_scope', Saved)).

%%% The is-var exemption: the ONE list both the check emission and the
%%% strengthenings read, so they cannot disagree.
%
% A clause that applies is-var to a parameter has HANDLED the unbound case -
% the author wrote exactly the branch a boundary check would preempt, so
% throwing before their handler runs would be the compiler overruling them.
% Such a parameter is exempt from the boundness check, and symmetrically it
% never counts as enforced-bound for the call-site strengthenings: the
% analysis must then prove the body deterministic WITH a possibly-unbound
% value, which the is-var branch structure is precisely equipped to do.
% Detection is a syntactic walk of the source body; over-detection (an
% is-var under quote, say) only costs the friendlier boundary error, never
% soundness, because both consumers weaken together.
det_enforced_params(Args, Body, DPs) :- include(var, Args, Vs),
                                        exclude(boundness_exempt_param(Body), Vs, DPs).

boundness_exempt_param(Body, V) :- body_applies_is_var(Body, V).

body_applies_is_var(E, V) :- nonvar(E), E = [H, X], H == 'is-var', X == V, !.
body_applies_is_var(E, V) :- nonvar(E), E = [X|Xs],
                             ( body_applies_is_var(X, V) -> true
                             ; body_applies_is_var(Xs, V) ).

det_head_var(H) :- catch(b_getval('$det_head_scope', scope(HVs, _, _)), _, fail),
                   member(V, HVs), V == H, !.

%A DIRECT variable parameter: a top-level head argument that is ITSELF a
%variable, as opposed to a variable field inside a destructured parameter like
%(P $u). term_variables (det_head_var/1) cannot tell them apart - it flattens
%(P $u) to [$u] - but only direct params get the boundness check in
%translate_clause, so the strengthenings must key on THIS, not det_head_var/1.
det_direct_param(H) :- catch(b_getval('$det_head_scope', scope(_, DPs, _)), _, fail),
                       member(V, DPs), V == H, !.

%The commitment gate. Published alongside the head vars whenever a body's
%determinism is analysed; enforced(F, N) only when the analysis subject itself
%carries an explicit -[det]->/-[semidet]-> arrow (validate_function_determinism),
%so its direct params are guaranteed bound at runtime by the boundary check - and
%it names the function so a consumption can be recorded against it. A transitive
%callee reached through body_determinism sets it from ITS OWN declaration, which
%in practice is never committed (a committed callee is answered from its
%declaration and never body-analysed), so it is false there - wired explicitly,
%not left to happen by accident.
with_det_enforced(Bool, Goal) :- catch(b_getval('$det_enforced', Saved), _, Saved = false),
                                 setup_call_cleanup(b_setval('$det_enforced', Bool),
                                                    Goal,
                                                    b_setval('$det_enforced', Saved)).

det_enforced_now :- det_enforced_fn(_, _).

%The (F, N) of the committed function currently under body analysis, or fail if
%the gate is not raised (value false):
det_enforced_fn(F, N) :- catch(b_getval('$det_enforced', E), _, fail), E = enforced(F, N).

%A DIRECT parameter of a function under an active committed arrow: the
%boundary check guarantees it is bound at runtime, so - unlike an arbitrary
%typed argument, which may arrive unbound out of ordinary well-typed code - the
%strengthenings below may treat it as bound. A destructured FIELD is not a
%direct param and does NOT qualify (the boundary check is spine-level).
%
%Succeeding here is the determinism proof CONSUMING this parameter's boundness.
%The consumption is returned as an analysis event.  Nothing in the analyzer
%publishes det_bound_proviso/4; ensure_deterministic_expr/3 is the compile /
%validation boundary that decides to publish the returned requirements.
enforced_bound_param(V) :- det_direct_param(V), det_enforced_fn(_, _),
                           ignore(note_bound_consumed(V, nonvar)).

%A list-enumerating builtin needs more than the generic boundness promise:
%a partial list still enumerates its tail. Record the stronger proper-list
%boundary proviso when its list input is a direct committed parameter.
enforced_proper_list_param(V) :- det_direct_param(V), det_enforced_fn(_, _),
                                 ignore(note_bound_consumed(V, proper_list)).

%A recursive traversal's tail is not itself a direct parameter, but a proper
%list boundary on the direct parameter proves every tail reached by matching
%a cons spine proper as well. Publish the requirement against the original
%parameter position; this is the user-defined counterpart of the list-builtin
%proper-list proviso.
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

%Locate V's 1-based position among the published head Args (by identity - V is a
%direct param, so it is a spine element of Args) and union it into the proviso
%set for (F, N). ignore/1 above keeps a failure to locate (an unexpected scope
%shape) from unravelling the strengthening: the boundness fact is still true.
note_bound_consumed(V, Kind) :- b_getval('$det_head_scope', scope(_, _, Args)),
                                nth1(Pos, Args, A), A == V, !,
                                analysis_emit(required_bound(Pos, Kind)).

publish_det_proof_requirements(F, N, Proof) :-
    analysis_proof_requirements(Proof, Bounds),
    forall(member(bound(Pos, Kind), Bounds),
           ( det_bound_proviso(F, N, Pos, Kind) -> true
           ; assertz(det_bound_proviso(F, N, Pos, Kind)) )).

%Whether the function whose body is about to be analysed carries an explicit
%committed arrow, as the gate value the enforcement publishes (enforced(F, N)
%carries the identity a consumption is recorded against):
det_enforced_flag(F, N, Flag) :- ( boundary_commitment(F, N, _) -> Flag = enforced(F, N)
                                                               ; Flag = false ).

%Only explicit commitments are enforced. Plain arrows are uncommitted in the
%modes that accept them and are rejected at declaration time by --strict-det.
boundary_commitment(F, N, Det) :- explicit_committed_decl(F, N, Det).
boundary_commitment(F, N, effect(Name)) :- effect_poly_decl(F, N, Name, _, _).

%A det body must neither branch nor fail; a semidet body is the same analysis
%minus the may-not-fail part - (empty) and calls to -[semidet]-> functions are
%exactly what it is allowed to do, and nothing else changes (superpose, match
%and overlapping heads stay rejected for both):
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
    fun_meta_parts(Meta, PrevArgs, PrevBody, _),
    ( clause_heads_overlap(Args, PrevArgs),
      \+ body_commits(PrevBody),
      \+ body_conditionally_commits(PrevBody)
      -> throw(error(overlapping_deterministic_clauses(F, Args, PrevArgs), determinism))
       ; ensure_non_overlapping_clause_heads(F, Args, Rest) ).

%Only a direct cut is guaranteed to execute before any failure or choice
%point.  In particular let/let* constrain their pattern before running the
%value goals, so a cut used as a let value is not an entry commitment.
body_commits(E) :- nonvar(E),
                   E = [C], C == cut.

%A cut used as the first let/let* value is not an unconditional entry commit:
%the pattern unification runs first.  It is nevertheless a safe conditional
%clause-selection commit: either that unification fails before the cut, or the
%cut executes before the clause can produce a value.  Keep this separate from
%body_commits/1 so code generation never mistakes it for an entry cut.
body_conditionally_commits(E) :- nonvar(E),
    ( E = [L, _, V, _], L == let, nonvar(V), V = [C], C == cut
    ; E = [L, [[_, V]|_], _], L == 'let*',
      nonvar(V), V = [C], C == cut ).

clause_heads_overlap(ArgsA, ArgsB) :- copy_term((ArgsA, ArgsB), (CA, CB)),
                                      unifiable(CA, CB, _).
