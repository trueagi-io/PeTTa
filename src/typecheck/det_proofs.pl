%%% Determinism proof rules and expression walker.
%
% Owns argument-sensitive builtin verdicts, manifest-shape and output
% certificates, closure cardinality, and the functional clause/body/expression
% determinism proof walk.
% Consumes the builtin registry, declaration/type queries, analysis proof and
% validation interfaces, plus effect/coverage helpers from det_analysis.pl.
% Boundary: procedural registry hooks stay here, but the fact that a builtin
% uses one lives in builtin_registry.pl. Every owned predicate is wholly here.
%
%builtin_call_determinism/3 is keyed on (name, arity) only, so one worst-case
%verdict has to cover every call site. Most of the weak verdicts above are
%weak for a SHAPE reason - the argument may be unbound, or an open list - and
%at a call site where the shape is manifest in the source the reason does not
%apply. This relation is consulted BEFORE the flat table and may only ever
%return a verdict at least as strong; where the shape cannot be established it
%simply fails and the flat table answers, the same provable-only discipline
%the -[det]-> exhaustiveness check uses.
%
%The judgements are about the SPINE, never the elements: a manifest list may
%hold unbound elements, and a builtin that raises on one (min_list/2 on a
%non-number) is still det, because an exception is not a solution.
call_site_determinism(F, N, Args, Det) :-
    call_site_base_determinism(F, N, Args, Base),
    ( trusted_unverified_call(F, Args)
      -> call_effect_join(Base, semidet, Det)
    ; Det = Base ).

call_site_base_determinism(F, N, Args, Det) :- builtin_call_determinism_args(F, N, Args, Det), !.
call_site_base_determinism(F, N, Args, Det) :- effect_poly_call_determinism(F, N, Args, Det), !.
call_site_base_determinism(F, N, _, Det) :- table_det_verdict(F, N, Det), !.
call_site_base_determinism(F, N, _, Det) :-
    catch(fn_determinism(F, N, Det0), _, fail),
    Det0 \== unspecified, !,
    Det = Det0.
call_site_base_determinism(F, N, Args, Det) :-
    ( inferred_call_determinism(F, N, Args, Inferred)
      -> Det = Inferred
    ; Det = unspecified ).

%A clause body's cardinality is not the cardinality of calling its function.
%For an uncommitted function, clause selection is part of the call: a bound
%argument may select no clause, while an unbound argument may enumerate
%several non-overlapping heads. Combine the body proof with a call-site
%selection proof instead of publishing the former as the latter.
inferred_call_determinism(F, N, Args, Det) :-
    catch(nb_getval(F, Metas0), _, fail),
    include(arity_meta(N), Metas0, Metas),
    Metas \== [],
    body_determinism(F, N, BodyDet),
    inferred_selection_determinism(F, N, Args, Metas, SelectionDet),
    call_effect_join(BodyDet, SelectionDet, Det).

%The argument-independent view used for a named function value has no future
%call site from which to learn boundness.  It therefore needs a proof over the
%whole input domain; probing the relation once with fresh variables is not
%such a proof (a single partial clause would look exactly-one).
inferred_unknown_call_determinism(F, N, Det) :-
    catch(nb_getval(F, Metas0), _, fail),
    include(arity_meta(N), Metas0, Metas),
    Metas \== [],
    body_determinism(F, N, BodyDet),
    inferred_total_selection_determinism(F, N, Metas, SelectionDet),
    call_effect_join(BodyDet, SelectionDet, Det).

call_effect_join(unspecified, _, unspecified) :- !.
call_effect_join(_, unspecified, unspecified) :- !.
call_effect_join(nondet, _, nondet) :- !.
call_effect_join(_, nondet, nondet) :- !.
call_effect_join(semidet, _, semidet) :- !.
call_effect_join(_, semidet, semidet) :- !.
call_effect_join(det, det, det).

%First exploit values whose applicability is already decidable at the source
%call site. Otherwise a multi-clause relation is at most-one only when one
%bound argument position carries distinct top-level head keys. It is
%exactly-one when those keys also cover that argument's known domain.
inferred_selection_determinism(F, N, Args, Metas, Det) :-
    ( member(MetaWithGoals, Metas), fun_meta_head_goals(MetaWithGoals)
      -> Det = unspecified
    ; maplist(call_head_status(Args), Metas, Statuses),
      ( memberchk(unknown, Statuses)
        -> Det = unspecified
      ; inferred_selection_statuses(F, N, Args, Metas, Statuses, Det) ) ).

inferred_selection_statuses(F, N, Args, Metas, Statuses, Det) :-
    include(==(yes), Statuses, Yeses),
    include(==(possible), Statuses, Possibles),
    length(Yeses, YN),
    length(Possibles, PN),
    ( PN =:= 0
      -> ( YN =:= 1 -> Det = det
         ; YN =:= 0 -> Det = semidet
         ; Det = nondet )
    ; YN =:= 0, PN =:= 1,
      single_possible_domain_covered(Args, Metas, Statuses)
      -> Det = det
    ; keyed_selection_position(Args, Metas, Idx, Keys),
      nth0(Idx, Args, Arg),
      selection_argument_bound(Arg, BoundKind)
      -> ( selection_column_covers(F, N, Args, Idx, Keys, BoundKind)
           -> Det = det
         ; Det = semidet )
    ; YN =:= 0, PN =:= 1
      -> Det = semidet
    ; Det = nondet ).

%A merely possible clause becomes positively applicable only on a domain that
%the current path has actually narrowed. The v1 narrowing carried by the
%analyzer is nonempty-list evidence; require the selected clause to cover all
%such lists and every other head position to be already decidable.
single_possible_domain_covered(Args, Metas, Statuses) :-
    nth0(MetaIndex, Statuses, possible),
    nth0(MetaIndex, Metas, Meta),
    fun_meta_parts(Meta, HeadArgs, _, _),
    nth0(Idx, Args, Arg),
    var(Arg), nonempty_var(Arg),
    known_singleton(Arg, T), nonvar(T), list_type(T, _),
    nth0(Idx, HeadArgs, Pattern),
    covers_all_nonempty_lists(Pattern),
    call_other_positions_yes(Args, HeadArgs, Idx, 0).

call_other_positions_yes([], [], _, _).
call_other_positions_yes([A|As], [P|Ps], Skip, I) :-
    ( I =:= Skip -> true
    ; call_pattern_status(A, P, yes) ),
    I2 is I + 1,
    call_other_positions_yes(As, Ps, Skip, I2).

%Argument-independent selection is exactly-one only when the normalized heads
%are mutually exclusive and visibly cover the entire input domain.  This is
%deliberately a small, provable-only relation: a universal head, or a complete
%constructor/literal discriminator with otherwise-unconstrained positions.
inferred_total_selection_determinism(_, _, Metas, unspecified) :-
    member(Meta, Metas), fun_meta_head_goals(Meta), !.
inferred_total_selection_determinism(_, _, Metas, nondet) :-
    selection_heads_overlap(Metas), !.
inferred_total_selection_determinism(F, N, Metas, det) :-
    total_selection_heads(F, N, Metas), !.
inferred_total_selection_determinism(_, _, _, semidet).

selection_heads_overlap(Metas) :-
    append(_, [M1|Rest], Metas),
    fun_meta_parts(M1, A1, _, _),
    member(M2, Rest),
    fun_meta_parts(M2, A2, _, _),
    clause_heads_overlap(A1, A2), !.

total_selection_heads(_, _, [Meta]) :-
    fun_meta_parts(Meta, Args, _, _),
    maplist(var, Args), !.
total_selection_heads(F, N, Metas) :-
    keyed_head_column(Metas, Idx, Keys),
    all_other_head_positions_unconstrained(Metas, Idx),
    selection_function_arg_type(F, N, Idx, T),
    selection_domain_keys(T, Domain0),
    sort(Domain0, Domain),
    sort(Keys, Domain).

keyed_head_column(Metas, Idx, Keys) :-
    Metas = [First|_],
    fun_meta_parts(First, Args, _, _),
    nth0(Idx, Args, _),
    findall(P, (member(Meta, Metas),
                fun_meta_parts(Meta, HArgs, _, _),
                nth0(Idx, HArgs, P)), Col),
    maplist(selection_pattern_key, Col, Keys),
    sort(Keys, Unique),
    same_length(Keys, Unique),
    maplist(selection_pattern_covers_key, Col).

selection_pattern_covers_key(P) :- atomic(P), !.
selection_pattern_covers_key(P) :-
    transformed_cons_pattern(P, H, T), !,
    var(H), var(T).
selection_pattern_covers_key(P) :-
    is_list(P), P = [_|Fields], Fields \== [],
    maplist(var, Fields).

all_other_head_positions_unconstrained(Metas, Idx) :-
    forall(( member(Meta, Metas),
             fun_meta_parts(Meta, Args, _, _),
             nth0(J, Args, P), J =\= Idx ),
           var(P)).

selection_function_arg_type(F, N, Idx, T) :-
    findall(ATs, fn_decl_arity(F, N, ATs, _), [ATs]), !,
    nth0(Idx, ATs, T).
selection_function_arg_type(F, N, Idx, T) :-
    findall(ATs, (inferred_fn_type(F, ATs, _), length(ATs, N)), [ATs]),
    nth0(Idx, ATs, T).

call_head_status(Args, Meta, Status) :-
    fun_meta_parts(Meta, HeadArgs, _, _),
    maplist(call_pattern_status, Args, HeadArgs, PosStatuses),
    combine_pattern_statuses(PosStatuses, Status).

combine_pattern_statuses(Statuses, no) :- memberchk(no, Statuses), !.
combine_pattern_statuses(Statuses, unknown) :- memberchk(unknown, Statuses), !.
combine_pattern_statuses(Statuses, possible) :- memberchk(possible, Statuses), !.
combine_pattern_statuses(_, yes).

call_pattern_status(_, Pattern, yes) :- var(Pattern), !.
call_pattern_status(Actual, _, possible) :- var(Actual), !.
%An evaluated expression normally contributes no source-shape evidence: its
%syntax is not its result.  An output certificate is the one exception.  It
%proves the result is bound and in a finite selector shape, while deliberately
%leaving WHICH shape (empty/cons, true/false) possible until runtime.
call_pattern_status(Actual, _, possible) :-
    nonvar(Actual),
    \+ selection_transparent_actual(Actual),
    selection_expression_certificate(Actual, _), !.
call_pattern_status(Actual, _, unknown) :-
    nonvar(Actual),
    \+ selection_transparent_actual(Actual), !.
call_pattern_status(Actual, Pattern, Status) :-
    selection_actual_key(Actual, AK),
    selection_pattern_key(Pattern, PK), !,
    ( AK == PK -> Status = yes ; Status = no ).
call_pattern_status(Actual, Pattern, Status) :-
    ( \+ unifiable(Actual, Pattern, _) -> Status = no
    ; ground(Actual), ground(Pattern) -> Status = yes
    ; Status = possible ).

%Only source values whose outer selection shape survives translation may
%participate in a head-selection proof.  Compiler forms and evaluated
%subexpressions deliberately contribute no evidence until selection operates
%on a future semantic IR rather than source syntax.
selection_transparent_actual(X) :- atomic(X), !.
selection_transparent_actual([]) :- !.
selection_transparent_actual(X) :-
    is_list(X), X = [H|T],
    \+ ( atom(H), special_builtin_form(H, T, _) ),
    data_headed(H).

%MeTTa's proper-list value and its source `cons` pattern use different source
%shapes but the same runtime selection key.
selection_actual_key([], list_empty) :- !.
selection_actual_key(X, list_cons) :-
    is_list(X), X = [H|_], data_headed(H), !.
selection_actual_key(X, key(X, 0)) :-
    atomic(X), X \== [], \+ (atom(X), fun(X)).

selection_pattern_key([], list_empty) :- !.
selection_pattern_key(P, list_cons) :-
    transformed_cons_pattern(P, _, _), !.
selection_pattern_key(P, K) :- pattern_key(P, K).

%constrain_args/3 normalizes source (cons H T) heads to Prolog [H|T].
%Accept the source form too for declaration-prepass metadata.
transformed_cons_pattern(P, H, T) :-
    nonvar(P), P = [C, H, T], (C == cons ; C == 'cons-atom'), !.
transformed_cons_pattern(P, H, T) :-
    nonvar(P), P = [H|T], var(T).

%A key column gives a unique clause selector only when every clause exposes a
%key there and no key is repeated. Repeated/nested discriminators stay
%conservative because a merely nonvar boundary does not ground their fields.
keyed_selection_position(Args, Metas, Idx, Keys) :-
    nth0(Idx, Args, _),
    findall(P, (member(Meta, Metas),
                fun_meta_parts(Meta, HArgs, _, _),
                nth0(Idx, HArgs, P)), Col),
    Col \== [],
    maplist(selection_pattern_key, Col, Keys),
    sort(Keys, Unique),
    same_length(Keys, Unique),
    maplist(selection_pattern_covers_key, Col).

selection_argument_bound(A, proper_list) :-
    var(A), scoped_proper_list_var(A), !.
selection_argument_bound(A, proper_list) :-
    var(A), selection_argument_list_type(A),
    typed_selection_evidence(A, proper_list), !.
selection_argument_bound(A, proper_list) :-
    var(A), enforced_recursive_proper_list_value(A), !.
selection_argument_bound(A, nonvar) :-
    var(A), known_singleton(A, T), nonvar(T),
    typed_selection_evidence(A, nonvar), !.
selection_argument_bound(A, nonvar) :-
    var(A), enforced_bound_param(A), !.
selection_argument_bound(A, proper_list) :-
    nonvar(A), selection_expression_certificate(A, proper_list), !.
selection_argument_bound(A, nonvar) :-
    nonvar(A), selection_expression_certificate(A, nonvar), !.
selection_argument_bound(A, proper_list) :-
    nonvar(A), manifest_proper_list(A), !.
selection_argument_bound(A, nonvar) :- nonvar(A).

%The expression-output counterpart of manifest/typed evidence.  Consume the
%functional certificate proof directly so its output_cert dependencies enter
%the enclosing analysis proof and Phase 4 can invalidate the consumer if a
%producer later gains a non-certifying clause.
selection_expression_certificate(Expr, Kind) :-
    selection_value_preserving_wrapper(Expr, Inner),
    selection_argument_bound(Inner, Kind), !.
selection_expression_certificate(Expr, proper_list) :-
    selection_proper_list_expression_certificate(Expr, Dependencies),
    emit_selection_certificate_dependencies(Dependencies).
selection_expression_certificate(Expr, nonvar) :-
    output_result_qualifies_core(bound_bool, Expr, [], yes, Dependencies),
    emit_selection_certificate_dependencies(Dependencies).

%Unlike data/make-list/evaluated compiler forms, these lower to their inner
%value unchanged; they may safely forward shape evidence without treating
%their source syntax as runtime structure.
selection_value_preserving_wrapper(Expr, Inner) :-
    nonvar(Expr), Expr = [W, _, Inner], ( W == the ; W == brand ).

%Literal/data spines belong to manifest evidence, not to this evaluated-output
%path.  Name the two intrinsic producers explicitly; every other accepted
%expression must have consumed a named producer certificate (and therefore
%return a nonempty dependency set).  This keeps compiler forms such as
%(data $tag 0) from masquerading as their source list syntax.
selection_proper_list_expression_certificate(Expr, []) :-
    nonvar(Expr), Expr = [C, _], C == collapse, !.
selection_proper_list_expression_certificate(Expr, []) :-
    nonvar(Expr), Expr = [F, _], F == list_to_set, !.
selection_proper_list_expression_certificate(Expr, Dependencies) :-
    output_result_qualifies_core(proper_list, Expr, [], yes, Dependencies),
    Dependencies \== [].

emit_selection_certificate_dependencies([]).
emit_selection_certificate_dependencies([Dependency|Dependencies]) :-
    analysis_emit(dependency(Dependency)),
    emit_selection_certificate_dependencies(Dependencies).

%A known selector type is usable selection evidence in the same two cases as
%the argument-sensitive builtin rules:
%  * a direct parameter of the enclosing committed clause, where consuming
%    the evidence publishes the appropriate runtime boundary proviso; or
%A local variable's type constraint is not shape evidence: an unbound value
%can carry List/Bool metadata without yet being a proper list or a bound
%selector. Locals qualify through the scoped producer-certificate paths above.
%A variable nested in the source head is neither.  Merely constraining such a
%field to List/Bool does not bind it, so it must still be justified by the
%recursive-tail/scoped-certificate paths above.
typed_selection_evidence(A, proper_list) :-
    det_direct_param(A),
    enforced_proper_list_param(A).
typed_selection_evidence(A, nonvar) :-
    det_direct_param(A),
    enforced_bound_param(A).

selection_column_covers(_, _, _, _, Keys, proper_list) :-
    sort([list_empty, list_cons], Domain),
    sort(Keys, Domain).
selection_column_covers(F, N, Args, Idx, Keys, _) :-
    selection_argument_type(F, N, Args, Idx, T),
    selection_domain_keys(T, Domain0),
    sort(Domain0, Domain),
    sort(Keys, Domain).

selection_argument_list_type(A) :-
    known_singleton(A, T), nonvar(T), list_type(T, _), !.
selection_argument_list_type(A) :-
    nonvar(A), manifest_proper_list(A).

selection_argument_type(_, _, Args, Idx, T) :-
    nth0(Idx, Args, A), known_singleton(A, T0), nonvar(T0), !, T = T0.
selection_argument_type(F, N, _, Idx, T) :-
    findall(ATs, fn_decl_arity(F, N, ATs, _), [ATs]),
    nth0(Idx, ATs, T).

selection_domain_keys('Bool', [key(true, 0), key(false, 0)]) :- !.
selection_domain_keys(T, Keys) :-
    domain_keys(T, [], Keys).

%The registry owns WHICH builtins have argument-sensitive cardinality. This
%file owns only the irreducibly procedural meaning of each named rule.
builtin_call_determinism_args(F, N, Args, Det) :-
    builtin_argument_rule(F, N, Rule),
    builtin_argument_rule_verdict(Rule, F, Args, Det).

builtin_argument_rule_defined(proper_list_arg0).
builtin_argument_rule_defined(proper_list_arg1).
builtin_argument_rule_defined(nonempty_list_arg0).
builtin_argument_rule_defined(manifest_indexed_list).
builtin_argument_rule_defined(space_update).
builtin_argument_rule_defined(manifest_foreign_goal).
builtin_argument_rule_defined(bound_membership_probe).
builtin_argument_rule_defined(manifest_booleans).

builtin_conditional_rule_defined(higher_order_list).

%%% FEATURE 1 - flow-sensitive nonemptiness upgrade %%%
%
%A -[semidet]-> USER-function call is det AT A NARROWED SITE. semidet means AT
%MOST one, and a callee produces ZERO in two ways: no clause head matches, or a
%clause body fails. So the upgrade to det requires BOTH legs proven:
%  (a) COVERAGE - the callee's clause heads cover the narrowed domain (here:
%      every NONEMPTY list matches some head - a (cons $h $t) head, stored as a
%      var-headed/var-tailed cons cell, matches every nonempty list), and
%  (b) NO-FAIL BODIES - every clause body is may-not-fail. body_determinism/3
%      returns det exactly when every body is `ok` (may-not-fail) AND the heads
%      do not overlap, so it certifies (b) and rules out the multi-solution case
%      in one call; a body that is just a head variable is trivially ok.
%Provable-only: any leg failing simply fails this predicate, and the semidet
%verdict stands - this feature only ever UPGRADES, it never rejects.
%
%Minimal by design: single-argument (arity-1) callees, a variable argument
%narrowed to a nonempty value of a list type by an == ()-shaped condition, and a
%unique declaration at the arity.
%
%Runtime clause changes match the consumed effect/clause-set dependency in the
%graph, so a previously compiled consumer cannot keep this upgrade after the
%callee's clauses invalidate it.
semidet_site_upgraded_to_det(Fun, N, Args) :-
    N =:= 1,
    nth0(Idx, Args, A), var(A), nonempty_var(A),
    known_singleton(A, T), nonvar(T), list_type(T, _),
    findall(ATs, fn_decl_arity(Fun, N, ATs, _), [_]),
    catch(nb_getval(Fun, Metas0), _, fail),
    include(arity_meta(N), Metas0, Metas), Metas \== [],
    nonempty_list_domain_covered(Metas, Idx),
    body_determinism(Fun, N, det).

%The narrowed domain is "nonempty lists", and every nonempty list value is a
%cons cell, so ONE clause-head pattern that matches every cons cell covers the
%whole domain: a bare variable (matches anything), or a cons cell whose head and
%tail are both unconstrained variables (matches every list of length >= 1). A
%pattern that pins the head or fixes the tail length covers only part of it.
nonempty_list_domain_covered(Metas, Idx) :- member(Meta, Metas),
                                            fun_meta_parts(Meta, HArgs, _, _),
                                            nth0(Idx, HArgs, P), covers_all_nonempty_lists(P), !.

covers_all_nonempty_lists(P) :- var(P), !.
covers_all_nonempty_lists(P) :- nonvar(P), P = [H|Tl], var(H), var(Tl).

%The narrowing note: variables proven NONEMPTY on the current analysis path.
%A b_setval-scoped list, restored on exit (deterministic_expr's if clause). The
%stored terms are the shared body variables, so membership is by identity (==).
with_nonempty_var(V, Goal) :- catch(b_getval('$nonempty_vars', Saved), _, Saved = []),
                              setup_call_cleanup(b_setval('$nonempty_vars', [V|Saved]),
                                                 Goal,
                                                 b_setval('$nonempty_vars', Saved)).

nonempty_var(V) :- catch(b_getval('$nonempty_vars', Vs), _, fail), member(X, Vs), X == V, !.

%(== V ()) or (== () V) with V a variable whose known type is a (List _). The
%empty literal () is the empty Prolog list here; == is the structural test the
%if compiles its condition from:
nonempty_narrowing_var([Eq, A, B], V) :- Eq == '==',
                                         ( var(A), B == [] -> V = A
                                         ; var(B), A == [] -> V = B ),
                                         known_singleton(V, T), nonvar(T), list_type(T, _).

expression_spine_narrowing_var([Pred, V], V) :-
    Pred == 'is-expr', var(V).

%--- Strengthened by a manifest list SPINE.
%length/2, reverse/2, append/3 and friends invert over an open list; over a
%proper one they answer exactly once. The properness is read off the source.
builtin_argument_rule_verdict(proper_list_arg0, _, [A], det) :-
    manifest_proper_list(A).
%append/3 and its aliases only need their FIRST list proper: the recursion is
%driven by it and the second operand is copied through untouched.
builtin_argument_rule_verdict(proper_list_arg0, _, [A, _], det) :-
    manifest_proper_list(A).
%exclude/3 walks its LIST argument, which is the second one here.
builtin_argument_rule_verdict(proper_list_arg1, _, [_, L], det) :-
    manifest_proper_list(L).
%last/2 and min/max_list/2 need the list NON-empty as well: they have no
%answer for (), and min-atom's non_list/1 guard does not catch it.
builtin_argument_rule_verdict(nonempty_list_arg0, _, [A], det) :-
    manifest_nonempty_list(A).
%nth0/3 enumerates only when the index is unbound; with a literal index it is
%semidet (out of range fails, and the range is not manifest).
builtin_argument_rule_verdict(manifest_indexed_list, _, [A, I], semidet) :-
    integer(I), manifest_proper_list(A).

%--- Strengthened by a manifest expression ARGUMENT.
%'add-atom'/3 and 'remove-atom'/3 (src/spaces.pl) are keyed semidet because
%their second clause reads Term as a list - add_sexp/remove_sexp do
%Term =.. [Space,Rel|Args], which FAILS on a non-list argument (an atom or an
%unbound variable), so a call whose argument shape is unknown may fail. When
%the argument is a manifest non-empty expression the =.. succeeds, assertz /
%retractall are det, and the function-equation first clause (guarded by cut on
%[=,_,_]) is equally det - so exactly one clause commits: the call is det.
%A DECLARED type never qualifies (see manifest_proper_list's note); only a
%literal spine built at the call site does. The payload is passed raw - the
%typed_space_update lowering never evaluates it - so a literal expression is
%such a spine whatever its head: a variable head is stored, not applied.
builtin_argument_rule_verdict(space_update, _, [_, T], det) :-
    ( is_list(T), T = [_|_] -> true ; manifest_nonempty_list(T) ).
%--- callPredicate: argument-sensitive via DECLARED foreign promises.
%callPredicate calls an arbitrary Prolog goal and stays nondet by default.
%But when the goal is built in place - (callPredicate (Predicate (g A1..An)))
%- its head is manifest at the call site, and an explicit determinism arrow
%DECLARED for g at that arity is a trusted promise about the foreign
%predicate, exactly like -[nondet]-> on get-type-space: no MeTTa clauses
%exist for it and the analysis cannot read Prolog, so the declaration is
%believed rather than validated (lib_builtin_types ships assertz/2 and
%erase/1; user code declares its own the same way). Not audited by
%--oracle-det, which wraps registered functions, not inner Prolog goals -
%the promise is the author's, as every foreign claim here is.
builtin_argument_rule_verdict(manifest_foreign_goal, _, [Arg], Det) :-
    nonvar(Arg), Arg = [P, Goal], P == 'Predicate',
    nonvar(Goal), Goal = [G|GArgs], atom(G), is_list(GArgs),
    length(GArgs, N),
    explicit_committed_decl(G, N, Det).
%The same reasoning, unlocked at a call site whose argument is not a manifest
%literal but a DIRECT parameter under an explicit committed arrow whose declared
%type is NOMINAL: every value of a nominal type is a constructor application, a
%nonempty list spine at runtime, so add_sexp/remove_sexp's =.. succeeds exactly
%once. The boundary check supplies the one fact the manifest-literal entries got
%from the source - that the argument is bound - which is why this is sound now
%and was not when the argument could arrive unbound out of well-typed code.
builtin_argument_rule_verdict(space_update, _, [_, T], det) :-
    enforced_bound_nominal(T).

%--- Strengthened by a bound probe against a ground, duplicate-free literal.
%is-member(X, L) is member(X, L) ; \+ member(X, L) - two mutually exclusive
%clauses. With X BOUND (an enforced-bound direct param, or a ground literal)
%and L a GROUND, DUPLICATE-FREE proper list, member/2 is a test that succeeds
%at most once, so exactly one of the two clauses yields exactly one solution:
%det. A duplicate in L would let clause one succeed twice, and an unbound X
%would let it enumerate - hence both conditions. The runtime predicate is left
%untouched: its generator mode is load-bearing (examples/functionhead3.metta).
builtin_argument_rule_verdict(bound_membership_probe, _, [Probe, L], det) :-
    is_member_probe_bound(Probe),
    manifest_ground_dupfree_list(L).

%--- Strengthened by manifestly bound boolean operands.
%and/2, or/2, not/1, xor/2 and implies/2 are nondet because bool/1 INVENTS a
%boolean it was not given. Where every operand is manifestly one already, that
%generator is a test and the if-then-else below it yields exactly one answer.
builtin_argument_rule_verdict(manifest_booleans, _, Args, det) :-
    maplist(manifest_bool, Args).

%A source expression whose head is DATA rather than a function being applied.
%For a variable head this is the translator's own test (nonfunction_type/1 at
%translate_expr/3): anything else compiles to reduce/2 or apply_fn/3, i.e. an
%application whose result shape is not known here. An untyped variable head is
%therefore conservatively read as a call.
data_headed(H) :- var(H), !, known_singleton(H, K), nonfunction_type(K).
data_headed(H) :- atom(H), !, \+ fun(H).
%A COMPOUND head is itself an expression, and whether the whole thing is data
%follows the same rule one level down: ((c) 1 2) with c a declared constant is
%a nested data literal, but ((foo) 2 3) with foo a function is compiled as an
%application (apply_fn/reduce, translate_expr/3) whose result is whatever the
%applied closure returns - possibly (), possibly open - and no part of its
%spine is built at this call site. The old catch-all read every compound head
%as data, which strengthened min-atom to det over an application's result.
%A fun-headed compound is still a DATA head when the function's unique
%declared output type is a non-function type: ((identity-number $a) $b $c)
%evaluates its head to a Number, and a Number cannot dispatch - the
%translator compiles the spine as data for exactly that reason
%(nonfunction_type at translate_expr/3), so the manifest judgement asks the
%same question. A call returning an arrow (or a wildcard, which admits
%function symbols) stays an application: ((foo) 2 3) with foo returning a
%closure is exactly the case this predicate exists to exclude.
data_headed(H) :- is_list(H), H = [F|Fargs], !,
                  ( data_headed(F) -> true
                  ; atom(F), fun(F), length(Fargs, N),
                    findall(OT, fn_decl_arity(F, N, _, OT), [OT1]),
                    nonvar(OT1), nonfunction_type(OT1) ).
data_headed(_).

%Manifestly a proper list: the literal (), a literal expression whose head is
%data, or a cons onto a manifestly proper tail. In every case the SPINE is
%built by the compiler at the call site, which is what makes it a fact rather
%than a claim.
%
%A DECLARED type never qualifies, not even a fixed-width tuple like
%(Number Number). That looks like it fixes the spine and does not: the residual
%guard is typecheck_or_error/2, which succeeds on an unbound variable, so a
%(Number Number) parameter can arrive unbound out of ordinary well-typed code
%- (B $u) leaves its field unfilled - and min_list/2 on an unbound argument
%answers once and then raises. That is exactly the assumption commit 6996b5b
%removed from the flat table, and re-admitting it here would put it back.
manifest_proper_list(X) :- X == [], !.
manifest_proper_list(X) :- var(X), !,
                           ( scoped_proper_list_var(X)
                           ; enforced_proper_list_value(X)
                           ; enforced_bound_tuple(X, _) ).
manifest_proper_list(X) :- is_list(X), X = [H|_], data_headed(H), !.
manifest_proper_list(X) :- nonvar(X), X = [C, _, Tl], ( C == cons ; C == 'cons-atom' ),
                           manifest_proper_list(Tl).
%FEATURE 2 - an output-properness certificate crosses the clause boundary a
%DECLARED (List _) type cannot. A call (G Arg...) to a function whose every
%clause provably RESULTS in a bound proper list (proper_list_output/2) is itself
%a bound proper list at this site - which a declared output type never proves,
%since a det function may still return an unbound variable. Nonempty is NOT
%implied (collapse can yield ()), so this lives ONLY here, never in
%manifest_nonempty_list/1. The cons/cons-atom heads are already handled above.
manifest_proper_list(X) :- nonvar(X), X = [G|GArgs], atom(G),
                           \+ ( G == cons ; G == 'cons-atom' ),
                           length(GArgs, N), proper_list_output(G, N).

manifest_nonempty_list(X) :- var(X), !, enforced_bound_tuple(X, W), W >= 1.
manifest_nonempty_list(X) :- is_list(X), X = [H|_], data_headed(H), !.
manifest_nonempty_list(X) :- nonvar(X), X = [C, _, Tl], ( C == cons ; C == 'cons-atom' ),
                             manifest_proper_list(Tl).

%Manifestly a bound boolean: a literal, or a call to a det builtin whose only
%declared output type is Bool - such a call cannot answer with an unbound
%result, because the predicate builds true/false itself ('>'/3 is a single
%if-then-else over a comparison, and raises rather than guessing).
manifest_bool(X) :- X == true, !.
manifest_bool(X) :- X == false, !.
%A DIRECT parameter, under an explicit committed arrow, whose declared type is
%Bool: the boundary check makes it bound at runtime and its type makes it a
%boolean, so and/or/not/xor/implies find a value to test rather than a hole to
%enumerate. A Bool-typed destructured FIELD does NOT qualify - enforced_bound_param/1
%tests direct params only, and a field is skipped by the spine-level boundary check.
manifest_bool(X) :- var(X), !, enforced_bound_param(X), known_singleton(X, 'Bool').
%The determinism asked for is the CALL-SITE verdict, not the flat table's
%worst case: (not X) is nondet in the table (bool/1 generates), but with X
%itself a manifest bool the argument-aware entry makes it det - and a det
%sole-Bool-output call delivers a bound boolean. The mutual recursion with
%builtin_call_determinism_args (its boolean entries test manifest_bool on
%their operands) is well-founded: operands are strict subterms.
manifest_bool([F|As]) :- atom(F), is_list(As), length(As, N),
                         ( builtin_call_determinism_args(F, N, As, det) -> true
                         ; builtin_call_determinism(F, N, det) ),
                         findall(OT, fn_decl_arity(F, N, _, OT), [OT1]), OT1 == 'Bool'.
%A USER function call whose bound_bool certificate holds: every clause of it
%provably results in a bound boolean, so the call cannot deliver the unbound
%hole bool/1 would enumerate. The certificate, not the declaration, is what
%qualifies - a declared Bool output can still return an unbound value:
manifest_bool([F|As]) :- atom(F), is_list(As), length(As, N),
                         bool_output(F, N).

%A bound is-member probe: a ground literal, or an enforced-bound direct param
%(any type - only boundness matters, since the probe is a test operand):
is_member_probe_bound(P) :- ground_data(P), !.
is_member_probe_bound(P) :- var(P), enforced_bound_param(P).

%A ground literal list that is duplicate-free. sort/2 dedups and orders; equal
%length to msort/2 (which keeps duplicates) means no dup:
manifest_ground_dupfree_list(L) :- is_list(L), ground_data(L),
                                   sort(L, S), msort(L, M), length(S, K), length(M, K).

%A ground term that translation keeps as data all the way down, so its runtime
%value is the source term itself. A call anywhere inside breaks that: (f),
%(cons 1 (1)) and (1 (f)) can all evaluate to a list with a duplicate, and
%(f) can evaluate to an unbound probe.
ground_data(X) :- atomic(X), !.
ground_data(X) :- compound(X), selection_transparent_actual(X), maplist(ground_data, X).

%%% FEATURE 2 - output-properness certificate %%%
%
%proper_list_output(F, N) holds when EVERY clause of F/N provably yields a bound
%proper list. It is derived per-clause during translation (update_output_certs/3,
%called from translate_clause): a clause QUALIFIES when its result expression is a
%collapse form (findall/3 always yields a bound proper list), a literal proper-list
%spine, or - same file only - a call to an already-certified function. The certificate
%is "ALL stored clauses qualify", tracked by the simplest sound bookkeeping: one
%fact recorded on the first qualifying clause, and a STICKY disqualification set the
%moment any clause fails to qualify (which also withdraws the fact). A function with
%no clause yet, or one poisoned clause, is not certified.
%
%INVALIDATION: a late clause of a certified F can break the certificate. Both
%the certificate memo and every compiled proof that consumed it record
%output_cert(Kind,F/N); notify_mutation/1 invalidates and recompiles that
%transitive consumer set.
%The store is parameterized by KIND - the same "every clause's result
%provably has this shape" question serves both proper_list (a bound proper
%list: collapse, literal spine, certified call) and bound_bool (a bound
%boolean: literal, det Bool builtin, certified call, an if/case whose every
%branch qualifies). Adding a certificate kind is one
%output_result_qualifies/2 clause.
%
%Derivation is DEMAND-DRIVEN with a COINDUCTIVE cycle assumption, exactly
%body_determinism/3's treatment of recursion: proving output_cert(K, F, N)
%checks every stored clause of F, and a recursive reference to a function
%already on the proof stack is assumed to hold. Sound for these
%shape-of-every-output properties: a value actually produced at runtime
%traces a FINITE call tree, and induction on that tree grounds every leaf in
%a literal or builtin shape - so mutually recursive definitions
%(even-number?/odd-number?) certify, which the previous one-pass
%sticky-poison derivation could not. Failure under the optimistic assumption
%is definitive (assumptions only ever ADD successes), so a `no` is a real
%no. The memo is written only for OUTERMOST proofs (empty stack on entry):
%inner members of a cycle are re-derived when asked directly, which costs
%recomputation, never correctness.
output_cert(Kind, F, N) :-
    output_cert_proof(Kind, F, N, Proof),
    analysis_proof_verdict(Proof, yes),
    analysis_reemit_proof(Proof).

output_cert_proof(Kind, F, N, Proof) :-
    analysis_cache_lookup(output(Kind, F, N), Proof), !.
output_cert_proof(Kind, F, N, Proof) :-
    output_cert_core(Kind, F, N, [], Verdict, Dependencies),
    ( Verdict == yes -> CertEvents = [certificate(Kind, F/N)]
                      ; CertEvents = [] ),
    analysis_make_proof(output_cert(Kind, F/N), Verdict, CertEvents,
                        [output_cert(Kind, F/N)|Dependencies], Proof),
    analysis_cache_store(output(Kind, F, N), Proof).

%The coinductive proof stack is an explicit core input.  It used to be the
%$cert_stack b_setval scope; returning dependencies from the recursion makes
%the proof usable by the next phase without reconstructing that hidden stack.
output_cert_core(Kind, F, N, Stack, yes, [output_cert(Kind, F/N)]) :-
    memberchk(c(Kind, F, N), Stack), !.
output_cert_core(Kind, F, N, Stack, Verdict, Dependencies) :-
    atom(F),
    cert_clause_bodies(F, N, Bodies),
    ( Bodies == []
      -> Verdict = no,
         Dependencies = [clause_set(F/N)]
    ; output_bodies_verdict(Kind, Bodies, [c(Kind, F, N)|Stack],
                            Verdict, BodyDeps),
      append([clause_set(F/N), decl(F/N)], BodyDeps, Dependencies) ).

%Every clause body of F/N the prover can see: the compiled store, PLUS the
%current file's pending prepass bodies (filereader.pl) - a later definition's
%clauses are visible while an earlier one is validated, which is what lets
%mutually recursive functions certify in source order. During a load both
%stores may hold the same body; qualifying it twice is idempotent and every
%probe is non-binding.
:- dynamic pending_clause_body/4.   % pending_clause_body(File, F, N, Body)

cert_clause_bodies(F, N, Bodies) :-
    findall(B, ( catch(nb_getval(F, Ms), _, fail),
                 member(Meta, Ms),
                 fun_meta_parts(Meta, As, B, _),
                 length(As, N) ), Rs),
    current_metta_file(File),
    findall(B, pending_clause_body(File, F, N, B), Ps),
    append(Rs, Ps, Bodies).

output_bodies_verdict(_, [], _, yes, []).
output_bodies_verdict(Kind, [B|Bs], Stack, Verdict, Dependencies) :-
    output_result_qualifies_core(Kind, B, Stack, Here, HereDeps),
    output_bodies_verdict(Kind, Bs, Stack, Rest, RestDeps),
    ( Here == yes, Rest == yes -> Verdict = yes ; Verdict = no ),
    append(HereDeps, RestDeps, Dependencies).

proper_list_output(F, N) :- output_cert(proper_list, F, N).
%Compatibility projection from the unified function summary.  During a file
%batch this is the same IR analysis used for branch translation and body
%cardinality; the legacy certificate remains only for callers not yet routed
%through that batch boundary.
bool_output(F, N) :- unified_function_result_fact(F, N, proper_bool), !.
bool_output(F, N) :- output_cert(bound_bool, F, N).

%Internal symbol teardown uses this narrow cache operation. Ordinary clause
%mutations invalidate certificate producers and all recorded consumers through
%notify_mutation/1; no global output-certificate flush is needed.
reset_output_certs(F) :- analysis_cache_invalidate_outputs(F).

output_result_qualifies(Kind, Body) :-
    output_result_qualifies_core(Kind, Body, [], yes, _).

output_result_qualifies_core(proper_list, Body, Stack, Verdict, Dependencies) :-
    clause_result_proper_list_core(Body, Stack, Verdict, Dependencies).
output_result_qualifies_core(bound_bool, Body, Stack, Verdict, Dependencies) :-
    clause_result_bool_core(Body, Stack, Verdict, Dependencies).

clause_result_bool_core(Body, _, yes, []) :-
    ( Body == true ; Body == false ), !.
clause_result_bool_core(Body, Stack, Verdict, Dependencies) :-
    nonvar(Body), Body = [F|Args], atom(F), bool_logic_builtin(F), !,
    cert_bool_args(Args, Stack, Verdict, Dependencies).
clause_result_bool_core(Body, _, yes, [effect(F/N), decl(F/N)]) :-
    nonvar(Body), Body = [F|Args], atom(F), is_list(Args), length(Args, N),
    \+ bool_logic_builtin(F),
    ( builtin_call_determinism_args(F, N, Args, det)
    ; builtin_call_determinism(F, N, det) ),
    findall(OT, fn_decl_arity(F, N, _, OT), [OT1]), OT1 == 'Bool', !.
clause_result_bool_core(Body, Stack, Verdict, Dependencies) :-
    nonvar(Body), Body = [If, _, T, E], If == if, !,
    output_result_qualifies_core(bound_bool, T, Stack, TV, TD),
    output_result_qualifies_core(bound_bool, E, Stack, EV, ED),
    ( TV == yes, EV == yes -> Verdict = yes ; Verdict = no ),
    append(TD, ED, Dependencies).
clause_result_bool_core(Body, Stack, Verdict, Dependencies) :-
    nonvar(Body), Body = [If, _, T], If == if, !,
    output_result_qualifies_core(bound_bool, T, Stack, Verdict, Dependencies).
clause_result_bool_core(Body, Stack, Verdict, Dependencies) :-
    nonvar(Body), Body = [Let, _, _, In], Let == let, !,
    output_result_qualifies_core(bound_bool, In, Stack, Verdict, Dependencies).
clause_result_bool_core(Body, Stack, Verdict, Dependencies) :-
    nonvar(Body), Body = [Ls, _, In], Ls == 'let*', !,
    output_result_qualifies_core(bound_bool, In, Stack, Verdict, Dependencies).
clause_result_bool_core(Body, Stack, Verdict, Dependencies) :-
    nonvar(Body), Body = [G|GArgs], atom(G), is_list(GArgs),
    length(GArgs, N), !,
    output_cert_core(bound_bool, G, N, Stack, Verdict, Dependencies).
clause_result_bool_core(_, _, no, []).

bool_logic_builtin(and).
bool_logic_builtin(or).
bool_logic_builtin(not).
bool_logic_builtin(xor).
bool_logic_builtin(implies).

cert_bool_args([], _, yes, []).
cert_bool_args([A|As], Stack, Verdict, Dependencies) :-
    cert_bool_value(A, Stack, Here, HereDeps),
    cert_bool_args(As, Stack, Rest, RestDeps),
    ( Here == yes, Rest == yes -> Verdict = yes ; Verdict = no ),
    append(HereDeps, RestDeps, Dependencies).

cert_bool_value(A, _, yes, []) :- ( A == true ; A == false ), !.
cert_bool_value(A, Stack, Verdict, Dependencies) :-
    nonvar(A), A = [F|Args], atom(F), bool_logic_builtin(F), !,
    cert_bool_args(Args, Stack, Verdict, Dependencies).
cert_bool_value(A, _, yes, [effect(F/N), decl(F/N)]) :-
    nonvar(A), A = [F|Args], atom(F), is_list(Args), length(Args, N),
    \+ bool_logic_builtin(F),
    ( builtin_call_determinism_args(F, N, Args, det)
    ; builtin_call_determinism(F, N, det) ),
    findall(OT, fn_decl_arity(F, N, _, OT), [OT1]), OT1 == 'Bool', !.
cert_bool_value(A, Stack, Verdict, Dependencies) :-
    nonvar(A), A = [G|GArgs], atom(G), is_list(GArgs), !,
    length(GArgs, N),
    output_cert_core(bound_bool, G, N, Stack, Verdict, Dependencies).
cert_bool_value(_, _, no, []).

clause_result_proper_list_core(Body, _, yes, []) :-
    nonvar(Body), Body = [Hd|Rest], nonvar(Hd), Hd == collapse,
    Rest = [_], !.
clause_result_proper_list_core(Body, _, yes, []) :-
    nonvar(Body), Body = [F, _], F == list_to_set, !.
clause_result_proper_list_core(Body, _, yes, []) :-
    proper_list_literal_spine(Body), !.
clause_result_proper_list_core(Body, Stack, Verdict, Dependencies) :-
    nonvar(Body), Body = [G|GArgs], atom(G),
    \+ ( G == cons ; G == 'cons-atom' ),
    length(GArgs, N), !,
    output_cert_core(proper_list, G, N, Stack, Verdict, Dependencies).
clause_result_proper_list_core(_, _, no, []).

%A clause body whose RESULT is provably a bound boolean. manifest_bool/1
%covers the leaves - the true/false literals and a det builtin whose sole
%declared output is Bool ((== $values ()) is one) - and, like every result
%probe here, all tests are NON-BINDING (== on heads, nonvar guards): Body is
%the shared clause term the translator compiles next. The enforced-param
%clause of manifest_bool cannot fire here (the analysis scope is not open at
%derivation time), which only costs precision, never soundness.
clause_result_bool(Body) :- manifest_bool(Body), !.
clause_result_bool(Body) :- nonvar(Body), Body = [G|GArgs], atom(G), is_list(GArgs),
                            length(GArgs, N), bool_output(G, N), !.
clause_result_bool(Body) :- nonvar(Body), Body = [If, _, T, E], If == if, !,
                            clause_result_bool(T), clause_result_bool(E).
clause_result_bool(Body) :- nonvar(Body), Body = [If, _, T], If == if, !,
                            clause_result_bool(T).   %no else: no result, not an unbound one
clause_result_bool(Body) :- nonvar(Body), Body = [Let, _, _, In], Let == let, !,
                            clause_result_bool(In).
clause_result_bool(Body) :- nonvar(Body), Body = [Ls, _, In], Ls == 'let*', !,
                            clause_result_bool(In).

%A clause body whose RESULT is provably a bound proper list. Every test here is
%NON-BINDING (nonvar guards + ==): Body is the SHARED clause body term that
%translate_expr/3 compiles next, so unifying a pattern into it - e.g. matching a
%var-headed application ($f $x) against [collapse, _] - would bind the clause's
%own variables and corrupt the compile.
clause_result_proper_list(Body) :- nonvar(Body), Body = [Hd|Rest], nonvar(Hd), Hd == collapse,
                                   Rest = [_], !.
%SWI list_to_set/2 always constructs a closed output list whenever it returns;
%the input may affect success/error behavior, never the result spine.
clause_result_proper_list(Body) :- nonvar(Body), Body = [F, _],
                                   F == list_to_set, !.
clause_result_proper_list(Body) :- proper_list_literal_spine(Body), !.
%recursive, same-file only: a call to an already-certified function. A data
%atom head is handled by proper_list_literal_spine above (data_headed), so this
%reaches only a genuine function application:
clause_result_proper_list(Body) :- nonvar(Body), Body = [G|GArgs], atom(G),
                                   \+ ( G == cons ; G == 'cons-atom' ),
                                   length(GArgs, N), proper_list_output(G, N).

%The same recognizer serves let_determinism/4: a let value of one of these
%shapes guarantees the bound variable holds a proper list, which is what the
%(== $v ()) nonemptiness narrowing needs to fire on a let-introduced var:
val_guaranteed_proper_list(Val) :- clause_result_proper_list(Val).

%A literal proper-list spine, built at the clause site: the empty list, a
%data-headed list literal, or a cons onto a literal spine. Mirrors
%manifest_proper_list's literal clauses without the var/enforced-tuple case -
%a parameter is not a literal the clause builds.
proper_list_literal_spine(X) :- X == [], !.
proper_list_literal_spine(X) :- is_list(X), X = [H|_], data_headed(H), !.
proper_list_literal_spine(X) :- nonvar(X), X = [C, _, Tl], ( C == cons ; C == 'cons-atom' ),
                                proper_list_literal_spine(Tl).

%A DIRECT parameter, under an explicit committed arrow, whose declared type is
%NOMINAL (a declared, non-wildcard, non-primitive atom type). Its values are
%constructor applications - nonempty list spines at runtime - so remove/add-atom
%is det. The boundary check removes the unbound-arrival case; the declaration
%makes the type nominal - PROVIDED no inhabitant is a bare atom: a nullary
%constructor or declared constant ((: left Left)) is an atom value, not a
%list spine, and remove_sexp's =.. FAILS on it - zero solutions under a det
%claim. The judgement publishes ctor_set(K), so a constant declared in a later
%file recompiles this graph consumer and withdraws the strengthening.
enforced_bound_nominal(T) :- var(T), enforced_bound_param(T),
                             known_singleton(T, K), atom(K),
                             user_atom_type(K), type_name_declared(K),
                             analysis_emit(dependency(ctor_set(K))),
                             \+ nominal_nullary_inhabitant(K).

%An inhabitant of K that is a bare atom at runtime: a declared constant of
%type K, or a nullary constructor (-> K):
nominal_nullary_inhabitant(K) :- declared_value_type(C, K2), K2 == K, atom(C), \+ fun(C), !.
nominal_nullary_inhabitant(K) :- member_ctor(K, 0, _).

%A DIRECT parameter, under an explicit committed arrow, whose declared type is a
%fixed-width positional tuple (tagged_tuple_type/3's positional reading: a list
%of concrete types whose value is an N-element list, N>=1 - the tagged and
%untagged readings agree on width = length of the type list). The value is
%therefore a proper, nonempty list. This re-admits the (Number Number) min-atom
%case that 28e87bd removed: its ONLY objection was unbound arrival, and the
%boundary check removes exactly that, so the case is sound under enforcement.
enforced_bound_tuple(X, W) :- var(X), enforced_bound_param(X),
                              known_singleton(X, K),
                              is_list(K), \+ special_compound_type(K),
                              length(K, W), W >= 1.

%A deterministic caller needs positive evidence about its callees. Functions
%without a determinism arrow are analyzed from their translated clauses
%(bodies deterministic, heads non-overlapping), memoized, and treated as det
%on cycles (a recursive call cannot introduce what the rest disproves).
%A registered symbol with no MeTTa clauses is a Prolog builtin, and the only
%thing known about it is what builtin_call_determinism/3 records: everything
%else is `unspecified`. Assuming det for a predicate nobody analysed is the
%strongest claim available about the least visible code in the system, and it
%certified -[det]-> functions whose bodies backtrack (see (get-atoms ...)).
body_determinism(F, N, Det) :-
    body_determinism_proof(F, N, Proof),
    analysis_proof_verdict(Proof, Det),
    analysis_reemit_proof(Proof).

body_determinism_proof(F, N, Proof) :-
    analysis_cache_lookup(det(F, N), Proof), !.
body_determinism_proof(F, N, Proof) :-
    catch(b_getval('$det_stack', St), _, St = []),
    memberchk(F/N, St), !,
    analysis_make_proof(body(F/N), det, [],
                        [effect(F/N), clause_set(F/N)], Proof).
body_determinism_proof(F, N, Proof) :-
    catch(nb_getval(F, Metas0), _, Metas0 = []),
    include(arity_meta(N), Metas0, Metas),
    ( Metas == []
      -> ( builtin_call_determinism(F, N, Det0)
           -> Det = Det0 ; Det = unspecified ),
         analysis_make_proof(body(F/N), Det, [],
                             [effect(F/N), decl(F/N)], Proof)
    ; catch(b_getval('$det_stack', St), _, St = []),
      setup_call_cleanup(
          b_setval('$det_stack', [F/N|St]),
          with_compiling_caller(F, N,
          ( type_meta_params(F, N, Metas, Metas1),
            det_enforced_flag(F, N, Enf),
            with_det_enforced(Enf,
                clause_set_determinism_proof(Metas1, ClauseProof)),
            analysis_proof_verdict(ClauseProof, Det),
            analysis_proof_requirements(ClauseProof, Bounds),
            analysis_proof_certificates(ClauseProof, Certs),
            analysis_proof_dependencies(ClauseProof, ClauseDeps),
            analysis_term_dependencies(Metas1, TermDeps),
            append([[effect(F/N), decl(F/N), clause_set(F/N)],
                    ClauseDeps, TermDeps], Ds0),
            sort(Ds0, Deps),
            Proof = analysis_proof(body(F/N), Det,
                                   requirements(Bounds),
                                   certificates(Certs),
                                   dependencies(Deps)) )),
          b_setval('$det_stack', St)),
      analysis_cache_store(det(F, N), Proof) ).

%A stored clause meta is captured before clause_param_types binds the
%declared arg types onto the head param vars, so those vars carry no type
%attribute. The unconditional own-body check (validate_function_determinism)
%analyzes the ACTUAL body whose vars ARE typed, but a transitive analysis of
%a callee reads only its untyped stored metas - and a data parameter then
%looks like a function of unknown determinism. In particular a var-headed
%tuple ($x ...) built from a data parameter is misread as a dynamic call
%(unknown) instead of deterministic data construction, so pure list/record
%helpers analyze as unspecified and wrongly poison any -[det]-> caller.
%Attach the declared parameter types to a COPY of each meta (never the stored
%one), mirroring clause_param_types, so the transitive analysis agrees with
%the direct one. Arrow parameters keep their declared arrow - a plain -> stays
%unspecified in every mode but --strict-det, exactly as before:
type_meta_params(F, N, Metas, Metas1) :- ( findall(ATs, fn_decl_arity(F, N, ATs, _), [ATs1])
                                           -> maplist(type_one_meta(ATs1), Metas, Metas1)
                                            ; Metas1 = Metas ).

type_one_meta(ATs1, Meta, Meta2) :- copy_term(Meta, Meta2),
                                    fun_meta_parts(Meta2, Args, _, _),
                                    maplist(bind_meta_param, Args, ATs1).

bind_meta_param(Arg, T) :- ignore(catch(bind_pattern_typed(Arg, T), _, true)).

arity_meta(N, Meta) :- fun_meta_parts(Meta, Args, _, _), length(Args, N).

%The worst verdict over ALL clause bodies decides (a may_fail clause followed
%by a nondeterministic one is nondet, not semidet), and overlapping heads
%multiply results whatever the bodies say:
clause_set_determinism(Metas, Det) :-
    clause_set_determinism_proof(Metas, Proof),
    analysis_proof_verdict(Proof, Det),
    analysis_reemit_proof(Proof).

clause_set_determinism_proof(Metas, Proof) :-
    analysis_collect(clause_set_determinism_core(Metas, Det), Events),
    analysis_term_dependencies(Metas, Dependencies),
    analysis_make_proof(clause_set, Det, Events, Dependencies, Proof).

clause_set_determinism_core(Metas, Det) :- clause_bodies_determinism(Metas, R),
                                           ( R = nondeterministic(_) -> Det = nondet
                                           ; R = unknown(_) -> Det = unspecified
                                           ; overlapping_meta_pair(Metas) -> Det = nondet
                                           ; R = may_fail(_) -> Det = semidet
                                           ; Det = det ).

%The metas are walked directly (never through findall) so the parameter type
%attributes type_meta_params/4 attached to the head vars stay visible in the
%bodies they are shared with:
clause_bodies_determinism([], ok).
%Each meta's Args are published as the head-variable set for its body's
%analysis (see with_det_head_vars/2): a wildcard-typed parameter has no type
%attribute, so identity against the head is what tells it from a fresh local.
clause_bodies_determinism([Meta|Ms], R) :-
                                                     fun_meta_parts(Meta, Args, B, _),
                                                     with_det_head_vars(Args, B, deterministic_expr_core(B, R1)),
                                                     ( det_result_final(R1) -> R = R1
                                                     ; clause_bodies_determinism(Ms, R2),
                                                       combine_det_results(R1, R2, R) ).

overlapping_meta_pair(Metas) :- append(_, [Meta1|Rest], Metas),
                                fun_meta_parts(Meta1, A1, _, _),
                                member(Meta2, Rest),
                                fun_meta_parts(Meta2, A2, B2, _),
                                clause_heads_overlap(A1, A2),
                                \+ body_commits(B2),
                                \+ body_conditionally_commits(B2).

%Public compatibility wrapper.  The functional core returns its full evidence;
%legacy callers that only need the detailed cardinality verdict keep /2.
deterministic_expr(Expr, Result) :-
    deterministic_expr_proof(Expr, Proof),
    analysis_proof_verdict(Proof, Result),
    analysis_reemit_proof(Proof).

deterministic_expr_proof(Expr, Proof) :-
    analysis_collect(deterministic_expr_core(Expr, Result), Events),
    analysis_term_dependencies(Expr, Dependencies),
    analysis_make_proof(expr(Expr), Result, Events, Dependencies, Proof).

deterministic_expr_core(Expr, ok) :- ( var(Expr) ; atomic(Expr) ; Expr = partial(_, _) ), !.
%A variable head must not unify with the construct patterns below. An
%explicit -[det]-> arrow (or nonfunction data type) on the head is det
%evidence in every mode. A plain arrow never proves a commitment:
deterministic_expr_core([Head|Args], Result) :- var(Head), !,
    ( Args == [] -> Result = ok                    %singleton ($x) is data, not application
    ; known_singleton(Head, K), nonvar(K)
      -> ( arrow_head_level(K, det) -> combine_determinism_list(Args, Result)
         ; arrow_head_level(K, semidet)
           -> combine_determinism_list(Args, R0),
              combine_det_results(may_fail(semidet_closure), R0, Result)
         ; arrow_head_level(K, nondet) -> Result = nondeterministic(nondet_closure)
         %non-arrow head: data construction - but NOT through a wildcard.
         %Atom/%Undefined%/Expression admit function symbols, and a var of
         %such a type bound to one at runtime makes reduce/2 DISPATCH the
         %"data" this analysis said it was building:
         ; \+ is_arrow_type(K), \+ wildcard_type_t(K) -> combine_determinism_list(Args, Result)
         ; Result = unknown(dynamic_head(Head)) )
       ; Result = unknown(dynamic_head(Head)) ).
deterministic_expr_core([collapse, _], ok) :- !.
deterministic_expr_core(['trace!', A, B], Result) :- !, combine_determinism_list([A, B], Result).
deterministic_expr_core([once, Expr], Result) :- !, once_determinism(Expr, Result).
deterministic_expr_core([quote, _], ok) :- !.
%`data` explicitly constructs an expression. Its first argument is a field,
%not a dynamic call target; only evaluations nested in the fields contribute
%to determinism.
deterministic_expr_core([data|Fields], Result) :- !, combine_determinism_list(Fields, Result).
%`make-list` has the same non-callable-head discipline as data: only its
%element evaluations contribute to determinism.
deterministic_expr_core(['make-list'|Elements], Result) :- !, combine_determinism_list(Elements, Result).
deterministic_expr_core([eval, _], unknown(dynamic_eval)) :- !.
deterministic_expr_core([reduce, _], unknown(dynamic_reduce)) :- !.
deterministic_expr_core([call, Expr], Result) :- !, deterministic_call_expr(Expr, Result).
deterministic_expr_core([superpose|_], nondeterministic(superpose)) :- !.
deterministic_expr_core([match|_], nondeterministic(match)) :- !.
deterministic_expr_core([hyperpose|_], nondeterministic(hyperpose)) :- !.
deterministic_expr_core([translatePredicate|_], nondeterministic(translatePredicate)) :- !.
%The structural (dis)equality tests unify their operands as DATA. A var-headed
%operand like ($name $index $vars) is a PATTERN built and unified, never
%dispatched: reduce/2 leaves a var-headed term unevaluated (src/translator.pl),
%so it contributes no dynamic-head uncertainty and the whole test stays det.
%A FUN-headed operand IS evaluated first and still contributes its determinism,
%so the reading only differs from the generic call path at a variable head -
%exactly the pattern case the honest dynamic_head verdict is too weak for.
deterministic_expr_core([Op, A, B], Result) :- unify_test_op(Op), !,
                                          unify_operand_determinism(A, RA),
                                          unify_operand_determinism(B, RB),
                                          combine_det_results(RA, RB, Result).
%A two-argument if has no else branch: when the condition is false the whole
%expression produces NOTHING. That is a failure path of the construct itself,
%invisible to an analysis of the parts, so it is may_fail unconditionally -
%-[semidet]-> accepts it, -[det]-> does not:
deterministic_expr_core([if, Cond, Then], Result) :- !, combine_determinism_list([Cond, Then], R0),
                                                combine_det_results(may_fail(if_without_else), R0, Result).
%A successful is-expr test proves that its variable is a bound, nonempty,
%proper expression spine in the then branch. This is selection evidence, not
%a declaration: it is scoped to that branch.
deterministic_expr_core([if, Cond, Then, Else], Result) :-
    expression_spine_narrowing_var(Cond, V), !,
    deterministic_expr_core(Cond, RC),
    with_scoped_proper_list_var(
        proper(V),
        with_nonempty_var(V, deterministic_expr_core(Then, RT))),
    deterministic_expr_core(Else, RE),
    combine_det_results(RC, RT, R01),
    combine_det_results(R01, RE, Result).
%FEATURE 1 - flow-sensitive nonemptiness. When the condition is (== V ()) or
%(== () V) with V a variable of known list type, the ELSE branch runs exactly
%when V is NONEMPTY, so a -[semidet]-> accessor whose only incompleteness is
%the empty case is det there. The narrowing is recorded for V while the Else
%branch is analysed (semidet_site_upgraded_to_det/3 reads it) and restored
%after; Cond and Then get no narrowing. Nothing here can REJECT: a
%non-matching condition falls through to the plain worst-of composition below.
deterministic_expr_core([if, Cond, Then, Else], Result) :- nonempty_narrowing_var(Cond, V), !,
                                                      deterministic_expr_core(Cond, RC),
                                                      deterministic_expr_core(Then, RT),
                                                      with_nonempty_var(V, deterministic_expr_core(Else, RE)),
                                                      combine_det_results(RC, RT, R01),
                                                      combine_det_results(R01, RE, Result).
deterministic_expr_core([if, Cond, Then, Else], Result) :- !, combine_determinism_list([Cond, Then, Else], Result).
deterministic_expr_core([progn|Exprs], Result) :- !, combine_determinism_list(Exprs, Result).
deterministic_expr_core([prog1|Exprs], Result) :- !, combine_determinism_list(Exprs, Result).
deterministic_expr_core([let, Pat, Val, In], Result) :- !, let_determinism(Pat, Val, In, Result).
deterministic_expr_core([chain, Pat, Val, In], Result) :- !, let_determinism(Pat, Val, In, Result).
deterministic_expr_core(['let*', Binds, Body], Result) :- !, binds_and_body_determinism(Binds, Body, Result).
deterministic_expr_core([sealed, _, Expr], Result) :- !, deterministic_expr_core(Expr, Result).
deterministic_expr_core(['forall', _, _], ok) :- !.
deterministic_expr_core(['foldall', _, _, _], ok) :- !.
%The three higher-order builtins exist in TWO forms, and both are live.
%
%  * The pseudo-lambda form the translator rewrites inline (src/translator.pl):
%    (foldl-atom List Init $acc $x Body), (map-atom List $x Body),
%    (filter-atom List $x Cond). The element variable is a binder, so the
%    determinism is that of the list and of the inlined body.
%  * The CLOSURE form, which is what src/metta.pl actually defines
%    ('map-atom'/3, 'foldl-atom'/4, 'filter-atom'/3 = 2/3/2 MeTTa arguments
%    plus the result): (map-atom List F), (foldl-atom List Init F),
%    (filter-atom List F). Here the determinism is the CLOSURE argument's -
%    the predicate calls reduce/2 on it once per element - so the closure has
%    to carry det evidence exactly as it does for a user-written fold
%    (det_arg_evidence/2, the caller-side discharge used by
%    det_closure_args_ok/3).
%
%Only the first form had clauses, so every closure-form call fell through to
%deterministic_call_expr/2, where the three are deliberately unlisted in
%builtin_call_determinism/3 ("their determinism is that of the closure they
%are given, which this table cannot express") and therefore `unspecified`.
%That is what rejected lib_he's for-each-in-atom under --strict-det.
%
%All six also require the list input to be manifestly proper. A direct
%committed parameter can establish that condition through a proper-list
%boundary proviso; an open or partial list remains unprovable and cannot be
%used to certify the traversal.
deterministic_expr_core(['foldl-atom', List, Init, _, _, Body], Result) :- !,
    list_builtin_determinism('foldl-atom', List, [List, Init, Body], Result).
deterministic_expr_core(['map-atom', List, _, Body], Result) :- !,
    list_builtin_determinism('map-atom', List, [List, Body], Result).
deterministic_expr_core(['filter-atom', List, _, Cond], Result) :- !,
    list_builtin_determinism('filter-atom', List, [List, Cond], Result).
deterministic_expr_core(['foldl-atom', List, Init, F], Result) :- !,
    closure_builtin_determinism('foldl-atom', List, F, 2, [List, Init], Result).
deterministic_expr_core(['map-atom', List, F], Result) :- !,
    closure_builtin_determinism('map-atom', List, F, 1, [List], Result).
deterministic_expr_core(['filter-atom', List, F], Result) :- !,
    closure_builtin_determinism('filter-atom', List, F, 1, [List], Result).
deterministic_expr_core(['|->', _, _], ok) :- !.
deterministic_expr_core([case, KeyExpr, PairsExpr], Result) :- !, case_expr_determinism(KeyExpr, PairsExpr, Result).
deterministic_expr_core([Head|Args], Result) :- ( atomic(Head), ( \+ atom(Head) ; \+ fun(Head) )
                                           ; is_list(Head) ), !,
                                           combine_determinism_list([Head|Args], Result).
deterministic_expr_core(Expr, Result) :-
    Expr = [Head|_], atom(Head), !,
    % Keep the original source occurrence intact.  The unified IR origin map
    % is identity based for nonground terms, so rebuilding [Head|Args] here
    % would discard the exact call-site proof.
    deterministic_call_expr(Expr, Result).
deterministic_expr_core([Head|_], unknown(dynamic_head(Head))).

%(map-atom L F), (foldl-atom L Init F) and (filter-atom L F): the data
%arguments contribute their own determinism, and the closure has to prove
%itself exactly as it does at a user-written higher-order call site.
list_builtin_determinism(Name, List, Exprs, Result) :-
    ( manifest_proper_list(List) -> combine_determinism_list(Exprs, Result)
                                 ; Result = unknown(open_list(Name, List)) ).

closure_builtin_determinism(Name, List, F, M, DataArgs, Result) :-
    ( manifest_proper_list(List)
      -> ( det_arg_evidence(F, M) -> combine_determinism_list(DataArgs, Result)
         ; Result = unknown(undetermined_closure(Name, F)) )
    ; Result = unknown(open_list(Name, List)) ).

deterministic_call_expr(Expr, Result) :-
    Expr = [Fun|Args], atom(Fun), !,
    length(Args, N),
    evaluated_call_args(Fun, Args, EArgs),
    ( unified_builtin_call_card(Expr, card(1,1))
      -> % The analyzer proved the builtin mode at this flow-sensitive call
         % site.  Retain the legacy argument walk: it supplies detailed
         % diagnostics and prevents a declared user-call contract nested in
         % an argument from becoming a circular determinism proof.
         combine_determinism_list(EArgs, Result)
    ; call_site_determinism(Fun, N, Args, Det),
      ( Det == nondet -> Result = nondeterministic(call(Fun))
      ; Det == semidet, semidet_site_upgraded_to_det(Fun, N, Args)
        -> combine_determinism_list(EArgs, Result)
      ; Det == semidet
        -> combine_determinism_list(EArgs, R0),
           combine_det_results(may_fail(call(Fun)), R0, Result)
      ; Det == det -> combine_determinism_list(EArgs, Result)
      ; det_closure_args_ok(Fun, N, Args),
        body_determinism_assuming(Fun, N, det)
        -> combine_determinism_list(EArgs, Result)
      ; underapplied_closure(Fun, N)
        -> combine_determinism_list(EArgs, Result)
      ; Result = unknown(undetermined_call(Fun)) ) ).
deterministic_call_expr(Expr, unknown(dynamic_call(Expr))).

%The arguments a call evaluates. add-atom and remove-atom store their payload
%raw (their typed_space_update lowering never reduces it, on typed and untyped
%spaces alike), so the payload is data whatever its head and contributes no
%determinism of its own.
evaluated_call_args(Fun, [Space, _Payload], [Space]) :-
    special_builtin_form(Fun, [Space, _], typed_space_update), !.
evaluated_call_args(_, Args, Args).

%Structural equality/unification builtins whose operands are DATA PATTERNS.
%These take their arguments unevaluated-as-data (a var head is unified, not
%dispatched), so a var-headed operand does not make the test unknown.
unify_test_op('=').
unify_test_op('=?').
unify_test_op('=alpha').
unify_test_op('=@=').

%Determinism of one operand of a structural test. A var-headed compound is a
%unification pattern: its head is data (never dispatched) and its arguments are
%themselves operands, so the pattern is det unless a nested fun-headed sub-call
%is not. A fun-headed (or atomic/var) operand is read exactly as anywhere else.
unify_operand_determinism(E, ok) :- ( var(E) ; atomic(E) ), !.
unify_operand_determinism([H|Args], R) :- var(H), unify_head_is_data(H), !,
                                          combine_unify_operands(Args, R).
unify_operand_determinism(E, R) :- deterministic_expr_core(E, R).

%A var head is data only when it provably CANNOT be a function at the reduce
%that builds the term. Two proofs exist: its known type rules functions out
%(non-arrow and non-wildcard - Atom/%Undefined%/Expression admit function
%symbols), or it is a fresh local: not a HEAD variable of the clause under
%analysis (det_head_var/1 - identity, because a wildcard-typed parameter
%carries no type attribute at all) and carrying no knowledge of any kind (a
%let/chain field is marked by let_determinism/4 exactly so it fails this
%test). A fresh local is bound by nothing when reduce/2 reaches it, and an
%unevaluated var-headed term is one solution of data. Without this test,
%(= 1 ($f 0)) with $f an Atom-typed parameter read as data while the compiled
%reduce dispatched whatever function the caller passed.
unify_head_is_data(H) :- ( known_singleton(H, K), nonvar(K)
                           -> \+ is_arrow_type(K), \+ wildcard_type_t(K)
                         ; det_head_var(H) -> fail
                         ; \+ get_attr(H, tknown, _) ).

combine_unify_operands([], ok).
combine_unify_operands([A|As], R) :- unify_operand_determinism(A, R1),
                                     combine_unify_operands(As, R2),
                                     combine_det_results(R1, R2, R).
