%%% Determinism proof rules and expression walker: argument-sensitive builtin
%%% verdicts, manifest-shape and output certificates, closure cardinality, and
%%% the clause/body/expression determinism proofs. Procedural registry hooks
%%% stay here; which builtin uses one is recorded in builtin_registry.pl.

%The registry's flat cardinality is the checker's own knowledge of a builtin
%and outranks any declaration of the same symbol, at a direct call, for a
%value-position arrow head (value_arrow_head/4) and for the oracle's wrapping
%decision (oracle_det_believed/3). The atom(F) guard keeps an unbound F from
%enumerating the table.
table_det_verdict(F, N, Det) :- atom(F), builtin_flat_cardinality(F, N, Det).

table_det_override(F, N, Fallback, Det) :- ( table_det_verdict(F, N, DetB) -> Det = DetB ; Det = Fallback ).

function_call_determinism(F, N, Det) :- table_det_verdict(F, N, Det), !.
function_call_determinism(F, N, Det) :- catch(fn_determinism(F, N, Det0), _, fail),
                                        Det0 \== unspecified, !, Det = Det0.
function_call_determinism(F, N, Det) :-
    ( inferred_unknown_call_determinism(F, N, Inferred)
      -> Det = Inferred
    ; Det = unspecified ).

%The flat cardinality is keyed on (name, arity), so one worst-case verdict
%covers every call site. Most weak verdicts are weak for a SHAPE reason (an
%unbound argument, an open list) that a call site whose shape is manifest in
%the source rules out, so builtin_call_determinism_args/4 is consulted first
%and only ever returns a verdict at least as strong. The judgements are about
%the spine, never the elements: an exception is not a solution.
call_site_determinism(F, N, Args, Det) :-
    call_site_base_determinism(F, N, Args, Base),
    ( trusted_unverified_call(F, Args)
      -> effect_join(Base, semidet, Det)
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
    fun_metas(F, N, Metas),
    body_determinism(F, N, BodyDet),
    inferred_selection_determinism(F, N, Args, Metas, SelectionDet),
    effect_join(BodyDet, SelectionDet, Det).

%A named function value has no call site to learn boundness from, so it needs
%a proof over the whole input domain; one probe with fresh variables would make
%a single partial clause look exactly-one.
inferred_unknown_call_determinism(F, N, Det) :-
    fun_metas(F, N, Metas),
    body_determinism(F, N, BodyDet),
    inferred_total_selection_determinism(F, N, Metas, SelectionDet),
    effect_join(BodyDet, SelectionDet, Det).

%First exploit values whose applicability is already decidable at the source
%call site. Otherwise a multi-clause relation is at most-one only when one
%bound argument position carries distinct top-level head keys. It is
%exactly-one when those keys also cover that argument's known domain.
inferred_selection_determinism(F, N, Args, Metas, Det) :-
    ( member(MetaWithGoals, Metas), MetaWithGoals = fun_meta(_, _, head_goals)
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
    ; keyed_head_column(Metas, Idx, Keys),
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
    Meta = fun_meta(HeadArgs, _, _),
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
    member(Meta, Metas), Meta = fun_meta(_, _, head_goals), !.
inferred_total_selection_determinism(_, _, Metas, nondet) :-
    selection_heads_overlap(Metas), !.
inferred_total_selection_determinism(F, N, Metas, det) :-
    total_selection_heads(F, N, Metas), !.
inferred_total_selection_determinism(_, _, _, semidet).

selection_heads_overlap(Metas) :-
    append(_, [M1|Rest], Metas),
    M1 = fun_meta(A1, _, _),
    member(M2, Rest),
    M2 = fun_meta(A2, _, _),
    clause_heads_overlap(A1, A2), !.

total_selection_heads(_, _, [Meta]) :-
    Meta = fun_meta(Args, _, _),
    maplist(var, Args), !.
total_selection_heads(F, N, Metas) :-
    keyed_head_column(Metas, Idx, Keys),
    all_other_head_positions_unconstrained(Metas, Idx),
    selection_function_arg_type(F, N, Idx, T),
    keys_cover_domain(T, Keys).

%A key column gives a unique clause selector only when every clause exposes a
%key there and no key is repeated. Repeated/nested discriminators stay
%conservative because a merely nonvar boundary does not ground their fields.
keyed_head_column(Metas, Idx, Keys) :-
    Metas = [First|_],
    First = fun_meta(Args, _, _),
    nth0(Idx, Args, _),
    findall(P, (member(Meta, Metas),
                Meta = fun_meta(HArgs, _, _),
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
             Meta = fun_meta(Args, _, _),
             nth0(J, Args, P), J =\= Idx ),
           var(P)).

selection_function_arg_type(F, N, Idx, T) :-
    unique_fn_decl(F, N, ATs, _), !,
    nth0(Idx, ATs, T).
selection_function_arg_type(F, N, Idx, T) :-
    findall(ATs, (inferred_fn_type(F, ATs, _), length(ATs, N)), [ATs]),
    nth0(Idx, ATs, T).

call_head_status(Args, Meta, Status) :-
    Meta = fun_meta(HeadArgs, _, _),
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

%A known selector type is selection evidence only for a direct parameter of
%the enclosing committed clause (enforced_proper_list_param/1,
%enforced_bound_param/1), where consuming it publishes a runtime boundary
%proviso. A local's or a nested head field's List/Bool type does not bind it.
selection_argument_bound(A, proper_list) :-
    var(A), scoped_proper_list_var(A), !.
selection_argument_bound(A, proper_list) :-
    var(A), known_singleton(A, T), nonvar(T), list_type(T, _),
    enforced_proper_list_param(A), !.
selection_argument_bound(A, proper_list) :-
    var(A), enforced_recursive_proper_list_value(A), !.
selection_argument_bound(A, nonvar) :-
    var(A), enforced_bound_param(A), !.
selection_argument_bound(A, proper_list) :-
    nonvar(A), selection_expression_certificate(A, proper_list), !.
selection_argument_bound(A, nonvar) :-
    nonvar(A), selection_expression_certificate(A, nonvar), !.
selection_argument_bound(A, proper_list) :-
    nonvar(A), manifest_proper_list(A), !.
selection_argument_bound(A, nonvar) :- nonvar(A).

%Consume the functional certificate proof directly so its output_cert
%dependencies enter the enclosing proof, which is invalidated if a producer
%later gains a non-certifying clause.
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

%Literal spines belong to manifest evidence, not to this evaluated-output path.
%Every accepted expression other than the two intrinsic producers must have
%consumed a named producer certificate, so compiler forms such as
%(data $tag 0) cannot masquerade as their source list syntax.
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

selection_column_covers(_, _, _, _, Keys, proper_list) :-
    sort([list_empty, list_cons], Domain),
    sort(Keys, Domain).
selection_column_covers(F, N, Args, Idx, Keys, _) :-
    selection_argument_type(F, N, Args, Idx, T),
    keys_cover_domain(T, Keys).

keys_cover_domain(T, Keys) :-
    domain_keys(T, [], Domain0),
    sort(Domain0, Domain),
    sort(Keys, Domain).

selection_argument_type(_, _, Args, Idx, T) :-
    nth0(Idx, Args, A), known_singleton(A, T0), nonvar(T0), !, T = T0.
selection_argument_type(F, N, _, Idx, T) :-
    unique_fn_decl(F, N, ATs, _),
    nth0(Idx, ATs, T).

%The registry owns WHICH builtins have argument-sensitive cardinality. This
%file owns only the irreducibly procedural meaning of each named rule.
builtin_call_determinism_args(F, N, Args, Det) :-
    builtin_argument_rule(F, N, Rule),
    builtin_argument_rule_verdict(Rule, F, Args, Det).

%A -[semidet]-> user-function call is det at a site narrowed to nonempty
%lists when (a) the callee's heads cover every nonempty list and (b) every
%body is may-not-fail with non-overlapping heads, which body_determinism/3
%certifies as det. Upgrade only, never a rejection; limited to arity-1
%callees with a unique declaration. The consumed effect/clause-set
%dependencies withdraw the upgrade when the callee's clauses change.
semidet_site_upgraded_to_det(Fun, N, Args) :-
    N =:= 1,
    nth0(Idx, Args, A), var(A), nonempty_var(A),
    known_singleton(A, T), nonvar(T), list_type(T, _),
    unique_fn_decl(Fun, N, _, _),
    fun_metas(Fun, N, Metas),
    nonempty_list_domain_covered(Metas, Idx),
    body_determinism(Fun, N, det).

%Every nonempty list is a cons cell, so one head matching every cons cell -
%a bare variable, or a cons of two unconstrained variables - covers the
%narrowed domain.
nonempty_list_domain_covered(Metas, Idx) :- member(Meta, Metas),
                                            Meta = fun_meta(HArgs, _, _),
                                            nth0(Idx, HArgs, P), covers_all_nonempty_lists(P), !.

covers_all_nonempty_lists(P) :- var(P), !.
covers_all_nonempty_lists(P) :- nonvar(P), P = [H|Tl], var(H), var(Tl).

%Variables proven nonempty on the current analysis path, scoped by b_setval
%and compared by identity (they are the shared body variables).
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

%add-atom/remove-atom are keyed semidet because add_sexp/remove_sexp's =..
%fails on a non-list atom argument. A manifest nonempty expression makes =..
%succeed, so exactly one clause commits. A declared type never qualifies (see
%manifest_proper_list/1); only a literal spine built at the call site does.
%The typed_space_update lowering passes the payload raw, so a literal
%expression is such a spine whatever its head: a variable head is stored, not
%applied.
builtin_argument_rule_verdict(space_update, _, [_, T], det) :-
    ( is_list(T), T = [_|_] -> true ; manifest_nonempty_list(T) ).
%callPredicate stays nondet, except when its goal (Predicate (g A1..An)) is
%built in place: an explicit arrow declared for g at that arity is then a
%trusted foreign promise, believed rather than validated (no MeTTa clauses
%exist to analyse), and not audited by --oracle-det.
builtin_argument_rule_verdict(manifest_foreign_goal, _, [Arg], Det) :-
    nonvar(Arg), Arg = [P, Goal], P == 'Predicate',
    nonvar(Goal), Goal = [G|GArgs], atom(G), is_list(GArgs),
    length(GArgs, N),
    explicit_committed_decl(G, N, Det).
%The same for a direct parameter under an explicit committed arrow whose type
%is nominal: every value is a constructor application, a nonempty spine, and
%the boundary check supplies the boundness the literal case reads off the
%source.
builtin_argument_rule_verdict(space_update, _, [_, T], det) :-
    enforced_bound_nominal(T).

%is-member(X, L) is member(X, L) ; \+ member(X, L). With X bound and L a
%ground duplicate-free proper list, member/2 succeeds at most once, so exactly
%one clause yields one solution. The runtime predicate keeps its generator
%mode, which examples/functionhead3.metta relies on.
builtin_argument_rule_verdict(bound_membership_probe, _, [Probe, L], det) :-
    is_member_probe_bound(Probe),
    manifest_ground_dupfree_list(L).

%and/or/not/xor/implies are nondet because bool/1 invents a boolean; with
%every operand manifestly bound it is only a test.
builtin_argument_rule_verdict(manifest_booleans, _, Args, det) :-
    maplist(manifest_bool, Args).

%A source expression whose head is data rather than an applied function. For
%a variable head this is the translator's own test (nonfunction_type/1); an
%untyped variable head is conservatively a call.
data_headed(H) :- var(H), !, known_singleton(H, K), nonfunction_type(K).
data_headed(H) :- atom(H), !, \+ fun(H).
%A compound head is data by the same rule one level down: ((c) 1 2) with c a
%declared constant is a nested literal, but ((foo) 2 3) applies whatever
%closure (foo) returns, and none of its spine is built here. A fun-headed
%compound is still data when the function's unique declared output is a
%non-function type, as the translator compiles it (nonfunction_type/1).
data_headed(H) :- is_list(H), H = [F|Fargs], !,
                  ( data_headed(F) -> true
                  ; atom(F), fun(F), length(Fargs, N),
                    unique_fn_decl(F, N, _, OT1),
                    nonvar(OT1), nonfunction_type(OT1) ).
data_headed(_).

%Manifestly a proper list: (), a literal expression whose head is data, or a
%cons onto a manifestly proper tail - a spine the compiler builds at the call
%site. A declared type never qualifies, not even a fixed-width tuple: the
%residual guard accepts an unbound variable, so a (Number Number) parameter
%can arrive unbound out of well-typed code ((B $u) leaves its field unfilled).
manifest_proper_list(X) :- X == [], !.
manifest_proper_list(X) :- var(X), !,
                           ( scoped_proper_list_var(X)
                           ; enforced_proper_list_value(X)
                           ; enforced_bound_tuple(X, _) ).
manifest_proper_list(X) :- is_list(X), X = [H|_], data_headed(H), !.
manifest_proper_list(X) :- nonvar(X), X = [C, _, Tl], ( C == cons ; C == 'cons-atom' ),
                           manifest_proper_list(Tl).
%A call to a function whose every clause provably results in a bound proper
%list (proper_list_output/2) - which a declared (List _) output never proves.
%Nonempty is not implied (collapse can yield ()), so this is not in
%manifest_nonempty_list/1.
manifest_proper_list(X) :- nonvar(X), X = [G|GArgs], atom(G),
                           \+ ( G == cons ; G == 'cons-atom' ),
                           length(GArgs, N), proper_list_output(G, N).

manifest_nonempty_list(X) :- var(X), !, enforced_bound_tuple(X, W), W >= 1.
manifest_nonempty_list(X) :- is_list(X), X = [H|_], data_headed(H), !.
manifest_nonempty_list(X) :- nonvar(X), X = [C, _, Tl], ( C == cons ; C == 'cons-atom' ),
                             manifest_proper_list(Tl).

%Manifestly a bound boolean: a literal, or a call to a det builtin whose only
%declared output type is Bool, which builds true/false itself.
manifest_bool(X) :- X == true, !.
manifest_bool(X) :- X == false, !.
%A direct parameter under an explicit committed arrow, declared Bool: the
%boundary check makes it bound. A destructured Bool field does not qualify.
manifest_bool(X) :- var(X), !, enforced_bound_param(X), known_singleton(X, 'Bool').
%The call-site verdict, not the flat worst case: (not X) with X a manifest
%bool is det. The mutual recursion with builtin_call_determinism_args/4 is
%well-founded on strict subterms.
manifest_bool([F|As]) :- atom(F), is_list(As), length(As, N),
                         ( builtin_call_determinism_args(F, N, As, det) -> true
                         ; builtin_flat_cardinality(F, N, det) ),
                         unique_fn_decl(F, N, _, OT1), OT1 == 'Bool'.
%A user function whose bound_bool certificate holds; a declared Bool output
%alone can still return an unbound value.
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

%%% Output certificates: output_cert(Kind, F, N) holds when EVERY clause of
%%% F/N provably yields a bound value of shape Kind - proper_list (collapse,
%%% literal spine, certified call) or bound_bool (literal, det Bool builtin,
%%% certified call, an if/let whose every branch qualifies).
%
%Derivation is demand-driven and coinductive like body_determinism/3: a
%recursive reference to a function already on the proof stack is assumed to
%hold. That is sound for shape-of-every-output properties, since a produced
%value traces a finite call tree whose leaves are literal or builtin shapes,
%and it certifies mutual recursion (even-number?/odd-number?). Assumptions
%only add successes, so a `no` is definitive. Only outermost proofs are
%memoized. Every consumer records output_cert(Kind, F/N), so a later clause
%that breaks the certificate invalidates and recompiles them.
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

%Every clause body of F/N the prover can see: the compiled store plus the
%current file's pending prepass bodies, so mutually recursive functions
%certify in source order. A body in both stores is qualified twice, which is
%idempotent.
:- dynamic pending_clause_body/4.   % pending_clause_body(File, F, N, Body)

cert_clause_bodies(F, N, Bodies) :-
    findall(B, ( catch(nb_getval(F, Ms), _, fail),
                 member(Meta, Ms),
                 Meta = fun_meta(As, B, _),
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
%The IR function summary answers inside a file batch; the certificate covers
%calls made outside one.
bool_output(F, N) :- unified_function_result_fact(F, N, proper_bool), !.
bool_output(F, N) :- output_cert(bound_bool, F, N).

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
    ; builtin_flat_cardinality(F, N, det) ),
    unique_fn_decl(F, N, _, OT1), OT1 == 'Bool', !.
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
    ; builtin_flat_cardinality(F, N, det) ),
    unique_fn_decl(F, N, _, OT1), OT1 == 'Bool', !.
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

%Also used by let_determinism/4: such a let value makes the bound variable a
%proper list, so (== $v ()) can narrow it.
val_guaranteed_proper_list(Val) :- clause_result_proper_list(Val).

%A literal proper-list spine, built at the clause site: the empty list, a
%data-headed list literal, or a cons onto a literal spine. Mirrors
%manifest_proper_list's literal clauses without the var/enforced-tuple case -
%a parameter is not a literal the clause builds.
proper_list_literal_spine(X) :- X == [], !.
proper_list_literal_spine(X) :- is_list(X), X = [H|_], data_headed(H), !.
proper_list_literal_spine(X) :- nonvar(X), X = [C, _, Tl], ( C == cons ; C == 'cons-atom' ),
                                proper_list_literal_spine(Tl).

%A direct parameter under an explicit committed arrow whose declared type is
%nominal, so its values are constructor applications - nonempty spines - and
%add/remove-atom is det. A nullary constructor or declared constant is an atom
%value on which remove_sexp's =.. fails, so the judgement publishes
%ctor_set(K): a later constant recompiles the consumer.
enforced_bound_nominal(T) :- var(T), enforced_bound_param(T),
                             known_singleton(T, K), atom(K),
                             user_atom_type(K), type_name_declared(K),
                             analysis_emit(dependency(ctor_set(K))),
                             \+ nominal_nullary_inhabitant(K).

%An inhabitant of K that is a bare atom at runtime: a declared constant of
%type K, or a nullary constructor (-> K):
nominal_nullary_inhabitant(K) :- declared_value_type(C, K2), K2 == K, atom(C), \+ fun(C), !.
nominal_nullary_inhabitant(K) :- member_ctor(K, 0, _).

%A direct parameter under an explicit committed arrow whose declared type is a
%fixed-width positional tuple: with the boundary check it is a bound proper
%nonempty list.
enforced_bound_tuple(X, W) :- var(X), enforced_bound_param(X),
                              known_singleton(X, K),
                              is_list(K), \+ special_compound_type(K),
                              length(K, W), W >= 1.

%A deterministic caller needs positive evidence about its callees. Functions
%without a determinism arrow are analyzed from their translated clauses,
%memoized, and treated as det on cycles (a recursive call cannot introduce
%what the rest disproves). A registered symbol with no MeTTa clauses is a
%Prolog builtin: only builtin_flat_cardinality/3 speaks for it, and anything
%unlisted is `unspecified` ((get-atoms) enumerates a whole space).
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
      -> ( builtin_flat_cardinality(F, N, Det0)
           -> Det = Det0 ; Det = unspecified ),
         analysis_make_proof(body(F/N), Det, [],
                             [effect(F/N), decl(F/N)], Proof)
    ; catch(b_getval('$det_stack', St), _, St = []),
      setup_call_cleanup(
          b_setval('$det_stack', [F/N|St]),
          with_compiling_caller(F, N,
          ( type_meta_params(F, N, Metas, Metas1),
            det_enforced_flag(F, N, Enf),
            clause_set_subject_proof(body(F/N), F/N, Enf, Metas1, Proof) )),
          b_setval('$det_stack', St)),
      analysis_cache_store(det(F, N), Proof) ).

%Stored clause metas are captured before clause_param_types binds the declared
%argument types, so a transitive analysis would read a data parameter as a
%function of unknown determinism (a var-headed tuple built from it looks like a
%dynamic call). Attach the declared parameter types to a COPY of each meta, as
%clause_param_types does for the direct check:
type_meta_params(F, N, Metas, Metas1) :- ( unique_fn_decl(F, N, ATs1, _)
                                           -> maplist(type_one_meta(ATs1), Metas, Metas1)
                                            ; Metas1 = Metas ).

type_one_meta(ATs1, Meta, Meta2) :- copy_term(Meta, Meta2),
                                    Meta2 = fun_meta(Args, _, _),
                                    maplist(bind_meta_param, Args, ATs1).

bind_meta_param(Arg, T) :- ignore(catch(bind_pattern_typed(Arg, T), _, true)).

arity_meta(N, Meta) :- Meta = fun_meta(Args, _, _), length(Args, N).

%The stored clause metadata of F/N; fails when it has no clauses:
fun_metas(F, N, Metas) :- catch(nb_getval(F, Metas0), _, fail),
                          include(arity_meta(N), Metas0, Metas),
                          Metas \== [].

%The worst verdict over ALL clause bodies decides (a may_fail clause followed
%by a nondeterministic one is nondet, not semidet), and overlapping heads
%multiply results whatever the bodies say:
clause_set_determinism(Metas, Det) :-
    clause_set_determinism_proof(Metas, Proof),
    analysis_proof_verdict(Proof, Det),
    analysis_reemit_proof(Proof).

%The proof of Subject, a property of F/N established by its clause set Metas
%analysed under the commitment gate Enf:
clause_set_subject_proof(Subject, F/N, Enf, Metas, Proof) :-
    with_det_enforced(Enf, clause_set_determinism_proof(Metas, ClauseProof)),
    analysis_proof_verdict(ClauseProof, Det),
    analysis_proof_requirements(ClauseProof, Bounds),
    analysis_proof_certificates(ClauseProof, Certs),
    analysis_proof_dependencies(ClauseProof, ClauseDeps),
    analysis_term_dependencies(Metas, TermDeps),
    append([[effect(F/N), decl(F/N), clause_set(F/N)], ClauseDeps, TermDeps],
           Ds0),
    sort(Ds0, Deps),
    Proof = analysis_proof(Subject, Det, requirements(Bounds),
                           certificates(Certs), dependencies(Deps)).

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
                                                     Meta = fun_meta(Args, B, _),
                                                     with_det_head_vars(Args, B, deterministic_expr_core(B, R1)),
                                                     ( det_result_final(R1) -> R = R1
                                                     ; clause_bodies_determinism(Ms, R2),
                                                       combine_det_results(R1, R2, R) ).

overlapping_meta_pair(Metas) :- append(_, [Meta1|Rest], Metas),
                                Meta1 = fun_meta(A1, _, _),
                                member(Meta2, Rest),
                                Meta2 = fun_meta(A2, B2, _),
                                clause_heads_overlap(A1, A2),
                                \+ body_commits(B2),
                                \+ body_conditionally_commits(B2).

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
         ; \+ is_arrow_type(K), \+ wildcard_type(K) -> combine_determinism_list(Args, Result)
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
%Structural (dis)equality unifies its operands as data: reduce/2 leaves a
%var-headed term unevaluated, so ($name $index $vars) is a pattern, not a
%dynamic call. Fun-headed operands are still evaluated and contribute.
deterministic_expr_core([Op, A, B], Result) :- unify_test_op(Op), !,
                                          unify_operand_determinism(A, RA),
                                          unify_operand_determinism(B, RB),
                                          combine_det_results(RA, RB, Result).
%A two-argument if produces nothing when the condition is false, so it is
%may_fail unconditionally:
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
%When the condition is (== V ()) or (== () V) with V of known list type, the
%else branch runs exactly when V is nonempty; record that narrowing while it
%is analysed (semidet_site_upgraded_to_det/3 reads it).
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
%The higher-order list builtins come in two forms. The pseudo-lambda form the
%translator inlines - (foldl-atom List Init $acc $x Body), (map-atom List $x
%Body), (filter-atom List $x Cond) - is as det as the list and the body. The
%closure form defined in src/metta.pl - (map-atom List F), (foldl-atom List
%Init F), (filter-atom List F) - is as det as its closure, which must carry det
%evidence exactly as for a user-written fold (det_arg_evidence/2). Both require
%a manifestly proper list (or a direct parameter with a proper-list proviso).
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
    % Keep the original occurrence: the IR origin map is identity based.
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
         % Walk the arguments: their diagnostics are detailed, and a declared
         % user-call contract nested in an argument must not prove itself.
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

%A var head is data only when it provably cannot be a function when reduce
%builds the term: its known type rules functions out (non-arrow, non-wildcard),
%or it is a fresh local - not a head variable of the clause (det_head_var/1,
%by identity, since a wildcard-typed parameter has no attribute) and carrying
%no knowledge (let_determinism/4 marks let/chain fields so they fail here).
unify_head_is_data(H) :- ( known_singleton(H, K), nonvar(K)
                           -> \+ is_arrow_type(K), \+ wildcard_type(K)
                         ; det_head_var(H) -> fail
                         ; \+ get_attr(H, tknown, _) ).

combine_unify_operands([], ok).
combine_unify_operands([A|As], R) :- unify_operand_determinism(A, R1),
                                     combine_unify_operands(As, R2),
                                     combine_det_results(R1, R2, R).
