%%% Type language, normalization, compatibility, and checker attributes.
%
% Owns type_unify/2, alias/newtype normalization, union/brand/type-shape
% helpers, and the tknown/mreq attributed-variable hooks. Consumes canonical
% declaration views and runtime value-check interfaces used when an attributed
% value becomes nonvar. Boundary: attribute hooks must remain in their named
% modules, but their implementation is wholly defined here; type_unify/2 is
% the subsystem's single compatibility relation.
%
% type_unify/2 binds type variables (polymorphism); wrap it in
% type_compat_soft/2 for a side-effect-free check.
wildcard_type('%Undefined%').
wildcard_type('Atom').
wildcard_type('Expression').
wildcard_type_t(T) :- atom(T), wildcard_type(T).

type_unify(A, B) :- ( var(A) ; var(B) ), !, A = B.
%A wildcard is NOT "every type at once" - it is "nothing is stated here". The
%difference is invisible for structural types (a wildcard on either side
%simply discharges the obligation) but load-bearing against a newtype brand,
%so the brand rules run BEFORE the wildcard shortcut and the shortcut never
%sees a brand on either side. See brand_unify/2:
type_unify(A, B) :- ( brand_name(A) ; brand_name(B) ), !, brand_unify(A, B).
type_unify(A, B) :- ( wildcard_type_t(A) ; wildcard_type_t(B) ), !.
%Union types (| T1 T2 ...): a union value must fit every context member-wise;
%a value fits a required union if it fits some member:
type_unify(A, B) :- is_union(A), !, A = ['|'|As],
                    \+ ( member(MA, As), \+ type_compat_soft(MA, B) ).
type_unify(A, B) :- is_union(B), !, B = ['|'|Ms],
                    member(M, Ms), type_unify(A, M), !.
type_unify(A, B) :- atom(A), !, A == B.
%Arrows: a det closure fits anywhere, a nondet closure only fits an
%uncommitted plain slot or an explicit nondet requirement:
type_unify(A, B) :- is_arrow_type(A), is_arrow_type(B), !,
                    A = [HA|As], B = [HB|Bs],
                    det_arrow_fits(HA, HB),
                    same_length(As, Bs), maplist(type_unify, As, Bs).
type_unify(A, B) :- is_list(A), !, is_list(B), same_length(A, B), maplist(type_unify, A, B).
type_unify(A, B) :- A == B.

brand_name(T) :- atom(T),
                 analysis_emit(dependency(declaration(newtype, T))),
                 declared_newtype(T, _).

%%% (Newtype R) is NOMINAL, and the rule is one-directional and non-vacuous:
%%%
%%%   1. A brand fits itself, and no other brand.
%%%   2. A brand fits a WILDCARD requirement (%Undefined%/Atom/Expression).
%%%      That is what erasure means: at runtime the value simply is its
%%%      representation, and a wildcard requirement asks for nothing.
%%%   3. A brand fits a concrete requirement T exactly when its representation
%%%      does - PROVIDED the representation is not itself a wildcard. A
%%%      wildcard representation says "the payload shape is unconstrained",
%%%      and reading that as "the payload fits every type" would erase the
%%%      brand into a universal type: (: Proof (Newtype Expression)) made Proof
%%%      and Number mutually compatible, so a Proof could be handed to
%%%      (-> Number Number) with the obligation discharged statically.
%%%      Unconstrained is not the same as universal.
%%%   4. NOTHING implicitly fits a brand - not a wildcard either. The whole
%%%      point of a brand is that a value acquires it in exactly one way, by
%%%      being written (brand T V); an unknown or unconstrained value is not
%%%      evidence that the brand was ever applied.
%%%
%%% (3) and (4) are what closed the hole; (1) and (2) are the previous
%%% behaviour restated. Note the asymmetry is deliberate and matches the
%%% relation's argument convention: type_unify(Actual, Required).
brand_unify(A, B) :- brand_name(A), brand_name(B), !, A == B.
brand_unify(A, B) :- brand_name(B), !,
                     %a union VALUE fits a brand only if every member does,
                     %mirroring the is_union(A) rule below; nothing else does:
                     is_union(A), A = ['|'|As],
                     \+ ( member(MA, As), \+ type_compat_soft(MA, B) ).
brand_unify(_, B) :- wildcard_type_t(B), !.
brand_unify(A, B) :- is_union(B), !, B = ['|'|Ms], member(M, Ms), type_unify(A, M), !.
brand_unify(A, B) :-
    atom(A),
    analysis_emit(dependency(declaration(newtype, A))),
    declared_newtype(A, RA), \+ wildcard_type_t(RA), type_unify(RA, B).

%A closure fits a required arrow when it can produce no MORE results than the
%requirement allows (det < semidet < nondet). A plain requirement is
%uncommitted; a plain actual is never evidence for an explicit commitment:
det_arrow_fits(HA, HB) :- arrow_atom_det(HA, LA), arrow_atom_det(HB, LB),
                          det_level_fits(LA, LB).

det_level_fits(_, nondet) :- !.
det_level_fits(_, plain) :- !.
det_level_fits(_, effect(_)) :- !.
det_level_fits(LA, LB) :- ( LA == det -> true
                          ; LA == semidet -> LB == semidet ).

is_union(T) :- nonvar(T), T = [P|_], P == '|'.

type_compat_soft(A, B) :- \+ \+ type_unify(A, B).

is_arrow_type(T) :- nonvar(T), T = [A|_], arrow_atom(A).

list_type(T, ET) :- nonvar(T), T = [L, ET], L == 'List'.

%A compound type with dedicated syntax and semantics - an arrow, union, list,
%or opaque foreign type - as opposed to a plain positional tuple. A
%[Head|Fields] term that is NONE of these is read as a tagged/positional tuple,
%so the "is this an ordinary tuple" sites exclude exactly this set:
special_compound_type(T) :- ( is_arrow_type(T) ; is_union(T) ; list_type(T, _) ; foreign_type(T) ).

%%% Attribute hooks (permissive merging; errors are raised by explicit checks):
tknown:attr_unify_hook(Cs, Other) :-
    ( var(Other) -> ( get_attr(Other, tknown, C2) -> variant_union(Cs, C2, U),
                                                     put_attr(Other, tknown, U)
                                                   ; put_attr(Other, tknown, Cs) )
                  ; true ).

mreq:attr_unify_hook(Rs, Other) :-
    ( var(Other) -> ( get_attr(Other, mreq, R2) -> variant_union(Rs, R2, U),
                                                   put_attr(Other, mreq, U)
                                                 ; put_attr(Other, mreq, Rs) )
                  ; forall(member(R, Rs), typecheck_or_error(Other, R)) ).

%Analysis-only shape evidence.  Unlike a declared (List T), this records that
%the current value-producing expression constructs a closed list spine.  The
%attribute propagates through the copies made by nested let/let* analysis.
proper_list_cert:attr_unify_hook(true, Other) :-
    ( var(Other) -> put_attr(Other, proper_list_cert, true) ; true ).

variant_member(X, [Y|_]) :- X =@= Y, !.
variant_member(X, [_|T]) :- variant_member(X, T).

variant_union([], Ys, Ys).
variant_union([X|Xs], Ys, U) :- ( variant_member(X, Ys) -> variant_union(Xs, Ys, U)
                                                         ; variant_union(Xs, [X|Ys], U) ).

%%% Translation-time known types of variables:
add_known_type(V, T) :- nonvar(T), unknown_candidate(T), !, note_unknown_candidate(V).
add_known_type(V, T) :- ( get_attr(V, tknown, Cs) -> ( Cs = [K], var(K) -> K = T
                                                      ; variant_member(T, Cs) -> true
                                                      ; put_attr(V, tknown, [T|Cs]) )
                                                   ; put_attr(V, tknown, [T]) ).

known_candidates(V, Cs) :- get_attr(V, tknown, Cs).
%A candidate set containing the unknown marker is NOT a singleton type: some
%flow into this variable carried a value of undetermined type, so nothing may
%be discharged from the types the other flows happened to contribute.
known_singleton(V, K) :- get_attr(V, tknown, [K]), \+ unknown_candidate(K).

%%% The candidate-evidence classifier.
%
% Every candidate stored in a variable's tknown attribute answers to exactly
% one kind of evidence. Each discharge/conflict/certainty site in the checker
% used to re-derive that classification with its own ad-hoc mix of ==, nonvar
% and marker tests; candidate_evidence/2 is the single relation they now
% consult. The marker atoms below stay as the stored representation (attribute
% contents are data) - this consolidates the READING of them, not the writing:
%
%   literal(V) - a '$certifiable_literal'(V) wrapper: a ground data literal a
%                merge could not assign a single type to - (a b) fed to a
%                (| Number (List Atom)) output. value_single_type/2 fails on it
%                (its bare-atom elements carry no declared type), so an ordinary
%                merge would record the plain unknown marker; it is recorded
%                wrapped instead. It counts as unknown EVERYWHERE (a merged
%                variable carrying it is never a known singleton, arg passing
%                and parametric checks stay conservative) and is read specially
%                only at the output certification (output_candidate_fits/2),
%                where the concrete target is in hand and check_value/3 can
%                certify the actual value V.
%   unknown    - the '$unknown_branch_type' marker: a branch whose type the
%                checker could not determine. "I don't know", never "compatible
%                with everything".
%   promised   - an unbound type variable this clause's declaration promised to
%                its callers (param_promise_var/1): its value is the CALLER's
%                choice, so it is evidence for nothing here (see below).
%   open_var   - any other unbound candidate: a legitimate open declaration
%                instance, universally quantified, which fits every requirement.
%   type(T)    - a concrete type T: ordinary evidence.
%
% The relation NEVER binds C: every clause tests by == or a nonvar/var guard,
% so a candidate list legitimately holding unbound declaration-instance type
% variables (a param promise, or an open instance shared with the context) is
% classified, never instantiated to a marker or a target type. The nonvar guard
% on certifiable_literal_candidate/2 is part of that discipline: the output
% discharge is called with candidates that may be such variables and must TEST,
% never bind one to the wrapper.
unknown_marker('$unknown_branch_type').
certifiable_literal_candidate(C, V) :- nonvar(C), C = '$certifiable_literal'(V).

candidate_evidence(C, literal(V)) :- certifiable_literal_candidate(C, V), !.
candidate_evidence(C, unknown)    :- unknown_marker(M), C == M, !.
candidate_evidence(C, E)          :- var(C), !, ( param_promise_var(C) -> E = promised ; E = open_var ).
candidate_evidence(C, type(C)).

%%% The unknown-branch marker.
%
% A construct that merges several branches into one result (if, case,
% let/chain, sealed, superpose, hyperpose) records each branch's type as a
% candidate of the merged variable. A branch whose type the checker cannot
% determine used to record NOTHING, which made "some recorded candidate fits"
% - an existential test - look like a proof about the whole disjunction: one
% typed branch discharged the obligation for all of them, and an untyped
% branch could then deliver a value of any type where a concrete one was
% certified. The marker (and the wrapped literal) make the untyped branch
% visible, so the discharge tests are effectively universal again: a merged
% variable carrying it is never a known singleton, and an output certification
% over it falls back to a runtime guard (a hard rejection under --strict).
unknown_candidate(C) :- candidate_evidence(C, E), ( E == unknown ; E = literal(_) ).

%%% Indefinite evidence: a PROMISED type variable.
%
% An obligation may only be discharged by EVIDENCE, and one particular kind of
% unbound type variable is not evidence: one this clause's own declaration
% promised to its callers (see param_promise_var/1). Its value is whatever the
% CALLER chose, so answering a concrete requirement by unifying with it is the
% same confusion the unknown marker was introduced to fix for branch merges -
% "I don't know" read as "compatible with everything":
%
%     (: g (-> (-> Number $b) Number))
%     (= (g $f) ($f 1))                    % result type is $b, unbound
%
% $b is whatever the caller's closure returns, yet type_compat_soft($b,
% 'Number') succeeded and certified the clause's output as Number with no
% guard, under --strict. (g h) with (: h (-> Number String)) then handed a
% string to arithmetic.
%
% NOT every unbound candidate is indefinite. An output type variable occurring
% in none of its function's argument types is universally quantified, so by
% parametricity only a bottom implementation can produce it - (: empty (-> $a))
% is the one in the standard library - and a value that is never produced fits
% every requirement (candidate_evidence/2 classes it open_var, not promised).
% set_call_out_type/3 already makes exactly that distinction; this is its
% counterpart on the reading side.
%
% The var is deliberately NOT replaced by the marker: its identity is
% load-bearing (it aliases the declaration instance shared with the context -
% map-flat's element type - and known_singleton/2 with a var K is consulted on
% purpose by ascribe_type/3). Nor does type_compat_soft/2 change: it is also
% the definite-CONFLICT test, where refusing a var would turn "unknown" into
% "wrong". The distinction belongs at the discharge test, so it lives here.
indefinite_candidate(C) :- candidate_evidence(C, E), ( E == unknown ; E = literal(_) ; E == promised ).

note_unknown_candidate(V) :- ( var(V) -> unknown_marker(M),
                                         ( get_attr(V, tknown, Cs)
                                           -> ( variant_member(M, Cs) -> true
                                              ; put_attr(V, tknown, [M|Cs]) )
                                            ; put_attr(V, tknown, [M]) )
                                       ; true ).

candidates_have_unknown(Cs) :- member(C, Cs), unknown_candidate(C), !.

%Drop the marker; fails if it was there at all, so callers that can make no
%claim about a partly unknown value simply make none:
known_candidates_certain(V, Cs) :- known_candidates(V, Cs), \+ candidates_have_unknown(Cs).

%Propagate Val's statically known type(s) into Out (branch and binding flows).
%A value whose type cannot be determined propagates the unknown marker - that
%is knowledge too, and the only kind that keeps a merge honest:
note_candidates(Out, Val) :- ( var(Out)
                               -> ( nonvar(Val) -> ( value_single_type(Val, VT)
                                                     -> add_known_type(Out, VT)
                                                      ; ground(Val), is_list(Val)
                                                        -> note_certifiable_literal(Out, Val)
                                                      ; note_unknown_candidate(Out) )
                                  ; known_candidates(Val, Cs) -> add_known_types(Out, Cs)
                                  ; note_unknown_candidate(Out) )
                                ; true ).

%Record a ground literal a merge could not type as a certifiable-literal
%candidate (see unknown_candidate/1). Mirrors note_unknown_candidate/1 but keeps
%the value so the output certification can check it against the concrete target.
note_certifiable_literal(V, Val) :- ( var(V)
                                      -> M = '$certifiable_literal'(Val),
                                         ( get_attr(V, tknown, Cs)
                                           -> ( variant_member(M, Cs) -> true
                                              ; put_attr(V, tknown, [M|Cs]) )
                                            ; put_attr(V, tknown, [M]) )
                                       ; true ).

%Explicit type ascription (the Type Expr): the author states the type of a
%dynamically typed value. The type becomes knowledge for the checker, and a
%runtime check is emitted even under --strict: strict mode forbids *implicit*
%residual checks, while an ascription is an explicit, visible boundary.
%An ascription that contradicts static knowledge is a compile-time error.
ascribe_type(V, T, Gs) :- ( var(T) -> Gs = []
                          ; wildcard_type_t(T) -> Gs = []
                          ; var(V) ->
                              %an unknown branch fed this value, but the
                              %ascription's own runtime guard establishes T
                              %where the merge could not: take the ascription
                              %as the answer and drop the marker
                              ( known_candidates(V, Cs0), candidates_have_unknown(Cs0), ground(T)
                                -> put_attr(V, tknown, [T]), ascription_guard(V, T, Gs)
                              ; known_singleton(V, K), var(K)
                                %the value's only known type is a bare declaration-instance
                                %variable: narrow it locally WITHOUT binding that variable, so a
                                %parametric parameter stays universally quantified (its callers
                                %remain unchecked) while this explicit (the ...) boundary still
                                %emits the runtime guard - the ascription is honest, not a
                                %concrete-type requirement leaking into the declaration:
                                -> put_attr(V, tknown, [T]), ascription_guard(V, T, Gs)
                              ; known_singleton(V, K)
                                -> ( type_unify(K, T) -> Gs = []
                                   ; \+ \+ type_unify(T, K)       %the ascribed type fits the known type
                                     -> put_attr(V, tknown, [T]), %(e.g. a union member): narrow to it, checked
                                        ascription_guard(V, T, Gs)
                                   ; throw(error(type_conflict(existing(K), required(T)), typecheck)) )
                                 ; add_known_type(V, T),
                                   ascription_guard(V, T, Gs) )
                          ; check_value(V, T, St),
                            ( St == ok -> Gs = []
                            ; St == mismatch -> throw(error(literal_type_mismatch(V, T), typecheck))
                            ; ascription_guard(V, T, Gs) ) ).

ascription_guard(V, T, Gs) :- ( ground(T) -> warn_residual_check('(the ...)', T),
                                             guard_goal(V, T, G), Gs = [G]
                                           ; Gs = [] ).

%(brand T Expr): erased trust in a semantic role. No runtime goal exists - a
%role has no runtime witness by construction - but a value already carrying
%a DIFFERENT brand is rejected, and the value must be statically admissible
%for the newtype's representation:
brand_type(V, T) :-
    ( \+ ( atom(T), declared_newtype(T, _) )
      -> throw(error(unknown_newtype(T), typecheck))
    ; var(V) -> brand_variable_type(V, T)
    ; check_value(V, T, St),
      ( St == mismatch -> throw(error(literal_type_mismatch(V, T), typecheck)) ; true ) ).

%An explicit brand is the point where an admissible representation acquires
%its nominal type. Control-flow and case results can carry several candidate
%types, not just one; when every candidate fits the representation, replace
%the whole candidate set with the brand. Keeping the old singleton-only rule
%left both the representation candidates and T on the variable, so the later
%declared-output/consumer check still saw an apparent conflict with T.
%
%An unknown candidate can be discharged only by a wildcard representation
%(Expression/Atom/%Undefined%), exactly where every runtime value is an
%admissible payload. With a concrete representation it remains alongside the
%brand and therefore preserves the established strict residual.
brand_variable_type(V, T) :-
    known_candidates(V, Cs), !,
    ( member(K, Cs), candidate_is_other_brand(K, T)
      -> throw(error(type_conflict(existing(K), required(T)), typecheck))
    ; declared_newtype(T, R),
      maplist(brand_candidate_fits_representation(R), Cs)
      -> put_attr(V, tknown, [T])
    ; declared_newtype(T, R),
      member(C, Cs),
      brand_candidate_conflict(R, C, Bad)
      -> throw(error(type_conflict(existing(Bad), required(T)), typecheck))
    ; Cs = [K]
      -> ( type_unify(K, T) -> true
         ; throw(error(type_conflict(existing(K), required(T)), typecheck)) )
    ; add_known_type(V, T) ).
brand_variable_type(V, T) :-
    add_known_type(V, T).

candidate_is_other_brand(K, T) :-
    candidate_evidence(K, type(KT)),
    atom(KT),
    declared_newtype(KT, _),
    KT \== T.

brand_candidate_fits_representation(R, C) :-
    candidate_evidence(C, Evidence),
    brand_evidence_fits_representation(Evidence, R).

brand_evidence_fits_representation(unknown, R) :-
    wildcard_type_t(R).
brand_evidence_fits_representation(literal(V), R) :-
    check_value(V, R, St),
    St == ok.
brand_evidence_fits_representation(type(K), R) :-
    \+ ( atom(K), declared_newtype(K, _) ),
    type_compat_soft(K, R).

%Concrete candidates at an explicit brand boundary must positively fit its
%representation. In particular, a ground but otherwise untyped compound is
%not silently admissible to a concrete representation merely because ordinary
%value checking calls it "unknown"; the explicit brand has no runtime guard.
%The genuine unknown/open-variable cases remain non-conflicts and retain the
%established residual behavior for concrete representations.
brand_candidate_conflict(R, C, Bad) :-
    candidate_evidence(C, literal(V)), !,
    \+ check_value(V, R, ok),
    Bad = V.
brand_candidate_conflict(R, C, K) :-
    candidate_evidence(C, type(K)),
    \+ type_compat_soft(K, R).

%The Expression argument convention: an argument whose declared type is the
%LITERAL Expression stays unevaluated data - that is what a code-taking
%function asks for. A brand of Expression does NOT inherit it (this reverses
%an earlier newtype-transparent reading): (Newtype Expression) says the
%payload SHAPE is unconstrained, not that callers must quote, and inheriting
%the convention made a brand-typed position unable to receive a computed
%value at all - (expected-role? (holder-role $h)) passed the literal call.
%Atom is the sole type marker for unevaluated source terms; Expression values
%follow ordinary eager translation.
expression_typed(Ty) :- Ty == 'Atom'.

%Derive match-pattern variable types from declared relation schemas: atoms
%matched by (F ...) conform to F's declared argument types, and a pattern
%(: $x T) binds $x : T directly. Conjunctive patterns type each conjunct.
type_match_pattern(P) :- ( is_list(P) -> type_match_pattern_list(P) ; true ).

type_match_pattern_list([C, V, Ty]) :- C == (:), var(V), nonvar(Ty), !,
                                       normalize_type(Ty, TN),
                                       ( \+ wildcard_type_t(TN) -> add_known_type(V, TN) ; true ).
type_match_pattern_list([C|Ps]) :- C == ',', !, maplist(type_match_pattern, Ps).
type_match_pattern_list([F|Args]) :- atom(F), length(Args, N),
                                     findall(ATs, fn_decl_arity(F, N, ATs, _), [ATs1]), !,
                                     maplist(bind_pattern_arg, Args, ATs1).
type_match_pattern_list(_).

bind_pattern_arg(V, T) :- var(V), !, ( nonvar(T), \+ wildcard_type_t(T) -> add_known_type(V, T) ; true ).
bind_pattern_arg(A, _) :- type_match_pattern(A).

%Type the element variable of a higher-order construct from its list argument:
note_list_elem_type(XVar, L) :-
    ( var(XVar), list_elem_type(L, ET) -> add_known_type(XVar, ET) ; true ).

list_elem_type(L, ET) :- var(L), !, known_singleton(L, ['List', ET0]), ground(ET0), ET = ET0.
%A variable element type is fine for literal lists: it is a declaration
%instance var (e.g. rcons appending an $a onto a (List $a)), compared by
%identity so distinct unknowns do not conflate:
list_elem_type(L, ET) :- is_list(L), L = [E|Es],
                         value_single_type(E, ET),
                         forall(member(E2, Es), ( value_single_type(E2, T2), T2 == ET )).

add_known_types(V, Cs) :- maplist(add_known_type(V), Cs).

set_out_type(Out, OT) :- ( var(Out), nonvar(OT), \+ wildcard_type_t(OT) -> add_known_type(Out, OT)
                                                                          ; true ).

%When F/N has exactly one declared output type, the call result is that type:
set_unique_decl_out(F, N, Out) :- ( atom(F), findall(OT, fn_decl_arity(F, N, _, OT), [OT1])
                                    -> set_out_type(Out, OT1) ; true ).

%Manual call/reduce dispatch bypasses typed translation, not typing: when the
%target has exactly one declaration at this arity, its input checks apply:
manual_dispatch_arg_checks(F, N, AVs, Gs) :- ( atom(F), findall(ATs, fn_decl_arity(F, N, ATs, _), [ATs1])
                                               -> apply_call_args(declared, F, AVs, ATs1, Gs)
                                                ; Gs = [] ).

manual_dispatch_arg_checks_status(F, N, AVs, Gs, Status) :-
    ( atom(F), findall(ATs, fn_decl_arity(F, N, ATs, _), [ATs1])
      -> apply_call_args_status(declared, F, AVs, ATs1, Gs, Status)
    ; Gs = [], Status = verified ).

%Call-site output typing. An output type variable that occurs in no argument
%type is universally quantified - by parametricity only a bottom function
%like (: empty (-> $a)) can implement it - so the result is compatible with
%every requirement, without assigning a concrete type or emitting a guard:
set_call_out_type(Out, ATs, OT) :- ( nonvar(OT) -> set_out_type(Out, OT)
                                   ; var(Out), term_variables(ATs, Vs), \+ memberchk_eq(OT, Vs)
                                     -> add_known_type(Out, OT)
                                      ; true ).
