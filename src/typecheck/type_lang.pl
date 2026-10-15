%%% Type language, normalization, compatibility, and the tknown/mreq
%%% attributed-variable hooks. type_unify/2 is the single compatibility
%%% relation; it binds type variables (polymorphism), so wrap it in
%%% type_compat_soft/2 for a side-effect-free check.
wildcard_type(T) :- atom(T), memberchk(T, ['%Undefined%', 'Atom', 'Expression']).

type_unify(A, B) :- ( var(A) ; var(B) ), !, A = B.
%A wildcard means "nothing is stated here", not "every type at once". The
%difference matters against a newtype brand, so the brand rules run before the
%wildcard shortcut (see brand_unify/2):
type_unify(A, B) :- ( brand_name(A) ; brand_name(B) ), !, brand_unify(A, B).
type_unify(A, B) :- ( wildcard_type(A) ; wildcard_type(B) ), !.
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

%%% (Newtype R) is nominal, and type_unify(Actual, Required) is one-directional:
%%%   1. A brand fits itself, and no other brand.
%%%   2. A brand fits a wildcard requirement: erased, the value simply is its
%%%      representation.
%%%   3. A brand fits a concrete requirement T exactly when its representation
%%%      does, unless the representation is a wildcard: "payload shape
%%%      unconstrained" is not "fits every type", or (Newtype Expression) would
%%%      make a brand compatible with Number.
%%%   4. Nothing implicitly fits a brand, not even a wildcard: a value acquires
%%%      one only by being written (brand T V).
brand_unify(A, B) :- brand_name(A), brand_name(B), !, A == B.
brand_unify(A, B) :- brand_name(B), !,
                     %a union VALUE fits a brand only if every member does,
                     %mirroring the is_union(A) rule below; nothing else does:
                     is_union(A), A = ['|'|As],
                     \+ ( member(MA, As), \+ type_compat_soft(MA, B) ).
brand_unify(_, B) :- wildcard_type(B), !.
brand_unify(A, B) :- is_union(B), !, B = ['|'|Ms], member(M, Ms), type_unify(A, M), !.
brand_unify(A, B) :-
    atom(A),
    analysis_emit(dependency(declaration(newtype, A))),
    declared_newtype(A, RA), \+ wildcard_type(RA), type_unify(RA, B).

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

%Analysis-only shape evidence: unlike a declared (List T), it records that the
%value-producing expression constructs a closed list spine.
proper_list_cert:attr_unify_hook(true, Other) :-
    ( var(Other) -> put_attr(Other, proper_list_cert, true) ; true ).

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
%A candidate set containing the unknown marker is not a singleton type: some
%flow carried a value of undetermined type.
known_singleton(V, K) :- get_attr(V, tknown, [K]), \+ unknown_candidate(K).

%%% Candidate evidence: every candidate in a tknown attribute is one of
%   literal(V) - a '$certifiable_literal'(V) wrapper: a ground data literal a
%                merge could not assign one type, such as (a b) fed to a
%                (| Number (List Atom)) output. Unknown everywhere except output
%                certification (output_candidate_fits/2), where check_value/3
%                can certify V against the concrete target.
%   unknown    - the '$unknown_branch_type' marker: "I don't know", never
%                "compatible with everything".
%   promised   - an unbound type variable this clause's declaration promised to
%                its callers (param_promise_var/1): the caller's choice, so it
%                is evidence for nothing here.
%   open_var   - any other unbound candidate: an open declaration instance,
%                universally quantified, which fits every requirement.
%   type(T)    - a concrete type T.
% Classification never binds the candidate: candidate lists legitimately hold
% unbound declaration-instance type variables.
unknown_marker('$unknown_branch_type').
certifiable_literal_candidate(C, V) :- nonvar(C), C = '$certifiable_literal'(V).

candidate_evidence(C, literal(V)) :- certifiable_literal_candidate(C, V), !.
candidate_evidence(C, unknown)    :- unknown_marker(M), C == M, !.
candidate_evidence(C, E)          :- var(C), !, ( param_promise_var(C) -> E = promised ; E = open_var ).
candidate_evidence(C, type(C)).

%A construct merging branches (if, case, let/chain, sealed, superpose,
%hyperpose) records each branch's type as a candidate of the result. An
%untyped branch records the marker, so "some candidate fits" cannot discharge
%an obligation for the whole disjunction: the result is never a known
%singleton, and its output certification falls back to a runtime guard (a
%rejection under --strict).
unknown_candidate(C) :- candidate_evidence(C, E), ( E == unknown ; E = literal(_) ).

%%% An obligation may only be discharged by evidence, and a type variable the
%%% clause's own declaration promised to its callers is not evidence:
%
%     (: g (-> (-> Number $b) Number))
%     (= (g $f) ($f 1))                    % result type is $b
%
% $b is whatever the caller's closure returns, so it must not certify the
% output as Number. An output type variable occurring in no argument type is
% different: by parametricity only a bottom implementation produces it, so it
% fits every requirement (open_var, as in set_call_out_type/3). The variable
% is not replaced by the marker because its identity aliases the declaration
% instance shared with the context, and type_compat_soft/2 is unchanged
% because it is also the definite-conflict test.
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

%Propagate Val's statically known type(s) into Out (branch and binding
%flows); an undeterminable type propagates the unknown marker:
note_candidates(Out, Val) :- ( var(Out)
                               -> ( nonvar(Val) -> ( value_single_type(Val, VT)
                                                     -> add_known_type(Out, VT)
                                                      ; ground(Val), is_list(Val)
                                                        -> note_certifiable_literal(Out, Val)
                                                      ; note_unknown_candidate(Out) )
                                  ; known_candidates(Val, Cs) -> add_known_types(Out, Cs)
                                  ; note_unknown_candidate(Out) )
                                ; true ).

%Like note_unknown_candidate/1, but keeps a ground literal so output
%certification can check it against the concrete target.
note_certifiable_literal(V, Val) :- ( var(V)
                                      -> M = '$certifiable_literal'(Val),
                                         ( get_attr(V, tknown, Cs)
                                           -> ( variant_member(M, Cs) -> true
                                              ; put_attr(V, tknown, [M|Cs]) )
                                            ; put_attr(V, tknown, [M]) )
                                       ; true ).

%Explicit type ascription (the Type Expr): the author states the type of a
%dynamically typed value. It becomes static knowledge and emits a runtime
%check even under --strict, which forbids only implicit residual checks; an
%ascription contradicting static knowledge is a compile-time error.
ascribe_type(V, T, Gs) :- ( var(T) -> Gs = []
                          ; wildcard_type(T) -> Gs = []
                          ; var(V) ->
                              %the ascription's own guard establishes T where
                              %an unknown branch could not: drop the marker
                              ( known_candidates(V, Cs0), candidates_have_unknown(Cs0), ground(T)
                                -> put_attr(V, tknown, [T]), ascription_guard(V, T, Gs)
                              ; known_singleton(V, K), var(K)
                                %the only known type is a bare declaration
                                %variable: narrow locally without binding it,
                                %so a parametric parameter stays universal
                                %while this boundary still emits its guard:
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

%(brand T Expr): erased trust in a semantic role, with no runtime goal. A value
%carrying a different brand is rejected, and the value must be statically
%admissible for the newtype's representation:
brand_type(V, T) :-
    ( \+ ( atom(T), declared_newtype(T, _) )
      -> throw(error(unknown_newtype(T), typecheck))
    ; var(V) -> brand_variable_type(V, T)
    ; check_value(V, T, St),
      ( St == mismatch -> throw(error(literal_type_mismatch(V, T), typecheck)) ; true ) ).

%An explicit brand is where an admissible representation acquires its nominal
%type: when every candidate fits the representation, the brand replaces the
%whole candidate set. An unknown candidate is discharged only by a wildcard
%representation; with a concrete one it stays beside the brand and keeps the
%strict residual.
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
    wildcard_type(R).
brand_evidence_fits_representation(literal(V), R) :-
    check_value(V, R, St),
    St == ok.
brand_evidence_fits_representation(type(K), R) :-
    \+ ( atom(K), declared_newtype(K, _) ),
    type_compat_soft(K, R).

%Concrete candidates must positively fit the representation: a brand has no
%runtime guard, so an untyped compound that ordinary checking calls "unknown"
%is not silently admissible to a concrete representation.
brand_candidate_conflict(R, C, Bad) :-
    candidate_evidence(C, literal(V)), !,
    \+ check_value(V, R, ok),
    Bad = V.
brand_candidate_conflict(R, C, K) :-
    candidate_evidence(C, type(K)),
    \+ type_compat_soft(K, R).

%An argument declared as the literal Atom stays unevaluated source - what a
%code-taking function asks for. Expression values, branded or not, follow
%ordinary eager translation:
expression_typed(Ty) :- Ty == 'Atom'.

%Derive match-pattern variable types from declared relation schemas: atoms
%matched by (F ...) conform to F's declared argument types, and a pattern
%(: $x T) binds $x : T directly. Conjunctive patterns type each conjunct.
type_match_pattern(P) :- ( is_list(P) -> type_match_pattern_list(P) ; true ).

type_match_pattern_list([C, V, Ty]) :- C == (:), var(V), nonvar(Ty), !,
                                       normalize_type(Ty, TN),
                                       ( \+ wildcard_type(TN) -> add_known_type(V, TN) ; true ).
type_match_pattern_list([C|Ps]) :- C == ',', !, maplist(type_match_pattern, Ps).
type_match_pattern_list([F|Args]) :- atom(F), length(Args, N),
                                     unique_fn_decl(F, N, ATs1, _), !,
                                     maplist(bind_pattern_arg, Args, ATs1).
type_match_pattern_list(_).

bind_pattern_arg(V, T) :- var(V), !, ( nonvar(T), \+ wildcard_type(T) -> add_known_type(V, T) ; true ).
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

set_out_type(Out, OT) :- ( var(Out), nonvar(OT), \+ wildcard_type(OT) -> add_known_type(Out, OT)
                                                                          ; true ).

%When F/N has exactly one declared output type, the call result is that type:
set_unique_decl_out(F, N, Out) :- ( atom(F), unique_fn_decl(F, N, _, OT1)
                                    -> set_out_type(Out, OT1) ; true ).

manual_dispatch_arg_checks_status(F, N, AVs, Gs, Status) :-
    ( atom(F), unique_fn_decl(F, N, ATs1, _)
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
