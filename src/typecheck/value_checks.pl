%%% Static and deferred value checking.
%
% Owns value candidate typing, literals/closures/lists/tuples checks, residual
% call-site guard construction, and deferred runtime requirement enforcement.
% Consumes the type language and attributes, canonical declarations, inference,
% determinism/effect queries, checker modes, and oracle hooks. It owns no
% persistent declaration or analysis store.
%
%%% Static typing of values (translated call results, literals, closures):
value_candidate_types(V, ['Number']) :- number(V), !.
value_candidate_types(V, ['String']) :- string(V), !.
value_candidate_types(true, ['Bool']) :- !.
value_candidate_types(false, ['Bool']) :- !.
value_candidate_types(V, Cs) :- atom(V), !,
                                analysis_emit(dependency(declaration(value, V))),
                                findall(T, declared_value_type(V, T), Vs),
                                findall([H|Xs], ( declared_fn_type(V, ATs, OT, Det),
                                                  length(ATs, NA), value_arrow_head(V, NA, Det, H),
                                                  append(ATs, [OT], Xs) ), Fs),
                                append(Vs, Fs, Cs0),
                                ( Cs0 == [], current_arithmetic_function(V)
                                  -> Cs = ['Number']                %arithmetic constants: inf, nan, pi, e
                                   ; Cs = Cs0 ).
value_candidate_types(partial(F, B), Cs) :- !,
                                length(B, N),
                                findall([H|Xs], ( fn_decl_partial(F, N, PTs, RTs, OT, Det),
                                                  length(RTs, NR), NA is N + NR,
                                                  value_arrow_head(F, NA, Det, H),
                                                  bound_args_match(B, PTs),
                                                  append(RTs, [OT], Xs) ), Cs).
value_candidate_types([], [['List', _]]) :- !.
%A constructor application (STV 0.5 0.8) has the constructor's output type,
%but only when its fields do not contradict the constructor's signature -
%otherwise the value is unknown and the (runtime or strict) guard decides.
%
%is_list/1 before length/2 is load-bearing, not defensive. A head pattern
%written with a variable tail - (cons Premises $p), which constrain_args/3
%compiles to the PARTIAL list ['Premises'|$p] - reaches here, and length/2 on
%a partial list is a GENERATOR: it proposes N = 0, 1, 2, ... forever, and
%since fn_decl_arity/4 fails for each one nothing ever cuts the loop. It ran
%out to a 76-million-element term (35s, 2.2GB, stack overflow) on a two-line
%file. A term whose tail is still unbound is not an n-argument constructor
%application - its arity is not known yet - so this clause simply does not
%apply to it, and it falls through to the "no candidate types" answer, which
%is the honest one. is_list/1 is also cycle-safe, so a rational tree (an
%inferred self-referential value) fails here rather than looping.
value_candidate_types([H|Args], Cs) :- atom(H), is_list(Args), length(Args, N), fn_decl_arity(H, N, _, _), !,
                                findall(OT, ( fn_decl_arity(H, N, ATs, OT),
                                              bound_args_match(Args, ATs) ), Cs).
value_candidate_types(V, Cs) :- is_list(V), maplist(value_single_type, V, Ts), !, Cs = [Ts].
value_candidate_types(_, []).

value_single_type(V, T) :- ( var(V) -> known_singleton(V, T)
                                     ; value_candidate_types(V, [T0]), T = T0 ).

det_arrow_head(Det, H) :- nonvar(Det), arrow_atom_det(H, Det), !.
det_arrow_head(_, (->)).

%The arrow head a declared symbol carries when it is used as a VALUE. The
%builtin table OVERRIDES the declared determinism (table_det_override/4, shared
%with the direct-call and oracle sites), then det_arrow_head/2 turns the
%effective determinism into the head atom. Without the override, a signature
%could make the same builtin look different in closure position than it does
%at a direct call. An undeclared builtin already got this right through
%inferred_arrow_head/3; the declaration was the only thing hiding the table:
value_arrow_head(F, N, Det, H) :- table_det_override(F, N, Det, Eff), det_arrow_head(Eff, H).

bound_args_match(B, PTs) :- \+ \+ maplist(arg_soft_ok, B, PTs).

%%% check_value(+Value, ?Type, -Status): Status in {ok, mismatch, unknown}.
%%% Binds type variables in Type on success (polymorphism resolution).
%%% Primitive fast paths first: they carry the hot arithmetic call sites.
check_value(V, T, St) :- number(V), !, ( var(T) -> T = 'Number', St = ok
                                       ; T == 'Number' -> St = ok
                                       ; prim_mismatch_status('Number', T, St) ).
check_value(V, T, St) :- string(V), !, ( var(T) -> T = 'String', St = ok
                                       ; T == 'String' -> St = ok
                                       ; prim_mismatch_status('String', T, St) ).
check_value(V, T, St) :- ( V == true ; V == false ), !,
                         ( var(T) -> T = 'Bool', St = ok
                         ; T == 'Bool' -> St = ok
                         ; prim_mismatch_status('Bool', T, St) ).
check_value(V, T, St) :- var(T), !, ( value_single_type(V, VT) -> T = VT ; true ), St = ok.
check_value(_, T, ok) :- foreign_type(T), !.
check_value(_, T, St) :- wildcard_type_t(T), !, St = ok.
check_value(V, T, St) :- is_union(T), !, T = ['|'|Ms],
                         ( member(M, Ms), check_value(V, M, SM), SM == ok -> St = ok
                         ; forall(member(M, Ms), check_value(V, M, mismatch)) -> St = mismatch
                         ; St = unknown ).
check_value(V, T, St) :- list_type(T, ET), !,
                         ( is_list(V) -> list_elems_status(V, ET, St)
                         ; non_list(V) -> St = mismatch
                         ; St = unknown ).
check_value(V, T, St) :- is_arrow_type(T), !,
                         ( ( atom(V) ; V = partial(_, _) )
                           -> value_candidate_types(V, Cs),
                              ( Cs == [] -> ( inferred_value_candidates(V, ICs),
                                              member(C, ICs), type_unify(C, T)
                                              -> St = ok       %inferred types are positive evidence only
                                               ; St = unknown )
                              ; member(C, Cs), type_unify(C, T) -> St = ok
                              ; St = mismatch )
                         ; ( number(V) ; string(V) ) -> St = mismatch
                         ; St = unknown ).

%Structural tuple types (Tag T1 ... Tn): the value must carry the same tag
%and arity, and its fields check recursively. A primitive or wildcard atom in
%head position is a type, not a tag - ($a Number) unified to (Number Number)
%is an untagged pair, handled by the next clause. See tagged_tuple_type/3 for
%when a head atom is a tag and when it is the first field's type:
check_value(V, T, St) :- tagged_tuple_type(T, Tag, FieldTs), !,
                         ( is_list(V) -> ( V = [VTag|Fields], VTag == Tag, same_length(Fields, FieldTs)
                                           -> tuple_fields_status(Fields, FieldTs, St)
                                            ; St = mismatch )
                         ; atom(V) -> atom_value_status(V, T, St)
                         ; non_list(V) -> St = mismatch
                         ; St = unknown ).
%Untagged tuple types like ($v Number): element-wise, the head position may
%be a type variable:
check_value(V, T, St) :- is_list(T), !,
                         ( is_list(V) -> ( same_length(V, T) -> tuple_fields_status(V, T, St)
                                                              ; St = mismatch )
                         ; atom(V) -> atom_value_status(V, T, St)
                         ; non_list(V) -> St = mismatch
                         ; St = unknown ).
%A raw or representation-typed value acquires a newtype contextually; a
%value already carrying a different brand does not (that is the feature):
check_value(V, T, St) :- atom(T), !,
                         analysis_emit(dependency(declaration(newtype, T))),
                         check_named_value(V, T, St).
check_value(_, _, unknown).

check_named_value(V, T, St) :- declared_newtype(T, R), !,
                         value_candidate_types(V, Cs),
                         ( Cs == [] -> ( constructed_definite_mismatch(V) -> St = mismatch
                                                                           ; check_value(V, R, St) )
                         ; member(C, Cs), type_unify(C, T) -> St = ok
                         ; member(C, Cs), candidate_not_branded(C),
                           type_unify(C, R) -> St = ok
                         ; St = mismatch ).
check_named_value(V, T, St) :-
                         value_candidate_types(V, Cs),
                         ( Cs == [] -> ( constructed_definite_mismatch(V) -> St = mismatch
                                                                           ; St = unknown )
                         ; member(C, Cs), type_unify(C, T) -> St = ok
                         ; member(C, Cs), refinement_pair(C, T) -> St = unknown
                         ; St = mismatch ).

%A constructor application every declaration of which is definitely
%contradicted by some field can never have any type. In particular a field
%branded with a different newtype is unfixable at runtime - brands are
%erased - so it must reject at compile time, not degrade to a guard:
constructed_definite_mismatch(V) :- is_list(V), V = [H|Args], atom(H),
                                    length(Args, N), fn_decl_arity(H, N, _, _),
                                    forall(fn_decl_arity(H, N, ATs, _),
                                           \+ \+ tuple_fields_status(Args, ATs, mismatch)).

%How an atom's declared candidate types stand against a required type:
atom_value_status(V, T, St) :- value_candidate_types(V, Cs),
                               ( Cs == [] -> St = unknown
                               ; member(C, Cs), type_unify(C, T) -> St = ok
                               ; St = mismatch ).

tuple_fields_status([], [], ok).
tuple_fields_status([F|Fs], [T|Ts], St) :- elem_status(F, T, S1),
                                           ( S1 == mismatch -> St = mismatch
                                           ; tuple_fields_status(Fs, Ts, S2),
                                             ( S2 == mismatch -> St = mismatch
                                             ; S1 == unknown -> St = unknown
                                             ; St = S2 ) ).

%Arrow types of closures over inferred (undeclared) functions:
%Inference makes no determinism claim by itself, but the clause-set analysis
%(the same transitive-evidence rule calls use) may PROVE one. That proof is
%worth exactly as much in every mode - --strict-det exists to force a
%determinism claim out of you, not to be a precondition for checking one you
%already wrote - so it runs unconditionally and, when it commits to det or
%semidet, the inferred arrow carries that real head. A committed head fits
%every slot a plain -> fits (see det_level_fits/2), so this only ever admits
%more. With no committed proof the old behaviour stands: conservatively
%nondet under --strict-det, an uncommitted plain -> otherwise.
committed_determinism(det).
committed_determinism(semidet).

inferred_arrow_head(F, N, H) :-
    ( catch(( function_call_determinism(F, N, D), committed_determinism(D) ), _, fail)
      -> det_arrow_head(D, H)
    ; strict_det(true) -> det_arrow_head(nondet, H)
    ; H = (->) ).

inferred_value_candidates(V, Cs) :- atom(V), !,
                                    findall([H|Xs], ( inferred_fn_type(V, ATs, OT),
                                                      length(ATs, N),
                                                      inferred_arrow_head(V, N, H),
                                                      append(ATs, [OT], Xs) ), Cs).
inferred_value_candidates(partial(F, B), Cs) :- !,
                                    length(B, N),
                                    findall([H|Xs], ( inferred_fn_type(F, ATs, OT),
                                                      length(ATs, Total), Total > N,
                                                      inferred_arrow_head(F, Total, H),
                                                      length(PTs, N), append(PTs, RTs, ATs),
                                                      bound_args_match(B, PTs),
                                                      append(RTs, [OT], Xs) ), Cs).
inferred_value_candidates(_, []).

%Slow completion of the primitive fast paths above:
prim_mismatch_status(P, T, St) :- ( wildcard_type_t(T) -> St = ok
                                  ; is_union(T) -> ( T = ['|'|Ms], member(M, Ms), type_compat_soft(P, M)
                                                     -> St = ok ; St = mismatch )
                                  ; atom(T), declared_newtype(T, R) -> prim_mismatch_status(P, R, St)
                                  ; atom(T) -> ( refinement_pair(P, T) -> St = unknown
                                                                        ; St = mismatch )
                                  ; ( T = [L|_], L == 'List' ; is_arrow_type(T) ) -> St = mismatch
                                  ; St = unknown ).

%A primitive/tuple type against a user-defined atom type may be a runtime
%refinement, but only once get-type has actually been extended by user code:
refinement_pair(C, T) :- user_extended_get_type,
                         ( ( primitive_type(C) ; tuple_type(C) ), user_atom_type(T) -> true
                         ; user_atom_type(C), ( primitive_type(T) ; tuple_type(T) ) ).

user_extended_get_type :- predicate_property('get-type'(_, _), number_of_clauses(N)), N > 1.

primitive_type('Number').
primitive_type('String').
primitive_type('Bool').
user_atom_type(T) :- atom(T), \+ primitive_type(T), \+ wildcard_type(T).
tuple_type(C) :- is_list(C), C \= [->|_], C \= ['List', _].

%Foreign types are nominal obligations over values supplied by native code.
%Their optional parameters are checked structurally, but their runtime terms
%are opaque and must never be inspected as tagged or positional tuples.
foreign_type(T) :- atom(T),
                   analysis_emit(dependency(declaration(foreign, T))),
                   declared_foreign_type(T, 0).
foreign_type(T) :- nonvar(T), T = [Name|Params], atom(Name),
                   analysis_emit(dependency(declaration(foreign, Name))),
                   declared_foreign_type(Name, N), length(Params, N).

candidate_not_branded(C) :-
    ( atom(C)
      -> analysis_emit(dependency(declaration(newtype, C))),
         \+ declared_newtype(C, _)
    ; true ).

%%% Tagged vs positional structural tuple types.
%
% A type of shape (H T1 ... Tn) is read in one of two ways, and the reading is
% driven by H's DECLARATION rather than by "H looks like a name":
%
%   TAGGED - the value must be the expression (H V1 ... Vn), carrying the
%   literal atom H, with Vi : Ti. Chosen when H is a declared constructor of
%   exactly n arguments - fn_decl_arity(H, n, _, _), the same discipline
%   structural_pattern_fields/4 uses - or when H carries NO type declaration
%   at all (an anonymous structural tag: (Stats Number Number Number)).
%
%   POSITIONAL - the value is any n+1 element expression whose i-th element
%   has the i-th listed type, H included. Chosen when the head is not an atom
%   (($v Number)), is a primitive or wildcard type ((Number Number)), or is an
%   atom DECLARED as something other than an n-ary constructor - typically a
%   type name, (: Statement Type). Naming a declared type in head position can
%   only mean "field 1 has this type", so (Statement KBContext Proof TV) is a
%   4-field record, not a tuple tagged with the atom Statement.
%
% Consequence for users: declare the field types of a positional tuple. An
% undeclared head atom keeps the legacy tagged reading.
tagged_tuple_type(T, Tag, FieldTs) :- nonvar(T), T = [Tag|FieldTs],
                                      atom(Tag), user_atom_type(Tag),
                                      \+ special_compound_type(T),
                                      length(FieldTs, N),
                                      ( fn_decl_arity(Tag, N, _, _) -> true
                                                                     ; \+ type_name_declared(Tag) ).

type_name_declared(Tag) :- ( declared_value_type(Tag, _) -> true
                           ; declared_newtype(Tag, _) -> true
                           ; declared_foreign_type(Tag, _) -> true
                           ; declared_fn_type(Tag, _, _, _) ).

%With an unresolved element type variable, a heterogeneous list is legal: the
%element type resolves to the common element type, or stays unconstrained.
list_elems_status(Es, ET, St) :- var(ET), !, St = ok,
                                 ( maplist(value_single_type, Es, Ts), Ts = [T1|Rest],
                                   forall(member(T2, Rest), T2 =@= T1)
                                   -> ET = T1 ; true ).
list_elems_status([], _, ok).
list_elems_status([E|Es], ET, St) :- elem_status(E, ET, S1),
                                     ( S1 == mismatch -> St = mismatch
                                     ; list_elems_status(Es, ET, S2),
                                       ( S2 == mismatch -> St = mismatch
                                       ; S1 == unknown -> St = unknown
                                       ; St = S2 ) ).

elem_status(E, ET, St) :- ( var(E) -> ( known_singleton(E, K) -> ( type_unify(K, ET) -> St = ok
                                                                                      ; St = mismatch )
                                                               ; St = unknown )
                                    ; check_value(E, ET, St) ).

%%% Side-effect-free per-argument admissibility (overload filtering):
arg_soft_ok(AV, T) :- ( var(AV) -> ( known_singleton(AV, K) -> copy_term(K, K2), type_unify(K2, T)
                                                             ; true )
                                 ; check_value(AV, T, St), St \== mismatch ).

decl_survives(AVs, ft(ATs, _)) :- \+ \+ maplist(arg_soft_ok, AVs, ATs).

arg_statically_ok(AV, T) :- \+ \+ ( var(AV) -> ( known_singleton(AV, K) -> type_unify(K, T)
                                               ; ( var(T) -> true ; wildcard_type_t(T) ) )
                                             ; check_value(AV, T, ok) ).

%%% Effectful call-site argument checking, one arg.
%
% The Mode is the PROVENANCE of the required type, and it decides what failing
% to establish that type at the call site may cost:
%
%   declared - the type is a promise the author wrote down, so it is a
%   requirement: a static mismatch is a compile error and anything unresolved
%   becomes a runtime guard.
%
%   inferred - the type was reconstructed from how the callee's body happens
%   to USE the parameter, which is not the same thing as what the callee
%   REQUIRES of it. In
%
%       (= (score $current $cand) (if (== $current none) $cand (max $cand $current)))
%
%   $current is inferred Number from the else branch, but the function
%   explicitly handles none and (score none 0.42) is a correct program. A
%   requirement is therefore only imposed where the compiler can see the value
%   is definitely of an incompatible type; where it merely cannot tell - an
%   undeclared atom like none, an untyped variable - inference stays silent
%   rather than demanding a type the callee never asked for. This is the
%   README's contract: inferred types add knowledge, they do not reject
%   programs that would otherwise run.
%
%   The definite-conflict guard is kept deliberately. It is what still catches
%   (f "a") against an inferred (= (f $x) (+ $x 1)): inference ELIDED the
%   guard inside f's body, so with no check at all that call quietly computes
%   98 (SWI reads a one-character string as its character code) instead of
%   raising a type error. Dropping a false rejection must not buy a silent
%   wrong answer.
check_call_arg(Mode, Fun, AV, T, Gs) :- ( var(AV)
                                          -> ( known_singleton(AV, K)
                                               -> ( nonvar(T), wildcard_type_t(T) -> Gs = []  %wildcards carry no knowledge
                                                  ; type_unify(K, T) -> oracle_arg_check(AV, T, Gs)
                                                  %conflicting brands cannot be deferred to a runtime
                                                  %guard - newtypes are erased there - so they reject
                                                  %now, but only on a promised type:
                                                  ; atom(T), declared_newtype(T, _), atom(K), declared_newtype(K, _)
                                                    -> ( Mode == declared
                                                         -> throw(error(type_conflict(existing(K), required(T)), typecheck))
                                                          ; taint_assumption(AV), Gs = [] )
                                                  ; taint_assumption(AV),  %known conflict: runtime error carries the value
                                                    type_guard(Fun, AV, T, Gs) )
                                             ; var(T) -> Gs = []
                                             ; wildcard_type_t(T) -> Gs = []
                                             %an untyped value is not evidence of a wrong one:
                                             ; Mode == inferred -> Gs = []
                                             ; type_guard(Fun, AV, T, Gs) )
                                        ; check_value(AV, T, St),   %also binds an open T: knowledge
                                          ( St == ok -> oracle_arg_check(AV, T, Gs)
                                          ; St == mismatch
                                            -> ( Mode == declared
                                                 -> throw(error(literal_type_mismatch(AV, T), typecheck))
                                                  ; type_guard(Fun, AV, T, Gs) )
                                          ; Mode == inferred -> Gs = []
                                          ; type_guard(Fun, AV, T, Gs) ) ).

%Open structured types (e.g. (List $a)) still guard their outer shape; only a
%fully unconstrained type variable needs no check at all:
type_guard(Fun, AV, T, Gs) :- ( nonvar(T), \+ wildcard_type_t(T)
                                -> ( undecidable_arrow_commitment(T)
                                     -> throw(error(determinism_conflict(Fun, unproven_closure(AV, T)), determinism))
                                   ; strict_mode(true),
                                     open_nominal_intersection_obligation(AV, T)
                                     -> open_nominal_intersection_types(AV, Types),
                                        throw(error(strict_nominal_intersection(Fun, Types),
                                                    typecheck))
                                   ; strict_mode(true)
                                     -> throw(error(strict_runtime_typecheck(Fun, typecheck_or_error(AV, T)), typecheck))
                                   ; trusted_guard_waiver(Fun)
                                     -> Gs = []
                                      ; warn_residual_check(Fun, T),
                                        guard_goal(AV, T, G), Gs = [G] )
                                 ; Gs = [] ).

%A runtime type check cannot count a closure's solutions: nothing it can
%inspect distinguishes a det function from a nondet one. So a determinism
%COMMITMENT in a required arrow type is undischargeable at runtime and must
%not be deferred to a guard - the same reason a conflicting newtype brand
%rejects rather than guards (brands are erased at runtime, determinism was
%never there in the first place). Reaching type_guard/4 with such a type means
%the commitment could not be established statically, so it is rejected here.
undecidable_arrow_commitment(T) :- is_arrow_type(T), T = [H|_], arrow_atom_det(H, L),
                                   committed_det(L).

%Inline the primitive fast path into the compiled goal so hot code only pays a
%native type test; the reflective check runs only when that test fails:
guard_goal(AV, 'Number', ( number(AV) -> true ; typecheck_or_error(AV, 'Number') )) :- !.
guard_goal(AV, 'String', ( string(AV) -> true ; typecheck_or_error(AV, 'String') )) :- !.
guard_goal(AV, 'Bool', ( ( AV == true ; AV == false ) -> true ; typecheck_or_error(AV, 'Bool') )) :- !.
guard_goal(AV, T, typecheck_or_error(AV, T)).

apply_call_args(Mode, Fun, AVs, ATs, Gs) :-
    open_nominal_intersection_obligations(AVs, ATs, Obligations),
    with_open_nominal_intersections(
        Obligations,
        ( maplist(check_call_arg(Mode, Fun), AVs, ATs, Gss),
          append(Gss, Gs) )).

%Open nominal atom types may overlap: one value can carry several declarations.
%When the same runtime variable fills such positions, strict compilation must
%not turn the unresolved intersection into a dead call.  Preserve the ordinary
%runtime boundary check for that one intrinsically dynamic case; primitive,
%newtype, parametric and unrelated obligations retain the normal strict rule.
open_nominal_intersection_obligations(AVs, ATs, Obligations) :-
    open_nominal_intersection_obligations_(AVs, ATs, AVs, ATs, 0, Os0),
    variant_union(Os0, [], Obligations).

open_nominal_intersection_obligations_([], [], _, _, _, []).
open_nominal_intersection_obligations_([V|Vs], [T|Ts], AVs, ATs, I, Os) :-
    ( var(V), open_nominal_atom_type(T),
      nth0(J, AVs, V2), J =\= I, V2 == V,
      nth0(J, ATs, T2), open_nominal_atom_type(T2), T \== T2
      -> Os = [obligation(V, T)|Rest]
    ; Os = Rest ),
    I2 is I + 1,
    open_nominal_intersection_obligations_(Vs, Ts, AVs, ATs, I2, Rest).

open_nominal_atom_type(T) :-
    atom(T), \+ primitive_type(T), \+ wildcard_type_t(T),
    \+ declared_newtype(T, _),
    \+ declared_type_alias(T, _),
    \+ declared_foreign_type(T, _),
    ( declared_value_type(_, T) ; member_ctor(T, _, _) ), !.

with_open_nominal_intersections(Obligations, Goal) :-
    ( catch(b_getval('$open_nominal_intersections', Saved), _, fail)
      -> true
    ; Saved = [] ),
    setup_call_cleanup(
        b_setval('$open_nominal_intersections', Obligations),
        Goal,
        b_setval('$open_nominal_intersections', Saved)).

open_nominal_intersection_obligation(V, T) :-
    catch(b_getval('$open_nominal_intersections', Obligations), _, fail),
    member(obligation(V0, T0), Obligations),
    V0 == V, T0 == T, !.

open_nominal_intersection_types(V, Types) :-
    catch(b_getval('$open_nominal_intersections', Obligations), _, fail),
    findall(T, (member(obligation(V0, T), Obligations), V0 == V), Ts0),
    sort(Ts0, Types).

%The ordinary interface remains goal-only. The status-bearing form is used by
%trusted library calls: suppressing a guard is allowed there, but it is not
%static evidence from which the declared result may be certified.
apply_call_args_status(Mode, Fun, AVs, ATs, Gs, Status) :-
    apply_call_args(Mode, Fun, AVs, ATs, Gs),
    ( trusted_guard_waiver(Fun),
      paired_unverified_obligation(AVs, ATs)
      -> Status = unverified
    ; Status = verified ).

%Library declarations are verified promises only across an untyped caller
%boundary.  A declaration on the enclosing function is an explicit opt-in to
%ordinary boundary enforcement, irrespective of the callee's origin.
trusted_guard_waiver(Fun) :-
    trusted_library_decl(Fun),
    current_compiling_caller(Caller, Arity),
    \+ fn_decl(Caller, Arity, _, _, _, _),
    analysis_emit(dependency(declaration(origin, Fun))).

trusted_unverified_call(Fun, Args) :-
    trusted_guard_waiver(Fun),
    length(Args, N),
    findall(ATs, fn_decl_arity(Fun, N, ATs, _), [ATs]),
    paired_unverified_obligation(Args, ATs).

paired_unverified_obligation([AV|AVs], [T|Ts]) :-
    ( \+ arg_statically_ok(AV, T)
    ; paired_unverified_obligation(AVs, Ts) ).

%%% Runtime residual guards (only emitted where types stay unresolved).
%%% Bound values are checked via the user-extensible get-type reflection, so
%%% runtime refinement types (see examples/types_dependent.metta) keep working:
typecheck_or_error(V, T) :- ( var(V) -> constrain_var_type(V, T)
                            ; runtime_type_ok(V, T) -> true
                            ; throw(error(literal_type_mismatch(V, T), typecheck)) ).

%Non-throwing variant used inside overload dispatch branches:
typecheck_match(V, T) :- ( var(V) -> constrain_var_type(V, T)
                                   ; runtime_type_ok(V, T) ).

%Fast paths first: primitive values in hot code must not pay for reflection.
runtime_type_ok(V, 'Number') :- number(V), !.
runtime_type_ok(V, 'String') :- string(V), !.
runtime_type_ok(V, 'Bool') :- ( V == true ; V == false ), !.
runtime_type_ok(_, T) :- var(T), !.
runtime_type_ok(_, T) :- wildcard_type_t(T), !.
runtime_type_ok(V, T) :- list_type(T, ET), !,
                         runtime_list_ok(V, ET).
runtime_type_ok(V, T) :- is_arrow_type(T), !,
                         runtime_callable_at_arrow_arity(V, T),
                         \+ value_definitely_mismatch(V, T).
runtime_type_ok(V, T) :- is_union(T), !, T = ['|'|Ms],
                         member(M, Ms), runtime_type_ok(V, M), !.
%Newtypes are erased: at runtime only the representation exists.
runtime_type_ok(V, T) :- atom(T), declared_newtype(T, R), !, runtime_type_ok(V, R).
runtime_type_ok(V, T) :- tagged_tuple_type(T, Tag, FieldTs), !,
                         is_list(V), V = [VTag|Fields], VTag == Tag,
                         same_length(Fields, FieldTs),
                         runtime_tuple_ok(Fields, FieldTs).
runtime_type_ok(V, T) :- is_list(T), !,
                         is_list(V), same_length(V, T),
                         runtime_tuple_ok(V, T).
%Nominal values type by their cached declarations before falling back to
%get-type reflection (which scans &self per lookup - hot residual checks on
%constructed values must not pay that). Only positive matches commit:
runtime_type_ok(V, T) :- atom(T), nominal_value_ok(V, T), !.
%get-type is user-extensible and extensions may call typechecked code, so a
%guard reached from within a get-type call must not recurse into get-type:
runtime_type_ok(_, _) :- nb_current('$in_typecheck', true), !.
runtime_type_ok(V, T) :- setup_call_cleanup(nb_setval('$in_typecheck', true),
                                            ( 'get-type'(V, T) *-> true ; 'get-metatype'(V, T) ),
                                            nb_setval('$in_typecheck', false)).

%Arrow guards require positive callable evidence. An arbitrary atom is valid
%MeTTa data and reduce/2 would return its application-shaped term unchanged;
%that fallback must not satisfy a function type.
runtime_callable_at_arrow_arity(V, T) :-
    T = [_|Tail],
    length(Tail, TailLen),
    Required is TailLen - 1,
    Required >= 0,
    ( atom(V), fun(V),
      CompiledArity is Required + 1,
      arity(V, CompiledArity)
    ; nonvar(V), V = partial(F, Bound), atom(F), fun(F),
      is_list(Bound), length(Bound, NB),
      arity(F, FullArity),
      Required =:= FullArity - NB - 1
    ).

runtime_list_ok(V, ET) :- var(V), !, constrain_var_type(V, ['List', ET]).
runtime_list_ok([], _).
runtime_list_ok([E|Es], ET) :- ( var(E) -> constrain_var_type(E, ET) ; runtime_type_ok(E, ET) ),
                               runtime_list_ok(Es, ET).

%A constructor application whose unique declaration outputs T (fields still
%checked), or a value atom declared T:
nominal_value_ok(V, T) :- is_list(V), V = [Ctor|Fields], atom(Ctor), !,
                          length(Fields, N),
                          findall(ATs-OT, fn_decl_arity(Ctor, N, ATs, OT), [FieldTs-OT1]),
                          OT1 == T,
                          runtime_tuple_ok(Fields, FieldTs).
nominal_value_ok(V, T) :- atom(V), declared_value_type(V, VT), VT == T.

runtime_tuple_ok([], []).
runtime_tuple_ok([F|Fs], [T|Ts]) :- ( var(F) -> constrain_var_type(F, T) ; runtime_type_ok(F, T) ),
                                    runtime_tuple_ok(Fs, Ts).

constrain_var_type(V, T) :- ( get_attr(V, mreq, Rs)
                              -> variant_union([T], Rs, U),
                                 put_attr(V, mreq, U)
                               ; put_attr(V, mreq, [T]) ).

value_definitely_mismatch(V, T) :- copy_term(T, T2), check_value(V, T2, St), !, St == mismatch.

goal_or_throw(Goal, Error) :- ( call(Goal) *-> true ; throw(Error) ).
