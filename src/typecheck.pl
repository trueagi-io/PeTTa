%%%%%%%%%% Compile-time typechecking support (see AGENTS.md) %%%%%%%%%%
%
% One canonical type store, one compatibility relation (type_unify/2), and one
% type channel: attributed variables on the Prolog vars representing MeTTa vars.
%   tknown - translation-time inferred/declared type candidates of a variable
%   mreq   - runtime type constraints placed on still-unbound values by guards
% Static errors are thrown during translation (never emitted and re-scanned).

:- dynamic declared_fn_type/4.     % declared_fn_type(F, ArgTypes, OutType, Det)
:- dynamic declared_value_type/2.  % declared_value_type(Name, Type)
:- dynamic strict_mode/1.

:- dynamic strict_det/1.

:- current_prolog_flag(argv, Argv),
   ( memberchk('--strict-det', Argv) -> assertz(strict_mode(true)), assertz(strict_det(true))
   ; memberchk('--strict', Argv) -> assertz(strict_mode(true)), assertz(strict_det(false))
                                  ; assertz(strict_mode(false)), assertz(strict_det(false)) ).

%%% Arrow shapes: prefix (-> A B) and infix determinism arrows (A -[det]-> B).
%%% Under --strict-det a plain -> is a determinism commitment: functions are
%%% deterministic unless declared -[nondet]->.
plain_arrow_det(Det) :- ( strict_det(true) -> Det = det ; Det = unspecified ).

arrow_det('->', Det) :- plain_arrow_det(Det).
arrow_det('-[det]->', det).
arrow_det('-[deterministic]->', det).
arrow_det('-[nondet]->', nondet).
arrow_det('-[nondeterministic]->', nondet).

fn_type_shape(Type, ArgTypes, OutType, Det) :- is_list(Type), Type = [Arrow|Xs],
                                               Arrow == (->), !,
                                               plain_arrow_det(Det),
                                               append(ArgTypes, [OutType], Xs).
%Juxtaposed infix form (A B -[det]-> C): a single arrow before the output type:
fn_type_shape(Type, ArgTypes, OutType, Det) :- is_list(Type),
                                               append(ArgTypes, [Arrow, OutType], Type),
                                               nonvar(Arrow), arrow_det(Arrow, Det),
                                               ArgTypes \== [],
                                               \+ ( member(X, ArgTypes), nonvar(X), arrow_det(X, _) ), !.
%Chained infix form (A -[det]-> B -[nondet]-> C): arrows between every element:
fn_type_shape(Type, ArgTypes, OutType, Det) :- is_list(Type), Type = [First, Arrow|_],
                                               nonvar(Arrow), arrow_det(Arrow, _),
                                               \+ (nonvar(First), arrow_det(First, _)),
                                               infix_fn_parts(Type, ArgTypes, OutType, Det).

infix_fn_parts([A, Arrow, B], [A], B, Det) :- nonvar(Arrow), arrow_det(Arrow, Det), !.
infix_fn_parts([A, Arrow|Rest], [A|As], Out, Det) :- nonvar(Arrow), arrow_det(Arrow, _),
                                                     infix_fn_parts(Rest, As, Out, Det).

%Normalize nested arrow types to canonical prefix form. Nondeterministic
%arrows keep their marker so closure parameters carry the commitment:
normalize_type(T, T) :- var(T), !.
normalize_type(T, T) :- atomic(T), !.
normalize_type(T, TN) :- is_list(T), fn_type_shape(T, ATs, OT, Det), !,
                         maplist(normalize_type, ATs, ATN),
                         normalize_type(OT, OTN),
                         append(ATN, [OTN], Xs),
                         ( Det == nondet -> TN = ['-[nondet]->'|Xs] ; TN = [->|Xs] ).
normalize_type(T, TN) :- is_list(T), !, maplist(normalize_type, T, TN).
normalize_type(T, T).

%%% Store maintenance, called from add_sexp/remove_sexp and forget_symbol.
%%% Caching is idempotent so seeded builtins and imports do not duplicate:
%A function type declared for a parenthesized name - (: (/?\) (-> ...)) - is a
%malformed declaration that would otherwise be ignored silently:
maybe_cache_type_decl(Space, Term) :- Space == '&self', is_list(Term), Term = [C, [Name], Type],
                                      C == (:), atom(Name),
                                      nonvar(Type), fn_type_shape(Type, _, _, _), !,
                                      format(user_error,
                                             "Warning: type declaration name (~w) is an expression; write (: ~w ...) to declare the function~n",
                                             [Name, Name]).
maybe_cache_type_decl(Space, Term) :- ( Space == '&self', is_list(Term), Term = [C, Name, Type],
                                        C == (:), atom(Name)
                                        -> ( nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
                                             -> maplist(normalize_type, ATs, ATN),
                                                normalize_type(OT, OTN),
                                                retractall(inferred_fn_type(Name, _, _)),  %declaration supersedes inference
                                                ( declared_fn_type(Name, A2, O2, D2),
                                                  (A2-O2-D2) =@= (ATN-OTN-Det) -> true
                                                ; warn_if_late_declaration(Name),
                                                  assertz(declared_fn_type(Name, ATN, OTN, Det)) )
                                              ; normalize_type(Type, TN),
                                                ( declared_value_type(Name, T2), T2 =@= TN -> true
                                                ; assertz(declared_value_type(Name, TN)) ) )
                                         ; true ).

%Declaration prepass: only function (arrow) declarations are hoisted, so
%definitions may call helpers declared later in the same file. Value
%declarations stay order-sensitive - they are knowledge atoms whose position
%is meaningful (see examples/types_nondet.metta):
precache_fn_type_decl(Space, Term) :- ( is_list(Term), Term = [C, Name, Type],
                                        C == (:), atom(Name), nonvar(Type),
                                        fn_type_shape(Type, _, _, _)
                                        -> maybe_cache_type_decl(Space, Term)
                                         ; true ).

%Seed the store with the builtin operator types (called once after loading):
seed_builtin_types :- library_path(Base),
                      atomic_list_concat([Base, '/lib_builtin_types.metta'], Path),
                      read_file_to_string(Path, S, []),
                      metta_string_forms(S, Forms),
                      forall(member(form(FormStr, _), Forms),
                             ( sread(FormStr, Term),
                               maybe_cache_type_decl('&self', Term) )).

maybe_uncache_type_decl(Space, Term) :- ( Space == '&self', is_list(Term), Term = [C, Name, Type],
                                          C == (:), atom(Name)
                                          -> ( nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
                                               -> maplist(normalize_type, ATs, ATN),
                                                  normalize_type(OT, OTN),
                                                  ( clause(declared_fn_type(Name, A2, O2, D2), true, Ref),
                                                    (A2-O2-D2) =@= (ATN-OTN-Det)
                                                    -> erase(Ref) ; true )
                                                ; normalize_type(Type, TN),
                                                  ( clause(declared_value_type(Name, T2), true, Ref),
                                                    T2 =@= TN
                                                    -> erase(Ref) ; true ) )
                                           ; true ).

%Type declarations only affect later forms; warn when one arrives after the
%function's clauses were already compiled (a silent no-op otherwise):
warn_if_late_declaration(Name) :-
    ( catch(nb_getval(Name, [_|_]), _, fail)
      -> format(user_error,
                "Warning: type declaration for ~w arrives after its definition; already-compiled clauses and earlier calls are unaffected~n",
                [Name])
       ; true ).

forget_symbol_types(Name) :- retractall(declared_fn_type(Name, _, _, _)),
                             retractall(declared_value_type(Name, _)),
                             retractall(inferred_fn_type(Name, _, _)).

%%% Store lookup (each retrieval yields a fresh copy of the declaration):
fn_decl_arity(F, N, ATs, OT) :- declared_fn_type(F, ATs, OT, _), length(ATs, N).
fn_decl_partial(F, N, PTs, RTs, OT) :- fn_decl_partial(F, N, PTs, RTs, OT, _).
fn_decl_partial(F, N, PTs, RTs, OT, Det) :- declared_fn_type(F, ATs, OT, Det),
                                            length(ATs, Total), Total > N,
                                            length(PTs, N), append(PTs, RTs, ATs).

%%% The single compatibility relation. Binds type variables (polymorphism);
%%% wrap in type_compat_soft/2 for a side-effect-free check.
wildcard_type('%Undefined%').
wildcard_type('Atom').
wildcard_type('Expression').
wildcard_type_t(T) :- atom(T), wildcard_type(T).

type_unify(A, B) :- ( var(A) ; var(B) ), !, A = B.
type_unify(A, B) :- ( wildcard_type_t(A) ; wildcard_type_t(B) ), !.
%Union types (| T1 T2 ...): a union value must fit every context member-wise;
%a value fits a required union if it fits some member:
type_unify(A, B) :- is_union(A), !, A = ['|'|As],
                    \+ ( member(MA, As), \+ type_compat_soft(MA, B) ).
type_unify(A, B) :- is_union(B), !, B = ['|'|Ms],
                    member(M, Ms), type_unify(A, M), !.
type_unify(A, B) :- atom(A), !, A == B.
%Arrows: a det closure fits anywhere, a nondet closure only fits a nondet
%requirement once --strict-det makes plain -> a determinism commitment:
type_unify(A, B) :- is_arrow_type(A), is_arrow_type(B), !,
                    A = [HA|As], B = [HB|Bs],
                    det_arrow_fits(HA, HB),
                    same_length(As, Bs), maplist(type_unify, As, Bs).
type_unify(A, B) :- is_list(A), !, is_list(B), same_length(A, B), maplist(type_unify, A, B).
type_unify(A, B) :- A == B.

det_arrow_fits(HA, _) :- HA == (->), !.
det_arrow_fits(_, HB) :- ( HB == '-[nondet]->' -> true ; \+ strict_det(true) ).

is_union(T) :- nonvar(T), T = [P|_], P == '|'.

type_compat_soft(A, B) :- \+ \+ type_unify(A, B).

is_arrow_type(T) :- nonvar(T), T = [A|_], ( A == (->) ; A == '-[nondet]->' ).

list_type(T, ET) :- nonvar(T), T = [L, ET], L == 'List'.

%%% Attribute hooks (permissive merging; errors are raised by explicit checks):
tknown:attr_unify_hook(Cs, Other) :-
    ( var(Other) -> ( get_attr(Other, tknown, C2) -> variant_union(Cs, C2, U),
                                                     put_attr(Other, tknown, U)
                                                   ; put_attr(Other, tknown, Cs) )
                  ; true ).

mreq:attr_unify_hook(Rs, Other) :-
    ( var(Other) -> ( get_attr(Other, mreq, R2) -> forall(member(A, Rs),
                                                          forall(member(B, R2), type_compat_soft(A, B))),
                                                   append(Rs, R2, U),
                                                   put_attr(Other, mreq, U)
                                                 ; put_attr(Other, mreq, Rs) )
                  ; forall(member(R, Rs), \+ value_definitely_mismatch(Other, R)) ).

variant_member(X, [Y|_]) :- X =@= Y, !.
variant_member(X, [_|T]) :- variant_member(X, T).

variant_union([], Ys, Ys).
variant_union([X|Xs], Ys, U) :- ( variant_member(X, Ys) -> variant_union(Xs, Ys, U)
                                                         ; variant_union(Xs, [X|Ys], U) ).

%%% Translation-time known types of variables:
add_known_type(V, T) :- ( get_attr(V, tknown, Cs) -> ( Cs = [K], var(K) -> K = T
                                                      ; variant_member(T, Cs) -> true
                                                      ; put_attr(V, tknown, [T|Cs]) )
                                                   ; put_attr(V, tknown, [T]) ).

known_candidates(V, Cs) :- get_attr(V, tknown, Cs).
known_singleton(V, K) :- get_attr(V, tknown, [K]).

%Propagate Val's statically known type(s) into Out (branch and binding flows):
note_candidates(Out, Val) :- ( var(Out)
                               -> ( nonvar(Val) -> ( value_single_type(Val, VT)
                                                     -> add_known_type(Out, VT) ; true )
                                  ; known_candidates(Val, Cs) -> add_known_types(Out, Cs)
                                  ; true )
                                ; true ).

%Explicit type ascription (the Type Expr): the author states the type of a
%dynamically typed value. The type becomes knowledge for the checker, and a
%runtime check is emitted even under --strict: strict mode forbids *implicit*
%residual checks, while an ascription is an explicit, visible boundary.
%An ascription that contradicts static knowledge is a compile-time error.
ascribe_type(V, T, Gs) :- ( var(T) -> Gs = []
                          ; wildcard_type_t(T) -> Gs = []
                          ; var(V) ->
                              ( known_singleton(V, K)
                                -> ( type_unify(K, T) -> Gs = []
                                   ; \+ \+ type_unify(T, K)       %the ascribed type fits the known type
                                     -> put_attr(V, tknown, [T]), %(e.g. a union member): narrow to it, checked
                                        ( ground(T) -> guard_goal(V, T, G), Gs = [G] ; Gs = [] )
                                   ; throw(error(type_conflict(existing(K), required(T)), typecheck)) )
                                 ; add_known_type(V, T),
                                   ( ground(T) -> guard_goal(V, T, G), Gs = [G] ; Gs = [] ) )
                          ; check_value(V, T, St),
                            ( St == ok -> Gs = []
                            ; St == mismatch -> throw(error(literal_type_mismatch(V, T), typecheck))
                            ; ground(T) -> guard_goal(V, T, G), Gs = [G]
                            ; Gs = [] ) ).

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
list_elem_type(L, ET) :- is_list(L), L = [E|Es],
                         value_single_type(E, ET), ground(ET),
                         forall(member(E2, Es), ( value_single_type(E2, T2), T2 == ET )).

add_known_types(V, Cs) :- maplist(add_known_type(V), Cs).

set_out_type(Out, OT) :- ( var(Out), nonvar(OT), \+ wildcard_type_t(OT) -> add_known_type(Out, OT)
                                                                          ; true ).

%When F/N has exactly one declared output type, the call result is that type:
set_unique_decl_out(F, N, Out) :- ( atom(F), findall(OT, fn_decl_arity(F, N, _, OT), [OT1])
                                    -> set_out_type(Out, OT1) ; true ).

%Call-site output typing. An output type variable that occurs in no argument
%type is universally quantified - by parametricity only a bottom function
%like (: empty (-> $a)) can implement it - so the result is compatible with
%every requirement, without assigning a concrete type or emitting a guard:
set_call_out_type(Out, ATs, OT) :- ( nonvar(OT) -> set_out_type(Out, OT)
                                   ; var(Out), term_variables(ATs, Vs), \+ memberchk_eq(OT, Vs)
                                     -> add_known_type(Out, OT)
                                      ; true ).

%A call is statically dead when the same variable occupies two argument
%positions whose required types can never both hold (e.g. (num-str $x $x)):
same_call_var_conflict([V|Vs], [T|Ts]) :- ( var(V), nonvar(T), \+ wildcard_type_t(T),
                                            var_conflict_in_rest(V, T, Vs, Ts) -> true
                                          ; same_call_var_conflict(Vs, Ts) ).

var_conflict_in_rest(V, T, [V2|Vs], [T2|Ts]) :- ( V == V2, nonvar(T2), \+ wildcard_type_t(T2),
                                                  \+ type_compat_soft(T, T2) -> true
                                                ; var_conflict_in_rest(V, T, Vs, Ts) ).

%%% Static typing of values (translated call results, literals, closures):
value_candidate_types(V, ['Number']) :- number(V), !.
value_candidate_types(V, ['String']) :- string(V), !.
value_candidate_types(true, ['Bool']) :- !.
value_candidate_types(false, ['Bool']) :- !.
value_candidate_types(V, Cs) :- atom(V), !,
                                findall(T, declared_value_type(V, T), Vs),
                                findall([H|Xs], ( declared_fn_type(V, ATs, OT, Det),
                                                  det_arrow_head(Det, H),
                                                  append(ATs, [OT], Xs) ), Fs),
                                append(Vs, Fs, Cs0),
                                ( Cs0 == [], current_arithmetic_function(V)
                                  -> Cs = ['Number']                %arithmetic constants: inf, nan, pi, e
                                   ; Cs = Cs0 ).
value_candidate_types(partial(F, B), Cs) :- !,
                                length(B, N),
                                findall([H|Xs], ( fn_decl_partial(F, N, PTs, RTs, OT, Det),
                                                  det_arrow_head(Det, H),
                                                  bound_args_match(B, PTs),
                                                  append(RTs, [OT], Xs) ), Cs).
value_candidate_types([], [['List', _]]) :- !.
%A constructor application (STV 0.5 0.8) has the constructor's output type,
%but only when its fields do not contradict the constructor's signature -
%otherwise the value is unknown and the (runtime or strict) guard decides:
value_candidate_types([H|Args], Cs) :- atom(H), length(Args, N), fn_decl_arity(H, N, _, _), !,
                                findall(OT, ( fn_decl_arity(H, N, ATs, OT),
                                              bound_args_match(Args, ATs) ), Cs).
value_candidate_types(V, Cs) :- is_list(V), maplist(value_single_type, V, Ts), !, Cs = [Ts].
value_candidate_types(_, []).

value_single_type(V, T) :- ( var(V) -> known_singleton(V, T)
                                     ; value_candidate_types(V, [T0]), T = T0 ).

det_arrow_head(nondet, '-[nondet]->') :- !.
det_arrow_head(_, (->)).

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
%is an untagged pair, handled by the next clause:
check_value(V, T, St) :- T = [Tag|FieldTs], user_atom_type(Tag), !,
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
check_value(V, T, St) :- atom(T), !,
                         value_candidate_types(V, Cs),
                         ( Cs == [] -> St = unknown
                         ; member(C, Cs), type_unify(C, T) -> St = ok
                         ; member(C, Cs), refinement_pair(C, T) -> St = unknown
                         ; St = mismatch ).
check_value(_, _, unknown).

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
%Inference makes no determinism claim, so under --strict-det inferred arrows
%are conservatively nondet - declare the function to commit it:
inferred_arrow_head(H) :- ( strict_det(true) -> H = '-[nondet]->' ; H = (->) ).

inferred_value_candidates(V, Cs) :- atom(V), !,
                                    inferred_arrow_head(H),
                                    findall([H|Xs], ( inferred_fn_type(V, ATs, OT),
                                                      append(ATs, [OT], Xs) ), Cs).
inferred_value_candidates(partial(F, B), Cs) :- !,
                                    length(B, N),
                                    inferred_arrow_head(H),
                                    findall([H|Xs], ( inferred_fn_type(F, ATs, OT),
                                                      length(ATs, Total), Total > N,
                                                      length(PTs, N), append(PTs, RTs, ATs),
                                                      bound_args_match(B, PTs),
                                                      append(RTs, [OT], Xs) ), Cs).
inferred_value_candidates(_, []).

%Slow completion of the primitive fast paths above:
prim_mismatch_status(P, T, St) :- ( wildcard_type_t(T) -> St = ok
                                  ; is_union(T) -> ( T = ['|'|Ms], member(M, Ms), type_compat_soft(P, M)
                                                     -> St = ok ; St = mismatch )
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

%%% Effectful call-site argument checking, one arg. Mode 'declared' throws on
%%% literal mismatches; mode 'inferred' only ever adds knowledge and guards:
check_call_arg(Mode, Fun, AV, T, Gs) :- ( var(AV)
                                          -> ( known_singleton(AV, K)
                                               -> ( nonvar(T), wildcard_type_t(T) -> Gs = []  %wildcards carry no knowledge
                                                  ; type_unify(K, T) -> Gs = []
                                                  ; taint_assumption(AV),  %known conflict: runtime error carries the value
                                                    type_guard(Fun, AV, T, Gs) )
                                             ; var(T) -> Gs = []
                                             ; wildcard_type_t(T) -> Gs = []
                                             ; type_guard(Fun, AV, T, Gs) )
                                        ; check_value(AV, T, St),
                                          ( St == ok -> Gs = []
                                          ; St == mismatch
                                            -> ( Mode == declared
                                                 -> throw(error(literal_type_mismatch(AV, T), typecheck))
                                                  ; type_guard(Fun, AV, T, Gs) )
                                          ; type_guard(Fun, AV, T, Gs) ) ).

type_guard(Fun, AV, T, Gs) :- ( ground(T), \+ wildcard_type_t(T)
                                -> ( strict_mode(true)
                                     -> throw(error(strict_runtime_typecheck(Fun, typecheck_or_error(AV, T)), typecheck))
                                      ; guard_goal(AV, T, G), Gs = [G] )
                                 ; Gs = [] ).

%Inline the primitive fast path into the compiled goal so hot code only pays a
%native type test; the reflective check runs only when that test fails:
guard_goal(AV, 'Number', ( number(AV) -> true ; typecheck_or_error(AV, 'Number') )) :- !.
guard_goal(AV, 'String', ( string(AV) -> true ; typecheck_or_error(AV, 'String') )) :- !.
guard_goal(AV, 'Bool', ( ( AV == true ; AV == false ) -> true ; typecheck_or_error(AV, 'Bool') )) :- !.
guard_goal(AV, T, typecheck_or_error(AV, T)).

apply_call_args(Mode, Fun, AVs, ATs, Gs) :- ( same_call_var_conflict(AVs, ATs) -> Gs = [fail]
                                            ; maplist(check_call_arg(Mode, Fun), AVs, ATs, Gss),
                                              append(Gss, Gs) ).

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
                         is_list(V),
                         runtime_list_ok(V, ET).
runtime_type_ok(V, T) :- is_arrow_type(T), !, \+ value_definitely_mismatch(V, T).
runtime_type_ok(V, T) :- is_union(T), !, T = ['|'|Ms],
                         member(M, Ms), runtime_type_ok(V, M), !.
runtime_type_ok(V, T) :- T = [Tag|FieldTs], user_atom_type(Tag), !,
                         is_list(V), V = [VTag|Fields], VTag == Tag,
                         same_length(Fields, FieldTs),
                         runtime_tuple_ok(Fields, FieldTs).
runtime_type_ok(V, T) :- is_list(T), !,
                         is_list(V), same_length(V, T),
                         runtime_tuple_ok(V, T).
%get-type is user-extensible and extensions may call typechecked code, so a
%guard reached from within a get-type call must not recurse into get-type:
runtime_type_ok(_, _) :- nb_current('$in_typecheck', true), !.
runtime_type_ok(V, T) :- setup_call_cleanup(nb_setval('$in_typecheck', true),
                                            ( 'get-type'(V, T) *-> true ; 'get-metatype'(V, T) ),
                                            nb_setval('$in_typecheck', false)).

runtime_list_ok([], _).
runtime_list_ok([E|Es], ET) :- ( var(E) -> true ; runtime_type_ok(E, ET) ),
                               runtime_list_ok(Es, ET).

runtime_tuple_ok([], []).
runtime_tuple_ok([F|Fs], [T|Ts]) :- ( var(F) -> true ; runtime_type_ok(F, T) ),
                                    runtime_tuple_ok(Fs, Ts).

constrain_var_type(V, T) :- ( get_attr(V, mreq, Rs)
                              -> ( member(R, Rs), \+ type_compat_soft(R, T) -> fail
                                 %ground duplicates add nothing; nonground variants are NOT
                                 %duplicates - their type vars can be bound independently:
                                 ; ground(T), memberchk(T, Rs) -> true
                                 ; put_attr(V, mreq, [T|Rs]) )
                               ; put_attr(V, mreq, [T]) ).

value_definitely_mismatch(V, T) :- copy_term(T, T2), check_value(V, T2, St), !, St == mismatch.

goal_or_throw(Goal, Error) :- ( call(Goal) *-> true ; throw(Error) ).

%%% Clause-level helpers %%%

%Bind declared parameter types onto clause-head variables. For an overloaded
%function, the clause's head patterns filter the declarations: a clause whose
%head selects exactly one overload is checked against it, a clause no overload
%can produce is rejected, and a genuinely ambiguous clause (e.g. all-variable
%head serving every overload) stays unchecked as before.
clause_param_types(F, Args, DeclOut) :- length(Args, N),
                                        findall(ATs-OTx, fn_decl_arity(F, N, ATs, OTx), Decls),
                                        ( Decls == [] -> DeclOut = none
                                        ; Decls = [ATs1-OT] -> maplist(bind_param_type, Args, ATs1), DeclOut = out(OT)
                                        ; include(clause_head_survives(Args), Decls, Survivors),
                                          ( Survivors == [] -> throw(error(no_matching_overload(F), typecheck))
                                          ; Survivors = [ATs1-OT] -> maplist(bind_param_type, Args, ATs1),
                                                                     DeclOut = out(OT)
                                          ; DeclOut = none ) ).

clause_head_survives(Args, ATs-_) :- \+ \+ maplist(head_arg_soft, Args, ATs).
head_arg_soft(A, T) :- ( var(A) -> true
                       ; check_value(A, T, St) -> St \== mismatch
                       ; true ).

bind_param_type(Arg, T) :- ( var(Arg) -> ( nonvar(T), \+ wildcard_type_t(T) -> add_known_type(Arg, T)
                                                                             ; true )
                           ; list_type(T, ET), Arg = [H|Rest]
                             -> bind_param_type(H, ET),          %type element vars of list patterns
                                bind_param_type(Rest, ['List', ET])
                           ; is_union(T)                        %clause heads narrow union params
                             -> bind_pattern_typed(Arg, T)
                           ; structural_pattern_fields(Arg, T, Fields, FieldTs)
                             -> maplist(bind_param_type, Fields, FieldTs)
                           ; is_list(Arg), is_list(T), same_length(Arg, T),
                             \+ is_arrow_type(T)                 %untagged tuple types: ($v Number)
                             -> maplist(bind_param_type, Arg, T)
                           ; check_value(Arg, T, St),
                             ( St == mismatch -> throw(error(literal_type_mismatch(Arg, T), typecheck))
                                               ; true ) ).

%Which union member does a pattern's shape select?
pattern_selects_member(P, M) :- nonvar(M), nonvar(P),
                                ( list_type(M, _) -> ( P == [] ; P = [C|_], C == cons ; is_list(P) )
                                ; atom(M) -> \+ \+ structural_pattern_fields(P, M, _, _)
                                ; is_list(M), is_list(P)
                                  -> ( M = [Tag|FTs], user_atom_type(Tag)
                                       -> P = [Tag2|Fs], Tag2 == Tag, same_length(Fs, FTs)
                                        ; same_length(P, M) )
                                ; fail ).

%A tagged pattern (Tag P1 ... Pn) against either the structural tuple type
%(Tag T1 ... Tn) or a nominal type produced by Tag's constructor declaration:
structural_pattern_fields(Arg, T, Fields, FieldTs) :- is_list(Arg), Arg = [Tag|Fields], atom(Tag), nonvar(T),
                                                      ( T = [Tag2|FieldTs], Tag2 == Tag,
                                                        same_length(Fields, FieldTs) -> true
                                                      ; atom(T), length(Fields, N),
                                                        findall(ATs-OT, fn_decl_arity(Tag, N, ATs, OT), [FieldTs-OT1]),
                                                        type_compat_soft(OT1, T) ).

%Contextual output typing for deliberately-undeclared builtins (one clause per
%builtin; the translator consults this after translating an undeclared call):
untyped_call_out(cons, [H, Tl], Out) :- cons_out_type(H, Tl, Out).
untyped_call_out('cons-atom', [H, Tl], Out) :- cons_out_type(H, Tl, Out).
untyped_call_out('union-atom', [A, B], Out) :- union_atom_out_type(A, B, Out).
untyped_call_out(append, [A, B], Out) :- union_atom_out_type(A, B, Out).
untyped_call_out('subtraction-atom', [A, _], Out) :- first_list_out_type(A, Out).
untyped_call_out(list_to_set, [A], Out) :- first_list_out_type(A, Out).

%Element-filtering builtins preserve their first argument's list type; the
%other operand may be any expression:
first_list_out_type(A, Out) :- ( var(Out), union_side_elem(A, T)
                                 -> set_out_type(Out, ['List', T]) ; true ).

%cons stays undeclared (a global (List $a) signature would reject legal
%heterogeneous expressions), but when the head provably fits the tail's list
%type the result is known to be that list type:
cons_out_type(H, Tl, Out) :- ( var(Out),
                               ( var(Tl) -> known_singleton(Tl, TT), list_type(TT, T)
                               ; Tl == [] -> true
                               ; list_elem_type(Tl, T) ),
                               ( wildcard_type_t(T) -> true    %(List %Undefined%): any head fits
                               ; var(H) -> known_singleton(H, K), type_unify(K, T)
                                         ; check_value(H, T, St), St == ok )
                               -> set_out_type(Out, ['List', T])
                                ; true ).

%union-atom likewise stays undeclared, but concatenating two provably
%compatible lists yields that list type:
union_atom_out_type(A, B, Out) :- ( var(Out),
                                    union_side_elem(A, TA),
                                    union_side_elem(B, TB),
                                    type_unify(TA, TB)
                                    -> set_out_type(Out, ['List', TA])
                                     ; true ).

union_side_elem(X, T) :- ( var(X) -> known_singleton(X, K), list_type(K, T)
                         ; X == [] -> true
                         ; list_elem_type(X, T) ).

%Destructuring bindings: type a pattern's variables from the bound value's
%known type, e.g. (let (Stats $sum $sq $n) (make-stats) ...):
bind_pattern_from(Pat, Val) :- ( nonvar(Pat),
                                 ( var(Val) -> known_singleton(Val, KT)
                                             ; value_single_type(Val, KT) )
                                 -> bind_pattern_typed(Pat, KT)
                                  ; true ).

%Tolerant variant used where a non-matching pattern must not fail or throw
%(case branches: a wrong pattern just never matches at runtime):
bind_pattern_typed(P, T) :- ( var(P) -> ( nonvar(T), \+ wildcard_type_t(T) -> add_known_type(P, T) ; true )
                            ; is_union(T), T = ['|'|Ms]        %a pattern narrows to the member it selects
                              -> ( findall(M, ( member(M, Ms), pattern_selects_member(P, M) ), [M1])
                                   -> bind_pattern_typed(P, M1) ; true )
                            ; list_type(T, ET), P = [C, H, R], C == cons
                              -> bind_pattern_typed(H, ET),    %source-form (cons H R) destructuring
                                 bind_pattern_typed(R, ['List', ET])
                            ; list_type(T, ET), P = [H|Rest]
                              -> bind_pattern_typed(H, ET),
                                 bind_pattern_typed(Rest, ['List', ET])
                            ; structural_pattern_fields(P, T, Fields, FieldTs)
                              -> maplist(bind_pattern_typed, Fields, FieldTs)
                            ; is_list(P), is_list(T), same_length(P, T),
                              \+ is_arrow_type(T)
                              -> maplist(bind_pattern_typed, P, T)
                            ; true ).

%Check the clause body's inferred output type against the declared output type:
clause_output_goals(_, none, _, _, []) :- !.
clause_output_goals(F, out(OT), ExpOut, BodyExpr, Gs) :-
        ( var(OT) -> Gs = []
        ; wildcard_type_t(OT) -> Gs = []
        ; nonvar(BodyExpr), BodyExpr = [Q|_], Q == quote -> Gs = []
        ; var(ExpOut) ->
            ( known_candidates(ExpOut, Cs) ->
                ( member(C, Cs), \+ type_compat_soft(C, OT), \+ refinement_pair(C, OT)
                  -> throw(error(type_conflict(existing(C), required(OT)), typecheck))
                ; member(C, Cs), \+ type_compat_soft(C, OT)
                  -> type_guard(F, ExpOut, OT, Gs)            %possible runtime refinement
                   ; Gs = [] )
            ; type_guard(F, ExpOut, OT, Gs) )
        ; check_value(ExpOut, OT, St),
          ( St == mismatch -> throw(error(literal_type_mismatch(ExpOut, OT), typecheck))
          ; St == unknown -> type_guard(F, ExpOut, OT, Gs)
          ; Gs = [] ) ).

%Strict mode: every compiled function needs a declared or inferred type
%(lambdas exempt). Checked after clause translation so inference can run first:
strict_check_function_typed(F, Args) :- ( strict_mode(true), \+ sub_atom(F, 0, _, _, 'lambda_')
                                          -> length(Args, N),
                                             ( fn_decl_arity(F, N, _, _) -> true
                                             ; inferred_decl_arity(F, N, _, _) -> true
                                             ; throw(error(strict_missing_function_type(F, N), typecheck)) )
                                           ; true ).

%%% Local type inference for undeclared functions %%%
%
% While an undeclared function's clause is translated, its variable parameters
% carry fresh assumption type variables; typed call sites in the body bind them
% by unification. A parameter whose assumption sees conflicting uses is tainted
% (no knowledge is recorded for it). The harvested types live in an internal
% store, are never asserted into &self, and are used only to *add* knowledge:
% eliminating guards, typing call outputs, and satisfying strict mode. Call
% sites of inferred functions never throw at compile time.
:- dynamic inferred_fn_type/3.     % inferred_fn_type(F, ArgTypes, OutType)

inferred_decl_arity(F, N, ATs, OT) :- inferred_fn_type(F, ATs, OT), length(ATs, N).

begin_clause_inference(F, Args, Assume, saved(OldA, OldD, OldT)) :-
        catch(b_getval('$assumptions', OldA), _, OldA = []),
        catch(b_getval('$assume_decl', OldD), _, OldD = none),
        catch(b_getval('$assump_taint', OldT), _, OldT = []),
        length(Args, N),
        ( \+ \+ fn_decl_arity(F, N, _, _) -> Assume = none, Pairs = [], Decl = none
                                           ; foldl(assume_param_type, Args, t([], []), t(PairsR, PTsR)),
                                             reverse(PairsR, Pairs), reverse(PTsR, PTs),
                                             Assume = assume(Pairs),
                                             Decl = d(F, N, PTs, _OutTv) ),
        b_setval('$assumptions', Pairs),
        b_setval('$assume_decl', Decl),
        b_setval('$assump_taint', []).

assume_param_type(Arg, t(Ps, Ts), t(Ps1, [T|Ts])) :- ( var(Arg)
                                                       -> ( known_singleton(Arg, T) -> Ps1 = Ps
                                                          ; add_known_type(Arg, T), Ps1 = [a(Arg, T)|Ps] )
                                                     ; value_single_type(Arg, T) -> Ps1 = Ps
                                                     ; Ps1 = Ps ).

taint_assumption(AV) :- ( catch(b_getval('$assumptions', Pairs), _, fail),
                          member(a(P, _), Pairs), P == AV
                          -> catch(b_getval('$assump_taint', Ts), _, Ts = []),
                             b_setval('$assump_taint', [AV|Ts])
                           ; true ).

end_clause_inference(F, Args, ExpOut, Assume, saved(OldA, OldD, OldT)) :-
        ( Assume = assume(Pairs) -> store_inferred_type(F, Pairs, Args, ExpOut) ; true ),
        b_setval('$assumptions', OldA),
        b_setval('$assume_decl', OldD),
        b_setval('$assump_taint', OldT).

%The provisional declaration of the clause being translated, for self-recursion:
assumed_self_decl(F, N, PTs, OutTv) :- catch(b_getval('$assume_decl', D), _, fail),
                                       D = d(F, N, PTs, OutTv).

store_inferred_type(F, Pairs, Args, ExpOut) :- catch(b_getval('$assump_taint', Taints), _, Taints = []),
                                               maplist(infer_param_type(Pairs, Taints), Args, ATs0),
                                               infer_out_type(ExpOut, OT0),
                                               maplist(normalize_inferred, ATs0, ATs),
                                               normalize_inferred(OT0, OT),
                                               ( member(T, [OT|ATs]), T \== '%Undefined%'
                                                 -> merge_inferred(F, ATs, OT) ; true ).

infer_param_type(Pairs, Taints, Arg, T) :- ( var(Arg) -> ( memberchk_eq(Arg, Taints) -> T = '%Undefined%'
                                                         ; member(a(P, Tv), Pairs), P == Arg -> T = Tv
                                                         ; known_singleton(Arg, K) -> T = K
                                                         ; T = '%Undefined%' )
                                           ; value_single_type(Arg, T0) -> T = T0
                                           ; T = '%Undefined%' ).

infer_out_type(Out, T) :- ( var(Out) -> ( known_singleton(Out, K) -> T = K ; T = '%Undefined%' )
                          ; value_single_type(Out, T0) -> T = T0
                          ; T = '%Undefined%' ).

%Only clearly usable shapes are recorded; everything else is no-knowledge:
normalize_inferred(T, '%Undefined%') :- var(T), !.
normalize_inferred(T, T) :- atom(T), !.
normalize_inferred(T, ['List', ETN]) :- ground(T), list_type(T, ET), !,
                                        normalize_inferred(ET, ETN).
normalize_inferred(T, T) :- ground(T), is_arrow_type(T), !.
normalize_inferred(_, '%Undefined%').

%Clauses of the same function are joined position-wise; disagreement widens:
merge_inferred(F, ATs, OT) :- length(ATs, N),
                              ( inferred_decl_arity(F, N, ATs0, OT0)
                                -> retract(inferred_fn_type(F, ATs0, OT0)),
                                   maplist(join_inferred, ATs0, ATs, ATs1),
                                   join_inferred(OT0, OT, OT1),
                                   ( member(T, [OT1|ATs1]), T \== '%Undefined%'
                                     -> assertz(inferred_fn_type(F, ATs1, OT1)) ; true )
                                 ; assertz(inferred_fn_type(F, ATs, OT)) ).

join_inferred(A, B, J) :- ( A =@= B -> J = A ; J = '%Undefined%' ).

%%% Determinism arrows (-[det]->, -[nondet]->) %%%

fn_determinism(F, N, Det) :- findall(D, ( declared_fn_type(F, ATs, _, D), length(ATs, N) ), Ds0),
                             sort(Ds0, Ds),
                             ( Ds == [] -> Det = unspecified
                             ; Ds = [D1] -> Det = D1
                             ; Ds == [det, unspecified] -> Det = unspecified
                             ; Ds == [nondet, unspecified] -> Det = nondet
                             ; throw(error(conflicting_determinism_declarations(F), determinism)) ).

validate_function_determinism(F, Args, BodyExpr, PrevClauses) :-
    length(Args, N),
    fn_determinism(F, N, Det),
    ( Det == det -> ensure_deterministic_expr(BodyExpr, F),
                    ensure_non_overlapping_clause_heads(F, Args, PrevClauses)
                  ; true ).

ensure_deterministic_expr(Expr, Fun) :- deterministic_expr(Expr, R),
                                        ( R == ok -> true
                                        ; R = nondeterministic(Reason)
                                          -> throw(error(determinism_conflict(Fun, Reason), determinism))
                                           ; throw(error(determinism_conflict(Fun, unknown(Expr)), determinism)) ).

%A clause whose body commits with (cut) never falls through to a later
%clause, so overlap with it cannot create a choicepoint:
ensure_non_overlapping_clause_heads(_, _, []).
ensure_non_overlapping_clause_heads(F, Args, [fun_meta(PrevArgs, PrevBody)|Rest]) :-
    ( clause_heads_overlap(Args, PrevArgs), \+ body_commits(PrevBody)
      -> throw(error(overlapping_deterministic_clauses(F, Args, PrevArgs), determinism))
       ; ensure_non_overlapping_clause_heads(F, Args, Rest) ).

body_commits(E) :- nonvar(E),
                   ( E = [C], C == cut -> true
                   ; is_list(E), member(X, E), body_commits(X) -> true
                   ; fail ).

clause_heads_overlap(ArgsA, ArgsB) :- copy_term((ArgsA, ArgsB), (CA, CB)),
                                      unifiable(CA, CB, _).

builtin_call_determinism(superpose, 1, nondet).
builtin_call_determinism(empty, 0, nondet).

function_call_determinism(F, N, Det) :- builtin_call_determinism(F, N, Det), !.
function_call_determinism(F, N, Det) :- catch(fn_determinism(F, N, Det), _, fail), !.
function_call_determinism(_, _, unspecified).

deterministic_expr(Expr, ok) :- ( var(Expr) ; atomic(Expr) ; Expr = partial(_, _) ), !.
%A variable head must not unify with the construct patterns below. Under
%--strict-det its known arrow type is a determinism commitment: a plain ->
%closure is deterministic, a -[nondet]-> closure is not.
deterministic_expr([Head|Args], Result) :- var(Head), !,
    ( Args == [] -> Result = ok                    %singleton ($x) is data, not application
    ; strict_det(true), known_singleton(Head, K), nonvar(K)
      -> ( K = [A|_], A == (->) -> combine_determinism_list(Args, Result)
         ; K = [A|_], A == '-[nondet]->' -> Result = nondeterministic(nondet_closure)
         ; nonfunction_type(K) -> combine_determinism_list(Args, Result)  %data construction
         ; Result = unknown(dynamic_head(Head)) )
       ; Result = unknown(dynamic_head(Head)) ).
deterministic_expr([collapse, _], ok) :- !.
deterministic_expr(['trace!', A, B], Result) :- !, combine_determinism_list([A, B], Result).
deterministic_expr([once, _], ok) :- !.
deterministic_expr([quote, _], ok) :- !.
deterministic_expr([eval, _], unknown(dynamic_eval)) :- !.
deterministic_expr([reduce, _], unknown(dynamic_reduce)) :- !.
deterministic_expr([call, Expr], Result) :- !, deterministic_call_expr(Expr, Result).
deterministic_expr([superpose|_], nondeterministic(superpose)) :- !.
deterministic_expr([match|_], nondeterministic(match)) :- !.
deterministic_expr([hyperpose|_], nondeterministic(hyperpose)) :- !.
deterministic_expr([translatePredicate|_], nondeterministic(translatePredicate)) :- !.
deterministic_expr([if, Cond, Then], Result) :- !, combine_determinism_list([Cond, Then], Result).
deterministic_expr([if, Cond, Then, Else], Result) :- !, combine_determinism_list([Cond, Then, Else], Result).
deterministic_expr([progn|Exprs], Result) :- !, combine_determinism_list(Exprs, Result).
deterministic_expr([prog1|Exprs], Result) :- !, combine_determinism_list(Exprs, Result).
deterministic_expr([let, Pat, Val, In], Result) :- !, combine_determinism_list([Pat, Val, In], Result).
deterministic_expr([chain, Pat, Val, In], Result) :- !, combine_determinism_list([Pat, Val, In], Result).
deterministic_expr(['let*', Binds, Body], Result) :- !, binds_and_body_determinism(Binds, Body, Result).
deterministic_expr([sealed, _, Expr], Result) :- !, deterministic_expr(Expr, Result).
deterministic_expr(['forall', _, _], ok) :- !.
deterministic_expr(['foldall', _, _, _], ok) :- !.
deterministic_expr(['foldl-atom', List, Init, _, _, Body], Result) :- !, combine_determinism_list([List, Init, Body], Result).
deterministic_expr(['map-atom', List, _, Body], Result) :- !, combine_determinism_list([List, Body], Result).
deterministic_expr(['filter-atom', List, _, Cond], Result) :- !, combine_determinism_list([List, Cond], Result).
deterministic_expr(['|->', _, _], ok) :- !.
deterministic_expr([case, KeyExpr, PairsExpr], Result) :- !, case_expr_determinism(KeyExpr, PairsExpr, Result).
deterministic_expr([Head|Args], Result) :- ( atomic(Head), ( \+ atom(Head) ; \+ fun(Head) )
                                           ; is_list(Head) ), !,
                                           combine_determinism_list([Head|Args], Result).
deterministic_expr([Head|Args], Result) :- atom(Head), !, deterministic_call_expr([Head|Args], Result).
deterministic_expr([Head|_], unknown(dynamic_head(Head))).

deterministic_call_expr([Fun|Args], Result) :- atom(Fun), !,
                                               length(Args, N),
                                               ( function_call_determinism(Fun, N, nondet)
                                                 -> Result = nondeterministic(call(Fun))
                                                  ; combine_determinism_list(Args, Result) ).
deterministic_call_expr(Expr, unknown(dynamic_call(Expr))).

combine_determinism_list([], ok).
combine_determinism_list([Expr|Exprs], Result) :- deterministic_expr(Expr, First),
                                                  ( First == ok -> combine_determinism_list(Exprs, Result)
                                                                 ; Result = First ).

binds_and_body_determinism([], Body, Result) :- deterministic_expr(Body, Result).
binds_and_body_determinism([[Pat, Val]|Rest], Body, Result) :-
    combine_determinism_list([Pat, Val], HeadResult),
    ( HeadResult == ok -> binds_and_body_determinism(Rest, Body, Result)
                        ; Result = HeadResult ).

case_expr_determinism(KeyExpr, PairsExpr, Result) :- deterministic_expr(KeyExpr, KeyResult),
                                                     ( KeyResult == ok -> case_pairs_determinism(PairsExpr, Result)
                                                                        ; Result = KeyResult ).

case_pairs_determinism([], ok).
case_pairs_determinism([[CaseExpr, BranchExpr]|Rest], Result) :-
    combine_determinism_list([CaseExpr, BranchExpr], PairResult),
    ( PairResult == ok -> case_pairs_determinism(Rest, Result)
                        ; Result = PairResult ).
