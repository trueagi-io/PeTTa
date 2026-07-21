%%%%%%%%%% Compile-time typechecking support (see AGENTS.md) %%%%%%%%%%
%
% One canonical type store, one compatibility relation (type_unify/2), and one
% type channel: attributed variables on the Prolog vars representing MeTTa vars.
%   tknown - translation-time inferred/declared type candidates of a variable
%   treq   - translation-time accumulated call-site requirements of a variable
%   mreq   - runtime type constraints placed on still-unbound values by guards
% Static errors are thrown during translation (never emitted and re-scanned).

:- dynamic declared_fn_type/4.     % declared_fn_type(F, ArgTypes, OutType, Det)
:- dynamic declared_value_type/2.  % declared_value_type(Name, Type)
:- dynamic strict_mode/1.

:- current_prolog_flag(argv, Argv),
   ( memberchk('--strict', Argv) -> assertz(strict_mode(true))
                                  ; assertz(strict_mode(false)) ).

%%% Arrow shape: prefix (-> A B) form:
fn_type_shape(Type, ArgTypes, OutType, unspecified) :- is_list(Type), Type = [Arrow|Xs],
                                                       Arrow == (->), !,
                                                       append(ArgTypes, [OutType], Xs).

%Normalize nested arrow types to canonical prefix form:
normalize_type(T, T) :- var(T), !.
normalize_type(T, T) :- atomic(T), !.
normalize_type(T, TN) :- is_list(T), fn_type_shape(T, ATs, OT, _), !,
                         maplist(normalize_type, ATs, ATN),
                         normalize_type(OT, OTN),
                         append(ATN, [OTN], Xs),
                         TN = [->|Xs].
normalize_type(T, TN) :- is_list(T), !, maplist(normalize_type, T, TN).
normalize_type(T, T).

%%% Store maintenance, called from add_sexp/remove_sexp and forget_symbol.
%%% Caching is idempotent so seeded builtins and imports do not duplicate:
maybe_cache_type_decl(Space, Term) :- ( Space == '&self', is_list(Term), Term = [C, Name, Type],
                                        C == (:), atom(Name)
                                        -> ( nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
                                             -> maplist(normalize_type, ATs, ATN),
                                                normalize_type(OT, OTN),
                                                ( declared_fn_type(Name, A2, O2, D2),
                                                  (A2-O2-D2) =@= (ATN-OTN-Det) -> true
                                                ; assertz(declared_fn_type(Name, ATN, OTN, Det)) )
                                              ; normalize_type(Type, TN),
                                                ( declared_value_type(Name, T2), T2 =@= TN -> true
                                                ; assertz(declared_value_type(Name, TN)) ) )
                                         ; true ).

%Seed the store with the builtin operator types (called once after loading):
seed_builtin_types :- library_path(Base),
                      atomic_list_concat([Base, '/lib_builtin_types.metta'], Path),
                      read_file_to_string(Path, S, []),
                      string_codes(S, Cs),
                      strip(Cs, 0, Codes),
                      phrase(top_forms(Forms, 1), Codes),
                      forall(member(form(FormStr), Forms),
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

forget_symbol_types(Name) :- retractall(declared_fn_type(Name, _, _, _)),
                             retractall(declared_value_type(Name, _)).

%%% Store lookup (each retrieval yields a fresh copy of the declaration):
fn_decl_arity(F, N, ATs, OT) :- declared_fn_type(F, ATs, OT, _), length(ATs, N).
fn_decl_partial(F, N, PTs, RTs, OT) :- declared_fn_type(F, ATs, OT, _),
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
type_unify(A, B) :- atom(A), !, A == B.
type_unify(A, B) :- is_list(A), !, is_list(B), same_length(A, B), maplist(type_unify, A, B).
type_unify(A, B) :- A == B.

type_compat_soft(A, B) :- \+ \+ type_unify(A, B).

is_arrow_type(T) :- nonvar(T), T = [A|_], A == (->).

%%% Attribute hooks (permissive merging; errors are raised by explicit checks):
tknown:attr_unify_hook(Cs, Other) :-
    ( var(Other) -> ( get_attr(Other, tknown, C2) -> variant_union(Cs, C2, U),
                                                     put_attr(Other, tknown, U)
                                                   ; put_attr(Other, tknown, Cs) )
                  ; true ).

treq:attr_unify_hook(_, _).

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
add_known_type(V, T) :- ( get_attr(V, tknown, Cs) -> ( variant_member(T, Cs) -> true
                                                                              ; put_attr(V, tknown, [T|Cs]) )
                                                   ; put_attr(V, tknown, [T]) ).

known_candidates(V, Cs) :- get_attr(V, tknown, Cs).
known_singleton(V, K) :- get_attr(V, tknown, [K]).

%Record the (statically known) type of a value flowing into Out via a branch:
note_value_candidate(Out, Val) :- ( var(Out), nonvar(Val), value_single_type(Val, VT)
                                    -> add_known_type(Out, VT) ; true ).

%Merge the known candidates of Val (a different variable) into Out:
note_var_candidates(Out, Val) :- ( var(Out), var(Val), known_candidates(Val, Cs)
                                   -> add_known_types(Out, Cs) ; true ).

add_known_types(_, []).
add_known_types(V, [C|Cs]) :- add_known_type(V, C), add_known_types(V, Cs).

set_out_type(Out, OT) :- ( var(Out), ground(OT), \+ wildcard_type_t(OT) -> add_known_type(Out, OT)
                                                                         ; true ).

%%% Translation-time requirement accumulation (detects statically dead calls):
add_required_type(V, T, Status) :- copy_term(T, T2),
                                   ( get_attr(V, treq, Rs)
                                     -> ( member(R, Rs), \+ type_compat_soft(R, T2) -> Status = dead
                                        ; put_attr(V, treq, [T2|Rs]), Status = ok )
                                      ; put_attr(V, treq, [T2]), Status = ok ).

%%% Static typing of values (translated call results, literals, closures):
value_candidate_types(V, ['Number']) :- number(V), !.
value_candidate_types(V, ['String']) :- string(V), !.
value_candidate_types(true, ['Bool']) :- !.
value_candidate_types(false, ['Bool']) :- !.
value_candidate_types(V, Cs) :- atom(V), !,
                                findall(T, declared_value_type(V, T), Vs),
                                findall([->|Xs], ( declared_fn_type(V, ATs, OT, _),
                                                   append(ATs, [OT], Xs) ), Fs),
                                append(Vs, Fs, Cs0),
                                ( Cs0 == [], catch(X is V, _, fail), number(X)
                                  -> Cs = ['Number']                %arithmetic constants: inf, nan, pi, e
                                   ; Cs = Cs0 ).
value_candidate_types(partial(F, B), Cs) :- !,
                                length(B, N),
                                findall([->|Xs], ( fn_decl_partial(F, N, PTs, RTs, OT),
                                                   bound_args_match(B, PTs),
                                                   append(RTs, [OT], Xs) ), Cs).
value_candidate_types([H|Args], Cs) :- atom(H), length(Args, N), fn_decl_arity(H, N, _, _), !,
                                findall(OT, fn_decl_arity(H, N, _, OT), Cs).
value_candidate_types(V, Cs) :- is_list(V), maplist(value_single_type, V, Ts), !, Cs = [Ts].
value_candidate_types(_, []).

value_single_type(V, T) :- ( var(V) -> known_singleton(V, T)
                                     ; value_candidate_types(V, [T0]), T = T0 ).

bound_args_match(B, PTs) :- \+ \+ maplist(arg_soft_ok, B, PTs).

%%% check_value(+Value, ?Type, -Status): Status in {ok, mismatch, unknown}.
%%% Binds type variables in Type on success (polymorphism resolution).
check_value(V, T, St) :- var(T), !, ( value_single_type(V, VT) -> T = VT ; true ), St = ok.
check_value(_, T, St) :- wildcard_type_t(T), !, St = ok.
check_value(V, T, St) :- T = [L, ET], L == 'List', !,
                         ( is_list(V) -> list_elems_status(V, ET, St)
                         ; non_list(V) -> St = mismatch
                         ; St = unknown ).
check_value(V, T, St) :- is_arrow_type(T), !,
                         ( ( atom(V) ; V = partial(_, _) )
                           -> value_candidate_types(V, Cs),
                              ( Cs == [] -> St = unknown
                              ; member(C, Cs), type_unify(C, T) -> St = ok
                              ; St = mismatch )
                         ; ( number(V) ; string(V) ) -> St = mismatch
                         ; St = unknown ).
check_value(V, T, St) :- atom(T), !,
                         value_candidate_types(V, Cs),
                         ( Cs == [] -> St = unknown
                         ; member(C, Cs), type_unify(C, T) -> St = ok
                         ; member(C, Cs), refinement_pair(C, T) -> St = unknown
                         ; St = mismatch ).
check_value(_, _, unknown).

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

%%% Effectful call-site argument checking, one arg (throws / emits guards):
check_call_arg(Fun, AV, T, Gs) :-
    ( var(AV) -> ( known_singleton(AV, K)
                   -> ( \+ \+ type_unify(K, T) -> type_unify(K, T), Gs = []
                      ; type_guard(Fun, AV, T, Gs) )              %known conflict: runtime error carries the value
                 ; var(T) -> Gs = []
                 ; wildcard_type_t(T) -> Gs = []
                 ; add_required_type(AV, T, S),
                   ( S == dead -> Gs = [fail]                     %requirements can never be met jointly
                   ; type_guard(Fun, AV, T, Gs) ) )
    ; check_value(AV, T, St),
      ( St == ok -> Gs = []
      ; St == mismatch -> throw(error(literal_type_mismatch(AV, T), typecheck))
      ; type_guard(Fun, AV, T, Gs) ) ).

type_guard(Fun, AV, T, Gs) :- ( ground(T), \+ wildcard_type_t(T)
                                -> ( strict_mode(true)
                                     -> throw(error(strict_runtime_typecheck(Fun, typecheck_or_error(AV, T)), typecheck))
                                      ; Gs = [typecheck_or_error(AV, T)] )
                                 ; Gs = [] ).

apply_decl_args(Fun, AVs, ATs, Gs) :- foldl(apply_decl_arg(Fun), AVs, ATs, [], GsR),
                                      reverse(GsR, GsL), append(GsL, Gs).
apply_decl_arg(Fun, AV, T, Acc, [G|Acc]) :- check_call_arg(Fun, AV, T, G).

%%% Runtime residual guards (only emitted where types stay unresolved).
%%% Bound values are checked via the user-extensible get-type reflection, so
%%% runtime refinement types (see examples/types_dependent.metta) keep working:
typecheck_or_error(V, T) :- ( var(V) -> constrain_var_type(V, T)
                            ; runtime_type_ok(V, T) -> true
                            ; throw(error(literal_type_mismatch(V, T), typecheck)) ).

%Non-throwing variant used inside overload dispatch branches:
typecheck_match(V, T) :- ( var(V) -> constrain_var_type(V, T)
                                   ; runtime_type_ok(V, T) ).

runtime_type_ok(_, T) :- var(T), !.
runtime_type_ok(_, T) :- wildcard_type_t(T), !.
runtime_type_ok(V, T) :- T = [L, ET], L == 'List', !,
                         is_list(V),
                         runtime_list_ok(V, ET).
runtime_type_ok(V, T) :- is_arrow_type(T), !, \+ value_definitely_mismatch(V, T).
%get-type is user-extensible and extensions may call typechecked code, so a
%guard reached from within a get-type call must not recurse into get-type:
runtime_type_ok(_, _) :- catch(nb_getval('$in_typecheck', true), _, fail), !.
runtime_type_ok(V, T) :- setup_call_cleanup(nb_setval('$in_typecheck', true),
                                            ( 'get-type'(V, T) *-> true ; 'get-metatype'(V, T) ),
                                            nb_setval('$in_typecheck', false)).

runtime_list_ok([], _).
runtime_list_ok([E|Es], ET) :- ( var(E) -> true ; runtime_type_ok(E, ET) ),
                               runtime_list_ok(Es, ET).

constrain_var_type(V, T) :- ( get_attr(V, mreq, Rs)
                              -> ( member(R, Rs), \+ type_compat_soft(R, T) -> fail
                                 ; put_attr(V, mreq, [T|Rs]) )
                               ; put_attr(V, mreq, [T]) ).

value_definitely_mismatch(V, T) :- copy_term(T, T2), check_value(V, T2, St), !, St == mismatch.

goal_or_throw(Goal, _) :- call(Goal).
goal_or_throw(Goal, Error) :- \+ once(Goal), throw(Error).

%%% Clause-level helpers %%%

%Bind declared parameter types onto clause-head variables (single decl only):
clause_param_types(F, Args, out(OT)) :- length(Args, N),
                                        findall(ATs-OTx, fn_decl_arity(F, N, ATs, OTx), [ATs1-OT]), !,
                                        maplist(bind_param_type, Args, ATs1).
clause_param_types(_, _, none).

bind_param_type(Arg, T) :- ( var(Arg) -> ( nonvar(T), \+ wildcard_type_t(T) -> add_known_type(Arg, T)
                                                                             ; true )
                           ; check_value(Arg, T, St),
                             ( St == mismatch -> throw(error(literal_type_mismatch(Arg, T), typecheck))
                                               ; true ) ).

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
              -> output_guard(F, ExpOut, OT, Gs)              %possible runtime refinement
               ; Gs = [] )
        ; output_guard(F, ExpOut, OT, Gs) )
    ; check_value(ExpOut, OT, St),
      ( St == mismatch -> throw(error(literal_type_mismatch(ExpOut, OT), typecheck))
      ; St == unknown -> output_guard(F, ExpOut, OT, Gs)
      ; Gs = [] ) ).

output_guard(F, Out, OT, Gs) :- ( ground(OT)
                                  -> ( strict_mode(true)
                                       -> throw(error(strict_runtime_typecheck(F, typecheck_or_error(Out, OT)), typecheck))
                                        ; Gs = [typecheck_or_error(Out, OT)] )
                                   ; Gs = [] ).

%Strict mode: every compiled function needs a declared type (lambdas exempt):
strict_check_function_typed(F, Args) :-
    ( strict_mode(true), \+ sub_atom(F, 0, _, _, 'lambda_')
      -> length(Args, N),
         ( \+ \+ fn_decl_arity(F, N, _, _) -> true
                                            ; throw(error(strict_missing_function_type(F, N), typecheck)) )
       ; true ).
