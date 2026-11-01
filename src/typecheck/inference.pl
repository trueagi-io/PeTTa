%%% Undeclared-function inference and parametric declaration validation.
%
%%% The promised type variables of the clause being compiled: a declaration's
%%% argument type variables are the caller's choice, so while the body is
%%% compiled (1) nothing it reads may be discharged against one
%%% (indefinite_candidate/1), and (2) the compiler may not pin one by guessing,
%%% as translate_closure_call/5 does for an unknown head. Scoped by b_setval so
%%% an abandoned nested compile cannot leak its set.
param_promises_scope(Promises, Outer) :- ( nb_current('$param_promises', Outer) -> true ; Outer = [] ),
                                         b_setval('$param_promises', Promises).

param_promises_restore(Outer) :- b_setval('$param_promises', Outer).

param_promise_var(V) :- var(V),
                        nb_current('$param_promises', Vs),
                        memberchk_eq(V, Vs).

%After the body is translated, every snapshotted position must still be
%unbound or a wildcard; a body that forced one to a concrete type makes the
%declaration dishonest:
parametric_param_check(F, Vars) :- forall(member(T, Vars),
                                          ( var(T) -> true
                                          ; wildcard_type(T) -> true
                                          ; throw(error(non_parametric_param(F, T), typecheck)) )).

%Only concrete type evidence makes a bottom body a dishonest parametric
%declaration; the marker, a wrapped literal or an open variable state nothing.
parametric_output_check(F, ExpOut) :-
    ( var(ExpOut)
      -> ( known_candidates(ExpOut, Cs), member(C, Cs),
           candidate_evidence(C, type(_))
           -> throw(error(non_parametric_output(F), typecheck))
            ; true )
       ; throw(error(non_parametric_output(F), typecheck)) ).

%Snapshot every type variable still open after head-pattern binding, including
%nested ones, for the post-body check above.
parametric_param_snapshot(out(_, ATs), Vars) :- !, term_variables(ATs, Vars).
parametric_param_snapshot(_, []).

%Strict mode: every compiled function needs a declared or inferred type
%(lambdas exempt). Checked after clause translation so inference can run first:
strict_check_function_typed(F, Args) :- ( strict_mode(true), \+ sub_atom(F, 0, _, _, 'lambda_')
                                          -> length(Args, N),
                                             ( fn_decl_arity(F, N, _, _) -> true
                                             ; inferred_decl_arity(F, N, _, _) -> true
                                             ; throw(error(strict_missing_function_type(F, N), typecheck)) )
                                           ; true ).

%%% Local type inference for undeclared functions. While a clause is
%%% translated, its parameters (including destructured pattern variables)
%%% carry fresh assumption type variables that typed call sites bind; a
%%% parameter seeing conflicting uses is tainted. The harvested types are an
%%% internal store used only to add knowledge: eliminating guards, typing call
%%% outputs, satisfying strict mode. Call sites of inferred functions demand an
%%% inferred type only where a value is visibly of another (check_call_arg/5).
:- dynamic inferred_fn_type/3.     % inferred_fn_type(F, ArgTypes, OutType)

inferred_decl_arity(F, N, ATs, OT) :- inferred_fn_type(F, ATs, OT), length(ATs, N).

begin_clause_inference(F, Args, Assume, saved(OldA, OldD, OldT)) :-
        ( nb_current('$assumptions', OldA) -> true ; OldA = [] ),
        ( nb_current('$assume_decl', OldD) -> true ; OldD = none ),
        ( nb_current('$assump_taint', OldT) -> true ; OldT = [] ),
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
                                                     ; value_single_type(Arg, T)
                                                       -> ctor_pattern_field_types(Arg), Ps1 = Ps
                                                     ; is_list(Arg) -> assume_pattern_vars(Arg, Ps, Ps1)
                                                     ; Ps1 = Ps ).

assume_pattern_vars([], Ps, Ps).
assume_pattern_vars([A|As], Ps0, Ps) :- ( var(A) -> ( known_singleton(A, _) -> Ps1 = Ps0
                                                    ; add_known_type(A, Tv), Ps1 = [a(A, Tv)|Ps0] )
                                        ; is_list(A) -> assume_pattern_vars(A, Ps0, Ps1)
                                        ; Ps1 = Ps0 ),
                                        assume_pattern_vars(As, Ps1, Ps).

%A pattern headed by a uniquely declared constructor (\+ fun(Tag), as in
%member_ctor/3) types its fields from that declaration: (: P (-> Number Number
%Pair)) makes the $a and $b of (= (f (P $a $b)) ...) Numbers. A function-headed
%pattern is an inverted call whose declaration says nothing about its
%variables, and a contradicting literal field is left to the head match.
ctor_pattern_field_types(Arg) :- ( functional_pattern_application(Arg, _, _)
                                   -> ( functional_pattern_signature(Arg, _ResultType,
                                                                     PatternArgs, ArgTypes)
                                        -> catch(maplist(bind_param_type,
                                                        PatternArgs, ArgTypes),
                                                 error(literal_type_mismatch(_, _),
                                                       typecheck),
                                                 true)
                                      ; true )
                                  ; is_list(Arg), Arg = [Tag|Fs], atom(Tag), \+ fun(Tag), Fs \== [],
                                   length(Fs, N), unique_fn_decl(Tag, N, ATs1, _)
                                   -> catch(maplist(bind_param_type, Fs, ATs1),
                                            error(literal_type_mismatch(_, _), typecheck), true)
                                    ; true ).

taint_assumption(AV) :- ( nb_current('$assumptions', Pairs),
                          member(a(P, _), Pairs), P == AV
                          -> ( nb_current('$assump_taint', Ts) -> true ; Ts = [] ),
                             b_setval('$assump_taint', [AV|Ts])
                           ; true ).

end_clause_inference(F, Args, ExpOut, Assume, saved(OldA, OldD, OldT)) :-
        ( Assume = assume(Pairs) -> store_inferred_type(F, Pairs, Args, ExpOut) ; true ),
        b_setval('$assumptions', OldA),
        b_setval('$assume_decl', OldD),
        b_setval('$assump_taint', OldT).

%The provisional declaration of the clause being translated, for self-recursion:
assumed_self_decl(F, N, PTs, OutTv) :- nb_current('$assume_decl', D),
                                       D = d(F, N, PTs, OutTv).

store_inferred_type(F, Pairs, Args, ExpOut) :- ( nb_current('$assump_taint', Taints) -> true ; Taints = [] ),
                                               inferred_clause_arg_types(Pairs, Taints, Args, ATs),
                                               infer_out_type(ExpOut, OT0),
                                               normalize_inferred_output(OT0, OT),
                                               ( member(T, [OT|ATs]), T \== '%Undefined%'
                                                 -> merge_inferred(F, ATs, OT) ; true ).

inferred_clause_arg_types(Pairs, Taints, Args, ATs) :-
    maplist(infer_param_type(Pairs, Taints), Args, Raw),
    maplist(normalize_inferred_param, Raw, Normalized),
    maplist(pattern_type_roundtrip, Args, Normalized, ATs).

%A structural parameter type is stored only if it still accepts the pattern it
%was read off: (Statement $s $p) under (: Statement Type) infers
%(Type Number Number), which reads back as a tagged shape no matching value
%has.
pattern_type_roundtrip(Arg, T, TN) :- ( \+ is_list(Arg) -> TN = T
                                      ; T \== '%Undefined%', \+ \+ check_value(Arg, T, ok) -> TN = T
                                      ; TN = '%Undefined%' ).

%A destructuring parameter's type is its pattern with each field replaced by
%its inferred type: (stv $s $c) used as numbers is (stv Number Number). The
%fields' body guards were elided, so call sites need this shape to check.
infer_param_type(Pairs, Taints, Arg, T) :- ( var(Arg) -> ( memberchk_eq(Arg, Taints) -> T = '%Undefined%'
                                                         ; member(a(P, Tv), Pairs), P == Arg -> T = Tv
                                                         ; known_singleton(Arg, K) -> T = K
                                                         ; T = '%Undefined%' )
                                           ; value_single_type(Arg, T0) -> T = T0
                                           ; is_list(Arg), Arg = [Tag|Fs], atom(Tag), Fs \== [],
                                             maplist(infer_param_type(Pairs, Taints), Fs, FTs)
                                             -> T = [Tag|FTs]
                                           ; T = '%Undefined%' ).

infer_out_type(Out, T) :- ( var(Out) -> ( known_singleton(Out, K) -> T = K ; T = '%Undefined%' )
                          ; value_single_type(Out, T0) -> T = T0
                          ; Out = [Tag|Fields], atom(Tag), Fields \== [], is_list(Fields),
                            maplist(infer_out_type, Fields, Types),
                            tagged_tuple_type([Tag|Types], Tag, Types),
                            maplist(fully_known_inferred_type, Types)
                            -> T = [Tag|Types]
                          ; T = '%Undefined%' ).

%Export only complete output shapes as positive evidence. A partial input
%shape is an obligation, but an unknown output field cannot certify a caller.
normalize_inferred_output(T, TN) :-
    ( inferred_pattern_requirement(T), fully_known_inferred_type(T)
      -> normalize_inferred_param(T, TN)
    ; normalize_inferred(T, TN) ).

fully_known_inferred_type(T) :- nonvar(T), \+ wildcard_type(T),
    ( tagged_tuple_type(T, _, Fields) -> maplist(fully_known_inferred_type, Fields)
    ; is_list(T) -> maplist(fully_known_inferred_type, T)
    ; true ).

%Only clearly usable shapes are recorded; everything else is no-knowledge:
normalize_inferred(T, '%Undefined%') :- var(T), !.
normalize_inferred(T, T) :- atom(T), !.
normalize_inferred(T, ['List', ETN]) :- ground(T), list_type(T, ET), !,
                                        normalize_inferred(ET, ETN).
normalize_inferred(T, T) :- ground(T), is_arrow_type(T), !.
normalize_inferred(_, '%Undefined%').

%Keep unknown fields as wildcards in tagged shapes. Variable-headed or
%otherwise unreadable patterns still drop their shape; their deferred checks
%are retained at the consuming body calls instead.
normalize_inferred_param(T, TN) :-
    ( is_list(T), T = [Tag|Fs], atom(Tag), Fs \== [],
      maplist(normalize_inferred_param, Fs, FTs),
      Tagged = [Tag|FTs], tagged_tuple_type(Tagged, Tag, FTs)
      -> TN = Tagged
    ; normalize_inferred(T, TN) ).

%Successful checks that used a field assumption are provisional until the
%whole parameter shape is known. Resolve them once, after body translation:
%erase checks covered by a caller shape, keep existing residual checks for
%lost fields. This avoids a speculative second translation and its effects.
assumed_field_value(Value) :-
    nb_current('$assumptions', Pairs), Pairs \== [],
    nb_current('$compiling_clause', clause(_, Args, _)),
    term_variables(Value, Vars),
    member(a(V, _), Pairs), memberchk_eq(V, Vars),
    \+ ( member(Direct, Args), Direct == V ), !.

resolve_inferred_field_checks(none, _, Goals, Goals) :- !.
resolve_inferred_field_checks(_, _, Goals, Goals) :-
    \+ ( sub_term(Check, Goals), compound(Check),
         functor(Check, '$inferred_field_check', 3) ), !.
resolve_inferred_field_checks(assume(Pairs), Args, Goals0, Goals) :-
    ( nb_current('$assump_taint', Taints) -> true ; Taints = [] ),
    inferred_clause_arg_types(Pairs, Taints, Args, Types),
    exclude(assumption_exported(Args, Types), Pairs, Lost),
    maplist(resolve_field_goal(Lost), Goals0, Goals1),
    exclude(==(true), Goals1, Goals).

assumption_exported(Args, Types, a(V, _)) :-
    nth0(I, Args, Arg), nth0(I, Types, Type),
    pattern_exports_var(Arg, Type, V), !.

pattern_exports_var(Arg, Type, V) :-
    nonvar(Type), \+ wildcard_type(Type),
    ( var(Arg) -> Arg == V
    ; is_list(Arg), tagged_tuple_type(Type, Tag, FieldTypes), Arg = [Tag|Fields]
      -> nth0(I, Fields, Field), nth0(I, FieldTypes, FieldType),
         pattern_exports_var(Field, FieldType, V)
    ; is_list(Arg), is_list(Type), same_length(Arg, Type)
      -> nth0(I, Arg, Field), nth0(I, Type, FieldType),
         pattern_exports_var(Field, FieldType, V)
    ).

resolve_field_goal(Lost, Goal0, Goal) :-
    ( var(Goal0) -> Goal = Goal0
    ; Goal0 = '$inferred_field_check'(Fun, Value, Type)
      -> ( field_check_without_assumptions(Value, Type, Lost)
           -> oracle_arg_check(Value, Type, Checks)
         ; type_guard(Fun, Value, Type, Checks) ),
         goals_list_to_conj(Checks, Goal)
    ; compound(Goal0)
      -> compound_name_arguments(Goal0, F, Args0),
         maplist(resolve_field_goal(Lost), Args0, Args),
         ( F == ',', Args = [true, Right] -> Goal = Right
         ; F == ',', Args = [Left, true] -> Goal = Left
         ; compound_name_arguments(Goal, F, Args) )
    ; Goal = Goal0 ).

%Recheck a copy with only lost assumptions removed. Merely mentioning an
%unused wildcard field is not a dependency; aggregate and union checks can
%also remain proven without every variable in the value carrying a type.
field_check_without_assumptions(_, _, []) :- !.
field_check_without_assumptions(Value, Type, Lost) :-
    term_variables(Value, Vars),
    ( member(a(V, _), Lost), memberchk_eq(V, Vars)
      -> copy_term(Value-Type-Lost, CopyValue-CopyType-CopyLost),
         maplist(clear_field_assumption, CopyLost),
         arg_statically_ok(CopyValue, CopyType)
    ; arg_statically_ok(Value, Type) ).

clear_field_assumption(a(V, _)) :- ( var(V) -> del_attr(V, tknown) ; true ).

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
