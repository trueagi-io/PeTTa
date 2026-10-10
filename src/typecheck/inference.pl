%%% Undeclared-function inference and parametric declaration validation.
%
%%% The promised type variables of the clause being compiled: a declaration's
%%% argument type variables are the caller's choice, so while the body is
%%% compiled (1) nothing it reads may be discharged against one
%%% (indefinite_candidate/1), and (2) the compiler may not pin one by guessing,
%%% as translate_closure_call/5 does for an unknown head. Scoped by b_setval so
%%% an abandoned nested compile cannot leak its set.
param_promises_scope(Promises, Outer) :- catch(b_getval('$param_promises', Outer), _, Outer = []),
                                         b_setval('$param_promises', Promises).

param_promises_restore(Outer) :- b_setval('$param_promises', Outer).

param_promise_var(V) :- var(V),
                        catch(b_getval('$param_promises', Vs), _, fail),
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
                                                     ; value_single_type(Arg, T)
                                                       -> ctor_pattern_field_types(Arg), Ps1 = Ps
                                                     %a destructured variable is a parameter too
                                                     %(its pattern type is rebuilt in infer_param_type/4):
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
                                               maplist(normalize_inferred_param, ATs0, ATs1),
                                               maplist(pattern_type_roundtrip, Args, ATs1, ATs),
                                               normalize_inferred(OT0, OT),
                                               ( member(T, [OT|ATs]), T \== '%Undefined%'
                                                 -> merge_inferred(F, ATs, OT) ; true ).

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
                          ; T = '%Undefined%' ).

%Only clearly usable shapes are recorded; everything else is no-knowledge:
normalize_inferred(T, '%Undefined%') :- var(T), !.
normalize_inferred(T, T) :- atom(T), !.
normalize_inferred(T, ['List', ETN]) :- ground(T), list_type(T, ET), !,
                                        normalize_inferred(ET, ETN).
normalize_inferred(T, T) :- ground(T), is_arrow_type(T), !.
normalize_inferred(_, '%Undefined%').

%A destructured parameter's shape survives only if it reads back as the tagged
%shape it was built from (tagged_tuple_type/3), and collapses when any field
%is undefined: a partly known tuple is not checkable. Parameter shapes are
%verified against their patterns afterwards (pattern_type_roundtrip/3).
normalize_inferred_param(T, TN) :- ( is_list(T), T = [Tag|Fs], atom(Tag), Fs \== [],
                                     maplist(normalize_inferred_param, Fs, FTs),
                                     \+ memberchk('%Undefined%', FTs),
                                     TN0 = [Tag|FTs], tagged_tuple_type(TN0, Tag, FTs)
                                     -> TN = TN0
                                      ; normalize_inferred(T, TN) ).

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
