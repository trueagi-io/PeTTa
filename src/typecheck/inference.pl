%%% Undeclared-function inference and parametric declaration validation.
%
% Owns: clause-local inference assumptions, inferred function types, promised
% declaration variables, and parametric input/output honesty checks.
% Consumes: canonical declaration/type queries and translation-time type
% candidate attributes.
% Boundary: inferred_fn_type/3 is the sole persistent store; all other
% assumptions are scoped and restored around one clause translation.
%
%%% The promised type variables of the clause currently being compiled.
%
% The snapshot above is a promise the declaration made to every caller, and
% two rules follow from that, both of which need to know the set while the
% BODY is being compiled (translate_clause/3 publishes it here):
%
%   1. Nothing the body reads may be discharged against one - it stands for a
%      type the caller picked, not one this clause knows (indefinite_candidate/1).
%   2. Nothing the compiler GUESSES may pin one. translate_closure_call/5
%      assumes an unknown head is a function and binds its type to an arrow
%      shape; that is sound inference about an undeclared function's own
%      parameter, but on a promised variable it is the compiler inventing a
%      fact about a position the declaration quantifies universally over -
%      and it costs nothing to skip, since a non-function head still reduces
%      to data exactly as before.
%
% Set with b_setval/2 so a nested compile (specialization, eval) that is later
% abandoned by backtracking cannot leak its set into the outer clause.
param_promises_scope(Promises, Outer) :- catch(b_getval('$param_promises', Outer), _, Outer = []),
                                         b_setval('$param_promises', Promises).

param_promises_restore(Outer) :- b_setval('$param_promises', Outer).

param_promise_var(V) :- var(V),
                        catch(b_getval('$param_promises', Vs), _, fail),
                        memberchk_eq(V, Vs).

%After the body is translated, every snapshotted position must still be unbound
%(var-var aliasing to another polymorphic function) or a wildcard. If the body
%forced it to a concrete type the declaration is dishonest - mirror
%parametric_output_check and reject at compile time:
parametric_param_check(F, Vars) :- forall(member(T, Vars),
                                          ( var(T) -> true
                                          ; wildcard_type_t(T) -> true
                                          ; throw(error(non_parametric_param(F, T), typecheck)) )).

%Only a candidate carrying CONCRETE type evidence makes a bottom body a
%dishonest parametric declaration: the marker, a wrapped literal and a still
%open type variable are all "no concrete result stated here".
parametric_output_check(F, ExpOut) :-
    ( var(ExpOut)
      -> ( known_candidates(ExpOut, Cs), member(C, Cs),
           candidate_evidence(C, type(_))
           -> throw(error(non_parametric_output(F), typecheck))
            ; true )
       ; throw(error(non_parametric_output(F), typecheck)) ).

%A declared argument type variable promises universality. Snapshot every type
%variable still open after head-pattern binding, including nested variables;
%the post-body check above must reject any one the implementation pins.
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

%%% Local type inference for undeclared functions %%%
%
% While an undeclared function's clause is translated, its variable parameters
% carry fresh assumption type variables; typed call sites in the body bind them
% by unification. A parameter whose assumption sees conflicting uses is tainted
% (no knowledge is recorded for it). This includes the variables of a
% destructuring head pattern - they are parameters of the clause too - whose
% assumptions are then rebuilt into the pattern's own type, the only thing a
% call site can go on once the body has stopped guarding them.
%
% The harvested types live in an internal store, are never asserted into &self,
% and are used only to *add* knowledge: eliminating guards, typing call
% outputs, and satisfying strict mode. Call sites of inferred functions never
% throw at compile time, and demand an inferred type only where the value is
% visibly of another one (see check_call_arg/5).
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
                                                     %a variable bound by a DESTRUCTURING head pattern is
                                                     %every bit as much a parameter of the clause, so it
                                                     %gets the same fresh assumption (the pattern's own
                                                     %type is rebuilt from those in infer_param_type/4):
                                                     ; is_list(Arg) -> assume_pattern_vars(Arg, Ps, Ps1)
                                                     ; Ps1 = Ps ).

assume_pattern_vars([], Ps, Ps).
assume_pattern_vars([A|As], Ps0, Ps) :- ( var(A) -> ( known_singleton(A, _) -> Ps1 = Ps0
                                                    ; add_known_type(A, Tv), Ps1 = [a(A, Tv)|Ps0] )
                                        ; is_list(A) -> assume_pattern_vars(A, Ps0, Ps1)
                                        ; Ps1 = Ps0 ),
                                        assume_pattern_vars(As, Ps1, Ps).

%A pattern headed by a uniquely declared constructor types its fields from that
%declaration, whatever the scrutinee's type turns out to be: (: P (-> Number
%Number Pair)) makes the $a and $b of (= (f (P $a $b)) ...) Numbers. Pure added
%knowledge - the fields of a value carrying P's tag are what P declared them.
%A literal field that contradicts the declaration only means this clause head
%cannot match a well-typed value; inference stays out of that judgement.
%\+ fun(Tag) is member_ctor/3's own constructor test: a declared symbol WITH
%equations is a function, and a function-headed pattern is compiled as an
%inverted CALL of that function - its fields are solved, not destructured -
%so its declaration says nothing about what the pattern variables hold.
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
                                   length(Fs, N), findall(ATs, fn_decl_arity(Tag, N, ATs, _), [ATs1])
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

%A structural parameter type is only worth storing if it still ACCEPTS the very
%pattern it was read off. The two readings of a tuple type (see
%tagged_tuple_type/3) do not compose freely: (Statement $s $p) under a declared
%(: Statement Type) infers (Type Number Number), which reads back as a tagged
%shape demanding the literal atom Type in head position - a claim no value
%matching that pattern satisfies. Round-tripping the pattern rejects exactly
%those, whichever way the type was built.
pattern_type_roundtrip(Arg, T, TN) :- ( \+ is_list(Arg) -> TN = T
                                      ; T \== '%Undefined%', \+ \+ check_value(Arg, T, ok) -> TN = T
                                      ; TN = '%Undefined%' ).

%The type of a destructuring parameter is its pattern with each field replaced
%by the field's inferred type - (stv $s $c) with both fields used as numbers is
%(stv Number Number). Rebuilding it is not decoration: the fields carry
%assumption types now, so the body no longer guards them, and this is what
%keeps a call site checking that (stv "a" "b") is not one of those.
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

%A destructuring parameter's shape is knowledge too, so it survives instead of
%collapsing - but only if it will be READ BACK as the tagged shape it was built
%from. (Statement $s $p) under a declared (: Statement Type) rebuilds to
%(Statement T1 T2), which tagged_tuple_type/3 reads positionally, as a 3-field
%record whose first field is a Statement: a different, and false, claim about
%the value. An undefined field collapses the whole shape as well - a partly
%known tuple type is not a shape any call site can check. Parameters only:
%their shapes are rebuilt from patterns this file controls and are verified
%against those patterns afterwards (pattern_type_roundtrip/3), which is not
%true of an output type read off an arbitrary body expression.
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
