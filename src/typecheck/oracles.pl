%%% Runtime soundness oracles. They only add runtime verification goals and
%%% never influence a compile-time acceptance decision.
%
%%% --oracle-det: a call to a committed function compiles to oracle_det_call/4,
%%% which counts the call's solutions (the clause-entry ! would hide extra
%%% ones) and then re-establishes the single solution's bindings. findall/3
%%% runs the callee once, so side effects are not duplicated; wrapped calls lose
%%% last-call optimization.
oracle_det_wrap(Fun, Args, Out, Goal, Wrapped) :-
    ( oracle_det_mode(true), atom(Fun),
      length(Args, NArgs),
      oracle_det_believed(Fun, NArgs, Args, Det), committed_det(Det)
      -> Wrapped = oracle_det_call(Fun, Det, Out, Goal)
       ; Wrapped = Goal ).

%Only a declared determinism is a promise to audit; inferred determinism is
%nobody's promise. Where the builtin table exists it overrides the
%declaration, as in function_call_determinism/3, or the oracle would be
%stricter than the checker it audits ((: empty (-> $a)) is semidet).
oracle_det_believed(F, N, Args, Det) :-
    ( effect_poly_call_determinism(F, N, Args, PolyDet)
      -> Det0 = PolyDet
    ; catch(fn_determinism(F, N, Det0), _, fail),
      Det0 \== unspecified ),
    table_det_override(F, N, Det0, Det).

%A call whose result is already bound tests a candidate answer
%((let True (> (myplus $x 2) 3) $x)), so zero solutions is a violation only
%when the result was left open. Two or more always are.
oracle_det_call(F, Det, Out, Goal) :-
    ( var(Out) -> Answering = true ; Answering = false ),
    findall(Goal, Goal, Sols),
    length(Sols, N),
    ( N >= 2 -> throw(error(determinism_cardinality(F, Det, N), determinism))
    ; Sols == [] -> ( Det == det, Answering == true
                      -> throw(error(determinism_cardinality(F, Det, 0), determinism))
                       ; fail )                 %semidet may fail; a filter may too
    ; Sols = [S], Goal = S ).

%oracle_check/2 adjudicates with the checker's own check_value/3, so it audits
%the certifications, not the type model: a too-permissive value relation
%agrees with itself. check_value/3 binds open types and can bind an unbound
%value, so both sides are copied, the call is semidet, and an unbound value is
%no evidence.
oracle_check(V, T) :- ( var(V) -> true
                      ; copy_term(V-T, V2-T2),
                        ( once(check_value(V2, T2, St)), St == mismatch
                          -> throw(error(literal_type_mismatch(V, T), typecheck))
                           ; true ) ).

%Under --oracle a statically discharged output certification is re-verified at
%runtime with check_value/3, which is stronger than the reflective guard.
oracle_output_check(DeclOut, Out, Gs0, Gs) :-
    ( oracle_mode(true), DeclOut = out(OT, _), nonvar(OT), \+ wildcard_type(OT), Gs0 == []
      -> Gs = [oracle_check(Out, OT)]
       ; Gs = Gs0 ).

%The audit for a statically discharged call argument. A rational term cannot be
%stored in an asserted clause, so such a value is not instrumented.
oracle_arg_check(AV, T, Gs) :-
    ( oracle_mode(true), nonvar(T), \+ wildcard_type(T),
      acyclic_term(T), acyclic_term(AV)
      -> Gs = [oracle_check(AV, T)]
       ; Gs = [] ).
