%%% Runtime soundness oracles.
%
% Owns: --oracle-det cardinality wrapping and --oracle rechecks of statically
% discharged argument/output certifications.
% Consumes: declaration/effect queries, check_value/3, and checker mode flags.
% Boundary: oracle predicates only add runtime verification goals; they never
% influence a compile-time acceptance decision.
%
%%% --oracle-det: the CARDINALITY oracle.
%
%A determinism declaration is a claim about how many solutions a call has:
%det = exactly one, semidet = zero or one. Nothing in the compiled program
%checks that claim, and the clause-entry ! actively hides violations by
%pruning the choicepoints that would reveal them. Under --oracle-det a call to
%a committed function is compiled to oracle_det_call/4, which enumerates the
%call's solutions, adjudicates the count, and then re-establishes the single
%solution's bindings. The callee is executed exactly ONCE (findall/3 drives it
%to exhaustion), so side effects are not duplicated - but last-call
%optimization is gone for wrapped calls, which is the price of counting.
oracle_det_wrap(Fun, Args, Out, Goal, Wrapped) :-
    ( oracle_det_mode(true), atom(Fun),
      length(Args, NArgs),
      oracle_det_believed(Fun, NArgs, Args, Det), committed_det(Det)
      -> Wrapped = oracle_det_call(Fun, Det, Out, Goal)
       ; Wrapped = Goal ).

%What is audited is a PROMISE, so only a declared determinism selects a call
%for wrapping - inferred determinism (body_determinism/3) is nobody's promise,
%and the builtin table is the checker's own bookkeeping rather than a claim the
%program made. But where that table exists it OVERRIDES the declaration, just
%as function_call_determinism/3 does: (: empty (-> $a)) reads as det under
%--strict-det while the checker knows empty is the canonical semidet bottom,
%and auditing that call against the declaration would make the oracle stricter
%than the thing it audits - a false positive by construction.
oracle_det_believed(F, N, Args, Det) :-
    ( effect_poly_call_determinism(F, N, Args, PolyDet)
      -> Det0 = PolyDet
    ; catch(fn_determinism(F, N, Det0), _, fail),
      Det0 \== unspecified ),
    table_det_override(F, N, Det0, Det).

%A call whose RESULT argument is already bound is not asking the function for
%its answer, it is testing a candidate one: (let True (> (myplus $x 2) 3) $x)
%compiles to a call to >/3 with the result fixed at true, and failing is how it
%says "no". "det" is a claim about the answering mode, so zero solutions is
%only a violation where the call site left the result open. Two or more
%solutions is a violation in either mode - no amount of input binding turns one
%answer into two.
oracle_det_call(F, Det, Out, Goal) :-
    ( var(Out) -> Answering = true ; Answering = false ),
    findall(Goal, Goal, Sols),
    length(Sols, N),
    ( N >= 2 -> throw(error(determinism_cardinality(F, Det, N), determinism))
    ; Sols == [] -> ( Det == det, Answering == true
                      -> throw(error(determinism_cardinality(F, Det, 0), determinism))
                       ; fail )                 %semidet may fail; a filter may too
    ; Sols = [S], Goal = S ).

%KNOWN LIMITATION: oracle_check/2 adjudicates with the checker's own
%check_value/3, so it audits the CERTIFICATIONS, not the type model. A value
%relation that is itself too permissive agrees with the certification it should
%contradict and stays invisible here - it can only ever re-ask the same
%question. That is why the (Newtype <wildcard>) hole had to be closed in
%type_unify/2 (see brand_unify/2) rather than instrumented: no oracle built on
%check_value/3 could have seen it. Auditing the model needs an INDEPENDENT
%value relation, which is out of scope.
%check_value/3 is a COMPILE-TIME relation over terms the compiler owns: it
%binds open types, and (via the partial(F,B) clause) it binds an unbound VALUE
%as well. Running it on live runtime data must not leak either binding back
%into the audited program - an oracle that mutates the program it audits is
%not an oracle. So both sides are copied, the call is made semidet, and an
%unbound value is treated as no evidence rather than as a certification.
oracle_check(V, T) :- ( var(V) -> true
                      ; copy_term(V-T, V2-T2),
                        ( once(check_value(V2, T2, St)), St == mismatch
                          -> throw(error(literal_type_mismatch(V, T), typecheck))
                           ; true ) ).

%Under --oracle a statically discharged output certification is re-verified
%at runtime with the checker's OWN value relation (check_value). The reflective
%guard is deliberately not used here: it is weaker than the checker.
oracle_output_check(DeclOut, Out, Gs0, Gs) :-
    ( oracle_mode(true), DeclOut = out(OT, _), nonvar(OT), \+ wildcard_type_t(OT), Gs0 == []
      -> Gs = [oracle_check(Out, OT)]
       ; Gs = Gs0 ).

%The corresponding audit for a statically discharged call argument.
%Rational terms cannot safely be stored in an asserted compiled clause, so an
%acyclicity failure means this optional oracle cannot instrument that value.
oracle_arg_check(AV, T, Gs) :-
    ( oracle_mode(true), nonvar(T), \+ wildcard_type_t(T),
      acyclic_term(T), acyclic_term(AV)
      -> Gs = [oracle_check(AV, T)]
       ; Gs = [] ).
