%%% Functional analysis records and the memo boundary.
%
% Analyses return one uniform, closed record:
%
%   analysis_proof(Subject, Verdict,
%                  requirements(BoundaryRequirements),
%                  certificates(OutputCertificates),
%                  dependencies(Dependencies))
%
% The core walkers communicate proof observations through a backtrackable,
% dynamically scoped event stack. A surrounding analysis_collect/2 turns the
% stack frame into ordinary returned data; no result survives that boundary.
% Nested proof-producing analyses contribute one returned proof record to their
% caller, so a top-level proof contains the transitive evidence it consumed.
%
% Owns analysis_proof/5 construction/projection, observation collection, and
% the sole analysis_memo/2 cache API. Consumes dependency-event matching and
% declaration/type dependency extraction. Boundary: the dynamic memo is
% private by convention; all reads, writes, and invalidation use this API.

:- dynamic analysis_memo/2.
:- dynamic analysis_memo_dep/2. % analysis_memo_dep(Dependency, MemoKey)

analysis_cache_lookup(Key, Proof) :-
    analysis_memo(Key, Stored),
    ( ground(Stored) -> Proof = Stored ; copy_term(Stored, Proof) ).

analysis_cache_store(Key, Proof) :-
    analysis_cache_remove_key(Key),
    analysis_compact_proof(Proof, Compact),
    ( ground(Compact) -> Stored = Compact ; copy_term(Compact, Stored) ),
    assertz(analysis_memo(Key, Stored)),
    analysis_memo_dependencies(Key, Stored, Dependencies),
    forall(member(Dependency, Dependencies),
           assertz(analysis_memo_dep(Dependency, Key))),
    record_constructor_dependency_owner(memo(Key), Dependencies).

analysis_compact_proof(
    analysis_proof(Subject, Verdict,
                   requirements(Bounds0),
                   certificates(Certs0),
                   dependencies(Deps0)),
    analysis_proof(Subject, Verdict,
                   requirements(Bounds),
                   certificates(Certs),
                   dependencies(Deps))) :-
    sort(Bounds0, Bounds),
    sort(Certs0, Certs),
    sort(Deps0, Deps).

analysis_cache_remove_key(Key) :-
    retractall(analysis_memo(Key, _)),
    retractall(analysis_memo_dep(_, Key)),
    forget_constructor_dependency_owner(memo(Key)).

analysis_cache_remove_keys(Keys) :-
    forall(member(Key, Keys), analysis_cache_remove_key(Key)).

analysis_cache_keys(Template, Keys) :-
    findall(Key,
            ( analysis_memo(Key, _),
              subsumes_term(Template, Key) ),
            Keys0),
    sort(Keys0, Keys).

% Legacy selectors remain for callers which are not mutation boundaries.  A
% program mutation goes through analysis_cache_invalidate_event/1 below: it
% removes only proofs whose recorded (or memo-key-implied) dependencies match.
analysis_cache_invalidate(all) :-
    findall(Key, analysis_memo(Key, _), Keys0),
    sort(Keys0, Keys),
    analysis_cache_remove_keys(Keys).
analysis_cache_invalidate(det) :-
    analysis_cache_keys(det(_, _), Keys),
    analysis_cache_remove_keys(Keys).
analysis_cache_invalidate(assume) :-
    analysis_cache_keys(assume(_, _), Keys),
    analysis_cache_remove_keys(Keys).
analysis_cache_invalidate(effect) :-
    analysis_cache_keys(effect(_, _, _), Keys),
    analysis_cache_remove_keys(Keys).
analysis_cache_invalidate(effect(F)) :-
    analysis_cache_keys(effect(F, _, _), Keys),
    analysis_cache_remove_keys(Keys).
analysis_cache_invalidate(output) :-
    analysis_cache_keys(output(_, _, _), Keys),
    analysis_cache_remove_keys(Keys).
analysis_cache_invalidate(output(Kind, F, N)) :-
    analysis_cache_remove_key(output(Kind, F, N)).

analysis_cache_invalidate_event(Event) :-
    findall(Key,
            ( mutation_candidate_dependency(Event, Dependency),
              analysis_memo_dep(Dependency, Key) ),
            Keys0),
    sort(Keys0, Keys),
    analysis_cache_remove_keys(Keys).

analysis_cache_invalidate_outputs(F) :-
    analysis_cache_keys(output(_, F, _), Keys),
    analysis_cache_remove_keys(Keys).

analysis_memo_dependencies(Key, Proof, Dependencies) :-
    analysis_proof_dependencies(Proof, Recorded),
    analysis_memo_key_dependencies(Key, Implied),
    append(Recorded, Implied, Ds0),
    sort(Ds0, Dependencies).

analysis_memo_key_dependencies(det(F, N), Ds) :- !,
    Ds = [effect(F/N), decl(F/N), clause_set(F/N)].
analysis_memo_key_dependencies(assume(F, N), Ds) :- !,
    Ds = [effect(F/N), decl(F/N), clause_set(F/N)].
analysis_memo_key_dependencies(effect(F, N, _), Ds) :- !,
    Ds = [effect(F/N), decl(F/N), clause_set(F/N)].
analysis_memo_key_dependencies(output(Kind, F, N), Ds) :- !,
    Ds = [output_cert(Kind, F/N), clause_set(F/N)].
analysis_memo_key_dependencies(_, []).

analysis_emit(Event) :-
    catch(b_getval('$analysis_event_stack', [Events|Outer]), _, fail), !,
    b_setval('$analysis_event_stack', [[Event|Events]|Outer]).
analysis_emit(_).

analysis_collect(Goal, Events) :-
    catch(b_getval('$analysis_event_stack', Stack0), _, Stack0 = []),
    setup_call_cleanup(
        b_setval('$analysis_event_stack', [[]|Stack0]),
        ( call(Goal),
          b_getval('$analysis_event_stack', [RevEvents|_]),
          reverse(RevEvents, Events) ),
        b_setval('$analysis_event_stack', Stack0)).

analysis_make_proof(Subject, Verdict, Events, ExtraDependencies, Proof) :-
    analysis_events_parts(Events, Bs0-[], Cs0-[], Ds0-[]),
    append(Ds0, ExtraDependencies, Ds1),
    sort(Bs0, Bounds),
    sort(Cs0, Certs),
    sort(Ds1, Deps),
    Proof = analysis_proof(Subject, Verdict,
                           requirements(Bounds),
                           certificates(Certs),
                           dependencies(Deps)).

%Nested analyses return complete proofs. Carry one proof observation through
%the collection boundary and merge its already-batched result lists here;
%replaying every dependency through reset/shift at every nesting level makes
%deep call analyses quadratic in observations. Boundary requirements remain
%local to their proof subject and are deliberately not inherited.
analysis_events_parts([], Bs-Bs, Cs-Cs, Ds-Ds).
analysis_events_parts([required_bound(Pos, Kind)|Events],
                      [bound(Pos, Kind)|Bs]-BT, Cs-CT, Ds-DT) :- !,
    analysis_events_parts(Events, Bs-BT, Cs-CT, Ds-DT).
analysis_events_parts([certificate(Kind, Key)|Events],
                      Bs-BT, [output(Kind, Key)|Cs]-CT, Ds-DT) :- !,
    analysis_events_parts(Events, Bs-BT, Cs-CT, Ds-DT).
analysis_events_parts([dependency(Dependency)|Events],
                      Bs-BT, Cs-CT, [Dependency|Ds]-DT) :- !,
    analysis_events_parts(Events, Bs-BT, Cs-CT, Ds-DT).
analysis_events_parts([proof(Proof)|Events],
                      Bs-BT, Cs0-CT, Ds0-DT) :- !,
    analysis_proof_certificates(Proof, Certs),
    analysis_proof_dependencies(Proof, Deps),
    analysis_list_dl(Certs, Cs0-Cs1),
    analysis_list_dl(Deps, Ds0-Ds1),
    analysis_events_parts(Events, Bs-BT, Cs1-CT, Ds1-DT).
analysis_events_parts([_|Events], Bs-BT, Cs-CT, Ds-DT) :-
    analysis_events_parts(Events, Bs-BT, Cs-CT, Ds-DT).

analysis_list_dl([], Tail-Tail).
analysis_list_dl([X|Xs], [X|Rest]-Tail) :-
    analysis_list_dl(Xs, Rest-Tail).

analysis_proof_verdict(analysis_proof(_, Verdict, _, _, _), Verdict).
analysis_proof_requirements(analysis_proof(_, _, requirements(Bounds), _, _), Bounds).
analysis_proof_certificates(analysis_proof(_, _, _, certificates(Certs), _), Certs).
analysis_proof_dependencies(analysis_proof(_, _, _, _, dependencies(Deps)), Deps).

analysis_reemit_proof(Proof) :-
    %Boundary requirements belong to Proof's subject. A caller may depend on
    %the callee proof, but must never reinterpret the callee's argument
    %positions as its own; analysis_events_parts/4 merges only certs and deps.
    analysis_emit(proof(Proof)).

% Conservative dependency inventory for an expression or clause-set term.
% Every real named call records its declaration/effect and both certificate
% families. A declared-but-as-yet undefined call or bare symbol instead gets a
% late_* dependency, so its first definition invalidates that decision without
% waking ordinary same-file callers already covered by the declaration/body
% prepasses.
% Type dependencies are attached only when the term actually carries them;
% the old "all types in the program" approximation would make the Phase-4
% dependency graph effectively global on every constructor declaration.
analysis_term_dependencies(Term, Dependencies) :-
    analysis_term_inventory(Term, Calls0-[], Symbols0-[]),
    sort(Calls0, Calls),
    findall(D,
            ( member(F/N, Calls),
              analysis_call_dependency(F, N, D) ),
            CallDeps),
    sort(Symbols0, Symbols),
    findall(D,
            ( member(S, Symbols),
              analysis_symbol_dependency(S, D) ),
            SymbolDeps),
    analysis_known_type_dependencies(Term, KnownTypeDeps),
    append([CallDeps, SymbolDeps, KnownTypeDeps], Ds0),
    sort(Ds0, Dependencies).

analysis_symbol_dependency(S, late_symbol(S)) :-
    \+ fun(S), fn_decl(S, _, _, _, _, _).
%A bare value-symbol lookup consumes the absence of a value declaration too:
%a later declaration can change its candidate type and must invalidate the
%compiled decision.
analysis_symbol_dependency(S, declaration(value, S)).

analysis_call_dependency(F, N, D) :-
    fun(F), !,
    member(D, [effect(F/N), decl(F/N),
               output_cert(proper_list, F/N),
               output_cert(bound_bool, F/N)]).
analysis_call_dependency(F, N, late_call(F/N)) :-
    fn_decl(F, N, _, _, _, _).

analysis_known_type_dependencies(Term, Dependencies) :-
    term_variables(Term, Vars),
    findall(D,
            ( member(V, Vars),
              get_attr(V, tknown, Types),
              member(T, Types),
              analysis_type_dependency(T, D) ),
            Ds0),
    sort(Ds0, Dependencies).

analysis_function_decl_dependencies(F, Dependencies) :-
    findall(Scheme-Provenance,
            fn_decl(F, _, Scheme, _, _, Provenance),
            Declarations),
    findall(D,
            ( member(scheme(ATs, OT)-_, Declarations),
              member(Term, [ATs, OT]),
              analysis_type_dependency(Term, D) ),
            NormalizedDeps),
    findall(D,
            ( member(_-provenance(_, syntax(Syntax)), Declarations),
              analysis_type_dependency(Syntax, D) ),
            SourceDeps),
    append(NormalizedDeps, SourceDeps, Ds0),
    sort(Ds0, Dependencies).

%Type normalization/checking consumes negative declarations as well as
%positive ones.  Recording all declaration kinds for an atom is a deliberate
%small over-approximation that prevents a late kind declaration from changing
%the interpretation of an already-compiled type spelling invisibly.
analysis_type_dependency(T, declaration(alias, T)) :- atom(T).
analysis_type_dependency(T, declaration(newtype, T)) :- atom(T).
analysis_type_dependency(T, declaration(foreign, T)) :- atom(T).
analysis_type_dependency(T, declaration(space, T)) :- atom(T).
analysis_type_dependency(T, ctor_set(T)) :-
    atom(T), \+ primitive_type(T), \+ wildcard_type(T),
    \+ declared_type_alias(T, _),
    \+ declared_newtype(T, _),
    \+ declared_foreign_type(T, _),
    \+ declared_space_type(T, _).
analysis_type_dependency(T, D) :-
    nonvar(T), is_list(T), member(E, T), analysis_type_dependency(E, D).

%One traversal collects the call and bare-symbol inventories consumed by the
%dependency proof. Difference lists avoid the append tree formerly built by
%two independent recursive walkers.
analysis_term_inventory(Term, Calls-CallsTail, Symbols-SymbolsTail) :-
    ( var(Term)
      -> Calls = CallsTail, Symbols = SymbolsTail
    ; atom(Term)
      -> Calls = CallsTail, Symbols = [Term|SymbolsTail]
    ; atomic(Term)
      -> Calls = CallsTail, Symbols = SymbolsTail
    ; Term = [F|Args], atom(F), is_list(Args)
      -> length(Args, N),
         Calls = [F/N|CallsRest],
         Symbols = [F|SymbolsRest],
         analysis_terms_inventory(Args,
                                  CallsRest-CallsTail,
                                  SymbolsRest-SymbolsTail)
    ; Term = [H|T]
      -> analysis_term_inventory(H, Calls-CallsMid, Symbols-SymbolsMid),
         analysis_term_inventory(T, CallsMid-CallsTail,
                                 SymbolsMid-SymbolsTail)
    ; compound(Term)
      -> Term =.. [_|Args],
         analysis_terms_inventory(Args, Calls-CallsTail,
                                  Symbols-SymbolsTail)
    ; Calls = CallsTail, Symbols = SymbolsTail ).

analysis_terms_inventory([], Calls-Calls, Symbols-Symbols).
analysis_terms_inventory([Term|Terms], Calls-CallsTail,
                         Symbols-SymbolsTail) :-
    analysis_term_inventory(Term, Calls-CallsMid, Symbols-SymbolsMid),
    analysis_terms_inventory(Terms, CallsMid-CallsTail,
                             SymbolsMid-SymbolsTail).

analysis_snapshot_proof(Subject, Snapshot, Dependencies,
                        analysis_proof(Subject, snapshot(Snapshot),
                                       requirements([]), certificates([]),
                                       dependencies(Deps))) :-
    sort(Dependencies, Deps).
