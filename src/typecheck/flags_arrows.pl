%%% Checker modes, oracle switches, and canonical arrow syntax.
%
% Types travel on attributed variables on the Prolog variables representing
% MeTTa variables:
%   tknown - translation-time inferred/declared type candidates of a variable
%   mreq   - runtime type constraints placed on still-unbound values by guards
% Static errors are thrown during translation.
%
% Soundness oracles (see examples/soundness_matrix.sh) only add runtime
% checking; none of them changes which programs compile:
%   --oracle      re-emits every statically discharged certification as a
%                 runtime check: clause outputs (oracle_output_check/4) and
%                 call-site arguments (oracle_arg_check/3).
%   --oracle-det  counts the solutions of every committed (-[det]-> or
%                 -[semidet]->) call and throws on a violated commitment. The
%                 commit cut sits at clause entry, so --no-det-cut only exposes
%                 clause-selection alternatives; body holes need this oracle.
%   --no-det-cut  suppresses the determinism commit itself.
% --warn-runtime-checks reports every runtime type check the compiler emits,
% implicit residual guards and explicit (the ...) ascriptions alike.

:- dynamic strict_mode/1, strict_det/1, oracle_mode/1, oracle_det_mode/1,
           suppress_det_cut/1, warn_runtime_checks/1.

:- current_prolog_flag(argv, Argv),
   ( memberchk('--strict-det', Argv) -> assertz(strict_mode(true)), assertz(strict_det(true))
   ; memberchk('--strict', Argv) -> assertz(strict_mode(true)), assertz(strict_det(false))
                                  ; assertz(strict_mode(false)), assertz(strict_det(false)) ),
   forall(member(Flag-Store, ['--oracle'-oracle_mode, '--oracle-det'-oracle_det_mode,
                              '--no-det-cut'-suppress_det_cut,
                              '--warn-runtime-checks'-warn_runtime_checks]),
          ( ( memberchk(Flag, Argv) -> V = true ; V = false ),
            Fact =.. [Store, V],
            assertz(Fact) )).

warn_residual_check(Ctx, T) :- ( warn_runtime_checks(true)
                                 -> format(user_error, "Warning: runtime type check in ~w against ~p~n", [Ctx, T])
                                  ; true ).

%%% Arrows are prefix, like every MeTTa form: (-> A B), (-[det]-> A B),
%%% (-[semidet]-> A B), (-[nondet]-> A B), and the effect-polymorphic
%%% (-[$v]-> A B). A plain -> carries no determinism commitment; --strict-det
%%% rejects it in declarations, including nested higher-order positions.
%%% Cardinality is a total order det < semidet < nondet. semidet commits
%%% exactly like det - it only adds the right to fail - so it keeps the
%%% clause-entry cut and last-call optimization.

%Accepted spellings and the canonical atom each one normalizes to:
arrow_spelling('->', '->').
arrow_spelling('-[det]->', '-[det]->').
arrow_spelling('-[deterministic]->', '-[det]->').
arrow_spelling('-[semidet]->', '-[semidet]->').
arrow_spelling('-[semideterministic]->', '-[semidet]->').
arrow_spelling('-[nondet]->', '-[nondet]->').
arrow_spelling('-[nondeterministic]->', '-[nondet]->').

%Canonical arrow atoms and their commitment. `plain` is deliberately not a
%determinism level, so asking for an explicit commitment never matches -> :
arrow_atom_det('->', plain).
arrow_atom_det('-[det]->', det).
arrow_atom_det('-[semidet]->', semidet).
arrow_atom_det('-[nondet]->', nondet).
arrow_atom_det(A, effect(Name)) :- effect_arrow_atom(A, Name).

%The determinism any spelling declares; a plain -> is `unspecified`:
arrow_det(A, Det) :- arrow_spelling(A, C), arrow_atom_det(C, L),
                     ( L == plain -> Det = unspecified ; Det = L ).
arrow_det(A, effect(Name)) :- effect_arrow_atom(A, Name).

%The reader keeps -[$v]-> as one atom. Parse (and, when A is open, rebuild)
%the textual slot without turning it into a Prolog or MeTTa logic variable:
effect_arrow_atom(A, Name) :-
    ( atom(A)
      -> atom_concat('-[', Rest, A),
         atom_concat(Slot, ']->', Rest),
         atom_chars(Slot, ['$'|NameChars]),
         NameChars \== [],
         atom_chars(Name, NameChars)
    ; nonvar(Name), atom(Name),
      atom_concat('$', Name, Slot),
      atom_concat('-[', Slot, Prefix),
      atom_concat(Prefix, ']->', A) ).

arrow_atom(A) :- nonvar(A), arrow_atom_det(A, _).

%The determinism level of an arrow TYPE's head, only ever read, never bound:
arrow_head_level(K, L) :- nonvar(K), K = [A|_], nonvar(A), arrow_atom_det(A, L).

%A commitment that makes the compiler emit the clause-entry cut and validate
%the clause set: det and semidet both promise at most one result:
committed_det(det).
committed_det(semidet).

fn_type_shape(Type, ArgTypes, OutType, Det) :- is_list(Type), Type = [Arrow|Xs],
                                               nonvar(Arrow), atom(Arrow), arrow_det(Arrow, Det), !,
                                               append(ArgTypes, [OutType], Xs).

%An arrow atom anywhere but the head of its expression is the abandoned infix
%syntax; rejected loudly because it would otherwise silently parse as a
%value/tuple type and drop the arrow:
infix_arrow_misuse(T) :- is_list(T), T = [_|Rest],
                         member(X, Rest), nonvar(X),
                         ( atom(X) -> arrow_det(X, _) ; infix_arrow_misuse(X) ), !.
infix_arrow_misuse(T) :- is_list(T), T = [H|_], nonvar(H), infix_arrow_misuse(H).

%Normalize nested arrow types to canonical prefix form and expand aliases.
%Nondeterministic arrows keep their marker so closure parameters carry the
%commitment. Mode `syntax` canonicalizes arrows only, reconstructing the exact
%type cached before a source-local alias was visible.
normalize_type(T, TN) :- normalize_type(aliases, T, TN).

normalize_type(_, T, T) :- var(T), !.
normalize_type(aliases, T, R) :- atom(T), !,
                                 analysis_emit(dependency(declaration(alias, T))),
                                 ( declared_type_alias(T, Alias) -> R = Alias ; R = T ).
normalize_type(_, T, T) :- atomic(T), !.
normalize_type(Mode, T, TN) :- is_list(T), fn_type_shape(T, ATs, OT, _), !,
                               T = [Arrow|_],
                               canonical_arrow(Arrow, H),
                               maplist(normalize_type(Mode), ATs, ATN),
                               normalize_type(Mode, OT, OTN),
                               append(ATN, [OTN], Xs),
                               TN = [H|Xs].
normalize_type(Mode, T, TN) :- is_list(T), !, maplist(normalize_type(Mode), T, TN).
normalize_type(_, T, T).

canonical_arrow(A, C) :- arrow_spelling(A, C0), !, C = C0.
canonical_arrow(A, A) :- effect_arrow_atom(A, _), !.
canonical_arrow(_, (->)).
