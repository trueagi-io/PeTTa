%%% Checker modes, oracle switches, and canonical arrow syntax.
%
% Owns: strict/oracle flag stores and initialization; arrow parsing,
% normalization, and commitment-level helpers.
% Consumes: normalize_type/2 recursively through the arrow helpers only.
% Boundary: loaded as a non-module unit by typecheck.pl; it owns no declaration
% stores and contributes no clauses to predicates owned by another unit.
%
% The subsystem has one canonical type store, one compatibility relation
% (type_unify/2), and one type channel: attributed variables on the Prolog vars
% representing MeTTa vars.
%   tknown - translation-time inferred/declared type candidates of a variable
%   mreq   - runtime type constraints placed on still-unbound values by guards
% Static errors are thrown during translation (never emitted and re-scanned).

:- dynamic strict_mode/1.

:- dynamic strict_det/1.

:- current_prolog_flag(argv, Argv),
   ( memberchk('--strict-det', Argv) -> assertz(strict_mode(true)), assertz(strict_det(true))
   ; memberchk('--strict', Argv) -> assertz(strict_mode(true)), assertz(strict_det(false))
                                  ; assertz(strict_mode(false)), assertz(strict_det(false)) ).

%Soundness oracles (see examples/soundness_matrix.sh). Three independent
%switches, each a pure ADDITION of runtime checking - none of them changes
%which programs compile, only what the compiled program verifies as it runs:
%
%  --oracle      re-emits every statically discharged certification as a
%                runtime check: clause OUTPUTS (oracle_output_check/4) and
%                call-site ARGUMENTS (oracle_arg_check/3) alike.
%  --oracle-det  counts the solutions of every committed (-[det]-> /
%                -[semidet]->) call, so a function that is declared
%                deterministic but semantically is not - zero results for det,
%                two or more for either - throws instead of quietly under- or
%                over-producing. This is what --no-det-cut cannot see:
%                clause_commit_cut/2 puts its ! at clause ENTRY, so removing it
%                only exposes CLAUSE-SELECTION alternatives, and overlapping
%                heads are already a hard static error. Every determinism hole
%                found so far lives in the BODY, where --no-det-cut is blind.
%  --no-det-cut  suppresses the determinism commit itself (kept: it is the only
%                switch that shows clause-selection alternatives directly).
:- dynamic oracle_mode/1.
:- dynamic oracle_det_mode/1.
:- dynamic suppress_det_cut/1.
:- current_prolog_flag(argv, Argv),
   ( memberchk('--oracle', Argv) -> assertz(oracle_mode(true)) ; assertz(oracle_mode(false)) ),
   ( memberchk('--oracle-det', Argv) -> assertz(oracle_det_mode(true)) ; assertz(oracle_det_mode(false)) ),
   ( memberchk('--no-det-cut', Argv) -> assertz(suppress_det_cut(true)) ; assertz(suppress_det_cut(false)) ).

%--warn-runtime-checks reports every runtime type check the compiler emits -
%implicit residual guards and explicit (the ...) ascriptions alike - so the
%user can see and eliminate them one by one. Independent of --strict (where
%implicit residuals are errors anyway, this reports the explicit remainder):
:- dynamic warn_runtime_checks/1.
:- current_prolog_flag(argv, Argv),
   ( memberchk('--warn-runtime-checks', Argv) -> assertz(warn_runtime_checks(true))
                                               ; assertz(warn_runtime_checks(false)) ).

warn_residual_check(Ctx, T) :- ( warn_runtime_checks(true)
                                 -> format(user_error, "Warning: runtime type check in ~w against ~p~n", [Ctx, T])
                                  ; true ).

%%% Arrow shapes: prefix, like every MeTTa form - (-> A B), (-[det]-> A B),
%%% (-[semidet]-> A B), (-[nondet]-> A B), and the bounded effect-polymorphic
%%% form (-[$v]-> A B). A plain -> carries no determinism commitment. Under
%%% --strict-det it is rejected in declarations, including nested higher-order
%%% positions; fixed explicit levels and effect variables remain.
%%% Cardinality is a total order: det (exactly one) < semidet (zero or one)
%%% < nondet (any). semidet commits exactly like det - it only adds the right
%%% to fail - so it keeps the clause-entry cut and last-call optimization.
arrow_det('->', unspecified).
arrow_det('-[det]->', det).
arrow_det('-[deterministic]->', det).
arrow_det('-[semidet]->', semidet).
arrow_det('-[semideterministic]->', semidet).
arrow_det('-[nondet]->', nondet).
arrow_det('-[nondeterministic]->', nondet).
arrow_det(A, effect(Name)) :- effect_arrow_atom(A, Name).

%%% The single enumeration of the CANONICAL arrow atoms (what
%%% canonical_arrow/2 produces) and the determinism each commits to. Every
%%% site that used to spell out ('->' ; '-[det]->' ; '-[nondet]->') goes
%%% through this - adding an arrow means adding a clause here, arrow_det/2 and
%%% canonical_arrow/2, and nowhere else. `plain` is the uncommitted ->; it is
%%% deliberately NOT a determinism level, so a site can ask for an explicit
%%% commitment without accidentally matching -> :
arrow_atom_det('->', plain).
arrow_atom_det('-[det]->', det).
arrow_atom_det('-[semidet]->', semidet).
arrow_atom_det('-[nondet]->', nondet).
arrow_atom_det(A, effect(Name)) :- effect_arrow_atom(A, Name).

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

%Normalize nested arrow types to canonical prefix form. Nondeterministic
%arrows keep their marker so closure parameters carry the commitment:
normalize_type(T, T) :- var(T), !.
normalize_type(T, R) :- atom(T), !,
                        analysis_emit(dependency(declaration(alias, T))),
                        ( declared_type_alias(T, Alias) -> R = Alias ; R = T ).
normalize_type(T, T) :- atomic(T), !.
normalize_type(T, TN) :- is_list(T), fn_type_shape(T, ATs, OT, _), !,
                         T = [Arrow|_],
                         canonical_arrow(Arrow, H),
                         maplist(normalize_type, ATs, ATN),
                         normalize_type(OT, OTN),
                         append(ATN, [OTN], Xs),
                         TN = [H|Xs].
normalize_type(T, TN) :- is_list(T), !, maplist(normalize_type, T, TN).
normalize_type(T, T).

%Explicit determinism markers survive normalization so closure parameters
%carry their commitment in every mode, not only under --strict-det:
canonical_arrow('-[det]->', '-[det]->') :- !.
canonical_arrow('-[deterministic]->', '-[det]->') :- !.
canonical_arrow('-[semidet]->', '-[semidet]->') :- !.
canonical_arrow('-[semideterministic]->', '-[semidet]->') :- !.
canonical_arrow('-[nondet]->', '-[nondet]->') :- !.
canonical_arrow('-[nondeterministic]->', '-[nondet]->') :- !.
canonical_arrow(A, A) :- effect_arrow_atom(A, _), !.
canonical_arrow(_, (->)).

%%%%%%%%%% Clause-analysis context: remaining scoped inputs %%%%%%%%%%
%
% The functional proof cores still receive a few load-bearing scoped inputs
% through backtrackable global variables.  They are INPUTS only: proof outputs
% (bound requirements, certificates and constructor dependencies) now return in
% analysis_proof/5, and genuine memos live only behind analysis_cache_*.
% These inputs use
% (b_setval/nb_setval), each with a setup/restore helper that saves the outer
% value and reinstates it on exit. They are NOT one context: their LIFETIMES
% differ, which is why they are kept separate (a single term would tie scopes
% that legitimately nest at different granularities). Inventory, so a reader
% need not reconstruct it from the writer/reader sites:
%
%   $det_head_scope   (det_validate.pl)  scope(HeadVars, DirectParams, Args).
%       LIFETIME per CLAUSE BODY. Writer with_det_head_vars/2; readers
%       det_head_var/1, det_direct_param/1 (-> unify_head_is_data/1,
%       enforced_bound_param/1, the det strengthenings). The two lists were once
%       two globals; they are captured from one head at one moment and merged,
%       and the whole Args list rides along so a returned requirement can be
%       located by its 1-based position.
%   $det_enforced     (det_validate.pl)  the commitment gate, enforced(F,N) or
%       false. LIFETIME per CLAUSE SET - strictly COARSER than $det_head_scope:
%       it is set once around clause_set_determinism/2, which then opens one
%       $det_head_scope per clause body inside it. That is why it is a separate
%       helper (with_det_enforced/2) and NOT folded into $det_head_scope.
%       Readers det_enforced_now/0, det_enforced_fn/2 (-> enforced_bound_param/1).
%   $det_stack        (det_proofs.pl)    list, the body_determinism/3 recursion
%       guard. LIFETIME per transitive-analysis call chain.
%   $det_assume_stack (det_analysis.pl)  list, the SEPARATE recursion guard for
%       body_determinism_assuming/3 - deliberately not $det_stack (see the note
%       there: sharing would cross-poison the two analyses). Same lifetime shape.
%   $assumptions / $assume_decl / $assump_taint  (inference.pl)
%       the assumption environment while an UNDECLARED function's clause is
%       type-inferred. LIFETIME per clause of an undeclared function. Writer
%       begin_clause_inference/4 (+ taint_assumption/1); restored from its
%       saved(...) token; readers taint_assumption/1, assumed_self_decl/4,
%       store_inferred_type/4.
%   $param_promises   (inference.pl)     the promised declaration type variables
%       of the clause under translation. LIFETIME per clause translation. Writer
%       param_promises_scope/2 (+ _restore/1); reader param_promise_var/1, which
%       candidate_evidence/2 consults to class an unbound candidate as promised.
%   $effect_assume_stack (det_analysis.pl) the distinct recursion guard for
%       effect-polymorphic intrinsic-body proofs.
%   $nonempty_vars    (det_proofs.pl) the branch-local flow fact established by
%       an if condition and consumed only while analyzing its else branch.
%
% The three recursion guards and the two clause-context scopes remain here in
% Phase 3 because deterministic_expr_core/2 is a mutually recursive walker with
% procedural argument-sensitive hooks. Threading them through every hook would
% be a high-risk signature rewrite. Unlike the removed accumulators they cannot
% leak a result: each is read-only inside its setup/restore extent.
