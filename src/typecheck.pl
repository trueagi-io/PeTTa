%%%%%%%%%% Compile-time typechecking support %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Each predicate is defined in exactly one unit and each store is declared in
% its owning unit, so load order is not semantic (examples/
% typecheck_boundary_matrix.sh loads the user-space units permuted).
%
% Relational checker modules (see typecheck/UNIFIED_CHECKER.md): the abstract
% domain, builtin call summaries, the relational IR, its analyzer, and the
% bridge that feeds their results to the translator. builtin_registry.pl holds
% the builtin table shared by the translator and both checkers.
%
% User-space units, consumed directly by the translator:
%   flags_arrows.pl      modes and canonical arrow syntax
%   decl_store.pl        declaration store and lifecycle
%   type_lang.pl         type language, compatibility, type attributes
%   value_checks.pl      static/deferred value checks and call-site guards
%   clause_checks.pl     clause patterns, contextual results, output checks
%   inference.pl         undeclared inference and parametric promises
%   oracles.pl           runtime soundness/cardinality oracles
%   analysis_proofs.pl   proof records and the proof memo
%   det_proofs.pl        determinism walker, certificates, builtin rules
%   det_analysis.pl      effect flow, coverage, whole-set validation
%   det_validate.pl      committed-arrow validation and bound provisos
%   dependency_graph.pl  compiled dependencies and mutation invalidation

:- use_module('typecheck/abstract_domain.pl').
:- use_module('typecheck/call_summaries.pl').
:- use_module('typecheck/relational_ir.pl').
:- use_module('typecheck/ir_analyzer.pl').
:- use_module('typecheck/unified_checker_bridge.pl').
:- use_module('typecheck/builtin_registry.pl').

:- ensure_loaded([
       'typecheck/analysis_proofs.pl',
       'typecheck/clause_checks.pl',
       'typecheck/decl_store.pl',
       'typecheck/dependency_graph.pl',
       'typecheck/det_analysis.pl',
       'typecheck/det_proofs.pl',
       'typecheck/det_validate.pl',
       'typecheck/flags_arrows.pl',
       'typecheck/inference.pl',
       'typecheck/oracles.pl',
       'typecheck/type_lang.pl',
       'typecheck/value_checks.pl'
   ]).
