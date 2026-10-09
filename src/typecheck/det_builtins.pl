%%% Determinism of the builtins (registered funs backed by a Prolog predicate
%%% rather than MeTTa equations).
%
% Owns the effective function-call effect lookup and the compatibility shim
% over fixed registry cardinalities. Consumes builtin_registry views,
% canonical declarations, and inferred body proofs. It owns no mutable state.
%
% The builtin registry is the ONLY fixed determinism knowledge the checker has
% about them: a builtin without cardinality metadata is `unspecified`, which is
% what "never analysed"
% honestly means. It used to be the opposite - any registered symbol whose
% Prolog predicate merely existed was assumed det - and that assumed the
% strongest possible claim about the one part of the system the analysis
% cannot see. (get-atoms) is the counterexample that motivated the flip: it
% enumerates a space's clauses, so a -[det]-> function bound to it produced
% one result per atom in the space.
%
% THE CALLING CONVENTION THAT ACTUALLY HOLDS.
%
% Arity N is the MeTTa argument count; the Prolog predicate has N+1 arguments,
% the last being the result. Beyond that, assume NOTHING:
%
%   * An argument position may be UNBOUND, and may hold a value of the wrong
%     type. No check the compiler emits rules either out. The residual guard is
%     typecheck_or_error/2, whose variable branch is constrain_var_type/2, and
%     that SUCCEEDS on an unbound variable - it records a requirement for later,
%     it does not test one. So "typed Bool" never implies "bound to a boolean",
%     and "typed (List T)" never implies "a proper list". An unbound argument of
%     a declared type arises from ordinary well-typed code: the constructor
%     application (B $u) leaves its Bool field unfilled.
%   * The result position is usually a fresh variable, but not always - a call
%     whose result is already bound is testing a candidate answer rather than
%     asking for one (see oracle_det_call/4).
%   * An exception is not a solution. A predicate that raises on a mode it
%     cannot serve is still det; one that FAILS on such a mode is at best
%     semidet, and one that enumerates is nondet.
%
% So: det = exactly one solution for every instantiation, semidet = at most
% one, nondet = no claim beyond "some". Where none of the three can be
% established, the entry is simply absent, and an unlisted builtin is
% `unspecified` - which is what "never analysed" honestly means.
%
% The previous preamble assumed a ground, well-typed convention, and in
% particular that a (List T) position carries a PROPER list. It does not, and
% that assumption is what made eighteen entries false.
%
% The remedy applied throughout is to correct the ENTRY, not the predicate. In
% every one of these cases the extra solutions are real: nth0/3 enumerates
% actual elements, append/3 inverts to solve for a prefix, length/2 enumerates
% the shapes an open list can take, and bool/1 enumerates a finite type. The
% relational modes are used on purpose - lib_roman's mylast/init/rcons invert
% union-atom, examples/logicprogset.metta solves for a list from its length,
% and examples/booleansolver.metta enumerates boolean assignments - so a
% predicate answering more than once is behaving correctly and the table was
% simply wrong about it. A determinism table is a description; changing what a
% builtin DOES so that a stale description becomes true would be a language
% change smuggled in as a bug fix.
%
% The cost is that a -[det]-> body may no longer call them, which is exactly
% the point: it could not have kept its promise while doing so.
%
% Every entry below was re-derived from the predicate's source (src/metta.pl,
% src/spaces.pl, src/parser.pl, or the SWI library) and checked by counting
% solutions with every argument unbound. Do not trust these comments over the
% source; the previous set of comments is what got this wrong.

%The facts formerly below now live in builtin_registry.pl. Keep this public
%predicate as the compatibility view used throughout the checker: fixed
%levels and the conservative fallback of argument-sensitive entries are
%derived from their registry records; conditional/unspecified entries still
%have no flat verdict, exactly as before.
builtin_call_determinism(F, N, Det) :- builtin_flat_cardinality(F, N, Det).

%The builtin table is the checker's OWN bookkeeping about a symbol's result
%count, and where it has an entry that entry OVERRIDES any determinism derived
%from the program's own declaration. table_det_verdict/3 is that lookup, and
%the override it implements is shared by every site that needs a symbol's
%effective determinism, so the three cannot drift: a direct call
%(function_call_determinism/3), a value-position arrow head (value_arrow_head/4),
%and the oracle's wrapping decision (oracle_det_believed/3). The atom(F) guard
%keeps a still-unbound F from enumerating the table by binding itself to a
%builtin name; every caller here already passes a bound atom.
table_det_verdict(F, N, Det) :- atom(F), builtin_call_determinism(F, N, Det).

%The override applied to an ALREADY-COMPUTED fallback determinism (value_arrow_head
%and oracle_det_believed hold theirs in hand). function_call_determinism keeps
%the table check first and its fallback lazy, because its fallback is a goal that
%may legitimately have no answer for a table-only builtin.
table_det_override(F, N, Fallback, Det) :- ( table_det_verdict(F, N, DetB) -> Det = DetB ; Det = Fallback ).

function_call_determinism(F, N, Det) :- table_det_verdict(F, N, Det), !.
function_call_determinism(F, N, Det) :- catch(fn_determinism(F, N, Det0), _, fail),
                                        Det0 \== unspecified, !, Det = Det0.
function_call_determinism(F, N, Det) :-
    ( inferred_unknown_call_determinism(F, N, Inferred)
      -> Det = Inferred
    ; Det = unspecified ).
