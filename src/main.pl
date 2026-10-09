% Default all stream I/O to UTF-8.
%
% MeTTa source files are UTF-8 by convention, but SWI-Prolog derives its
% default `encoding` flag from the ambient locale. Where that locale is not
% UTF-8 - Windows (cp1252), or any POSIX/C locale, which minimal container
% images and CI runners commonly have - read_file_to_string/3 in
% filereader.pl decodes .metta sources in the locale codepage instead, and
% multi-byte characters in string literals or symbol names silently become
% several wrong characters.
%
% Setting the flag here, before anything opens a stream, makes source loading
% deterministic regardless of environment. It is a no-op where the locale is
% already UTF-8.
:- set_prolog_flag(encoding, utf8).

:- ensure_loaded(metta).
:- multifile prolog:error_message//1.

% Typecheck error messages — the spec suite (test.sh, examples/fail_*.metta)
% asserts on these exact phrasings; keep them in sync with the error terms the
% translator throws.
prolog:error_message(literal_type_mismatch(Value, Required)) -->
    [ 'Type mismatch: got ~p but expected ~p'-[Value, Required] ].
prolog:error_message(car_atom_empty) -->
    [ 'car-atom expects a non-empty expression' ].
prolog:error_message(type_conflict(existing(Existing), required(Required))) -->
    [ 'Type conflict: value is constrained as ~p but also required as ~p'-[Existing, Required] ].
prolog:error_message(determinism_conflict(Fun, Reason)) -->
    [ 'Determinism check failed for ~p: ~p'-[Fun, Reason] ].
prolog:error_message(det_functional_head_commitment(Fun, HeadArgs)) -->
    [ 'Determinism check failed for ~p: functional head ~p generates fallible head goals; explicit det/semidet validation cannot certify it'-[Fun, [Fun|HeadArgs]] ].
prolog:error_message(conflicting_determinism_declarations(Fun)) -->
    [ 'Conflicting determinism declarations for ~p'-[Fun] ].
prolog:error_message(conflicting_determinism_declarations(Fun, Locations)) -->
    [ 'Conflicting determinism declarations for ~p at ~p'-[Fun, Locations] ].
prolog:error_message(det_nonexhaustive(Fun, Pos, Missing)) -->
    { missing_case_text(Missing, Txt) },
    [ 'Deterministic function ~p is not exhaustive: argument ~w matches no clause for ~w - cover the remaining cases, or declare it -[semidet]-> (zero or one result, committed exactly like -[det]->)'-[Fun, Pos, Txt] ].
prolog:error_message(overlapping_deterministic_clauses(Fun, ArgsA, ArgsB)) -->
    [ 'Deterministic function ~p has overlapping clauses with heads ~p and ~p'-[Fun, ArgsA, ArgsB] ].
prolog:error_message(inferred_type_conflict(Fun, Types)) -->
    [ 'Inferred type conflict for ~p: incompatible candidates ~p'-[Fun, Types] ].
prolog:error_message(no_matching_overload(Fun)) -->
    [ 'No matching typed overload for ~p'-[Fun] ].
prolog:error_message(non_parametric_output(Fun)) -->
    [ 'Declared output type variable of ~p requires a parametric (bottom) implementation'-[Fun] ].
prolog:error_message(non_parametric_param(Fun, T)) -->
    [ 'Declared parametric parameter of ~p is used as ~p by its implementation; declare the concrete type instead'-[Fun, T] ].
prolog:error_message(unknown_newtype(T)) -->
    [ 'brand requires a declared (Newtype ...) name, got ~p'-[T] ].
prolog:error_message(infix_arrow_syntax(Name, Type)) -->
    [ 'Arrows are prefix - write (-[det]-> A B), not (A -[det]-> B) - in the declaration of ~p: ~p'-[Name, Type] ].
prolog:error_message(strict_det_plain_arrow(Name)) -->
    [ '--strict-det requires -[det]->, -[semidet]->, -[nondet]->, or -[$v]-> in every arrow position of the declaration for ~p; plain -> is not allowed'-[Name] ].
prolog:error_message(effect_variable_multiple(Name, Vars)) -->
    [ 'Effect-polymorphic declaration for ~p uses more than one effect variable ~p; v1 permits one'-[Name, Vars] ].
prolog:error_message(effect_variable_bad_position(Name, Var)) -->
    [ 'Effect variable $~w in the declaration for ~p may appear only on the top-level arrow or a direct closure parameter arrow'-[Var, Name] ].
prolog:error_message(effect_variable_uninstantiable(Name, Var)) -->
    [ 'Effect variable $~w on the top-level arrow of ~p is never instantiated by a closure argument'-[Var, Name] ].
prolog:error_message(strict_runtime_typecheck(Context, Goal)) -->
    [ 'Strict mode rejected residual runtime type goal in ~p: ~p'-[Context, Goal] ].
prolog:error_message(strict_nominal_intersection(Context, Types)) -->
    [ 'Strict mode cannot express the unresolved nominal intersection ~p in ~p; use an explicit (the ...) cast or redesign the declaration'-[Types, Context] ].
prolog:error_message(space_schema_row_mismatch(Space, Row, Schema)) -->
    [ 'SpaceOf declaration for ~p rejects existing row ~p: it definitely contradicts schema ~p'-[Space, Row, Schema] ].
prolog:error_message(determinism_cardinality(Fun, Det, N)) -->
    [ 'Determinism cardinality violated: ~p is declared -[~w]-> but this call produced ~w solutions'-[Fun, Det, N] ].
prolog:error_message(strict_missing_function_type(Fun, Arity)) -->
    [ 'Strict mode requires a declared or inferable type for ~p/~p'-[Fun, Arity] ].
prolog:error_message(unbound_det_argument(Fun, Det)) -->
    [ 'Argument of ~p is unbound: a -[~w]-> function requires bound arguments'-[Fun, Det] ].
prolog:error_message(det_argument_not_proper_list(Fun, Det)) -->
    [ 'Argument of ~p is not a proper list: its -[~w]-> proof requires a bound proper list'-[Fun, Det] ].

is_silent_flag(silent).
is_silent_flag('--silent').
is_silent_flag('-s').

strip_silent_flags([], []).
strip_silent_flags([Arg|Rest], Filtered) :-
        is_silent_flag(Arg),
        !,
        strip_silent_flags(Rest, Filtered).
strip_silent_flags([Arg|Rest], [Arg|Filtered]) :-
        strip_silent_flags(Rest, Filtered).

prologfunc(X,Y) :- Y is X+1.

prolog_interop_example :- register_fun(prologfunc),
                          process_metta_string("(= (mettafunc $x) (prologfunc $x))", _),
                          listing(mettafunc),
                          mettafunc(30, R),
                          format("mettafunc(30) = ~w~n", [R]).

main :- current_prolog_flag(argv, RawArgs),
        strip_silent_flags(RawArgs, Args),
        ( Args = [] -> prolog_interop_example
        ; Args = [mork] -> prolog_interop_example,
                           mork_test
        ; metta_files(Args, Files), Files = [First|_]
          -> file_directory_name(First, Dir),
             assertz(working_dir(Dir)),
             maplist(load_metta_file, Files, ResultsList),
             append(ResultsList, Results),
             maplist(swrite,Results,ResultsR),
             maplist(format("~w~n"), ResultsR)
        ),
        halt.

% Several .metta files may be named, and they are loaded in exactly the order
% given. Load ORDER is semantically significant - a declaration or a
% constructor that arrives in a later file cannot retroactively inform code
% already compiled from an earlier one - so some soundness properties can only
% be exercised by a multi-file program (see examples/soundness/ and Phase D of
% examples/soundness_matrix.sh). Leading arguments ending in .metta are files;
% everything from the first non-file argument on is flags.
% Without a leading .metta argument the first argument is the one file to load,
% whatever its extension, as before several files could be named.
metta_files(Args, Files) :- leading_metta_files(Args, Files), Files = [_|_], !.
metta_files([File|_], [File]).

leading_metta_files([A|As], [A|Fs]) :- atom(A), file_name_extension(_, metta, A), !,
                                       leading_metta_files(As, Fs).
leading_metta_files(_, []).

:- initialization(main, main).
