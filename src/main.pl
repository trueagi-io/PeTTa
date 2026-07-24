:- ensure_loaded(metta).
:- multifile prolog:error_message//1.

% Typecheck error messages — the spec suite (test.sh, examples/fail_*.metta)
% asserts on these exact phrasings; keep them in sync with the error terms the
% translator throws.
prolog:error_message(literal_type_mismatch(Value, Required)) -->
    [ 'Type mismatch: got ~p but expected ~p'-[Value, Required] ].
prolog:error_message(type_conflict(existing(Existing), required(Required))) -->
    [ 'Type conflict: value is constrained as ~p but also required as ~p'-[Existing, Required] ].
prolog:error_message(determinism_conflict(Fun, Reason)) -->
    [ 'Determinism check failed for ~p: ~p'-[Fun, Reason] ].
prolog:error_message(conflicting_determinism_declarations(Fun)) -->
    [ 'Conflicting determinism declarations for ~p'-[Fun] ].
prolog:error_message(overlapping_deterministic_clauses(Fun, ArgsA, ArgsB)) -->
    [ 'Deterministic function ~p has overlapping clauses with heads ~p and ~p'-[Fun, ArgsA, ArgsB] ].
prolog:error_message(inferred_type_conflict(Fun, Types)) -->
    [ 'Inferred type conflict for ~p: incompatible candidates ~p'-[Fun, Types] ].
prolog:error_message(no_matching_overload(Fun)) -->
    [ 'No matching typed overload for ~p'-[Fun] ].
prolog:error_message(non_parametric_output(Fun)) -->
    [ 'Declared output type variable of ~p requires a parametric (bottom) implementation'-[Fun] ].
prolog:error_message(unknown_newtype(T)) -->
    [ 'brand requires a declared (Newtype ...) name, got ~p'-[T] ].
prolog:error_message(infix_arrow_syntax(Name, Type)) -->
    [ 'Arrows are prefix - write (-[det]-> A B), not (A -[det]-> B) - in the declaration of ~p: ~p'-[Name, Type] ].
prolog:error_message(strict_runtime_typecheck(Context, Goal)) -->
    [ 'Strict mode rejected residual runtime type goal in ~p: ~p'-[Context, Goal] ].
prolog:error_message(strict_missing_function_type(Fun, Arity)) -->
    [ 'Strict mode requires a declared or inferable type for ~p/~p'-[Fun, Arity] ].

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
        ; Args = [File|_] -> file_directory_name(File, Dir),
                             assertz(working_dir(Dir)),
                             load_metta_file(File,Results),
                             maplist(swrite,Results,ResultsR),
                             maplist(format("~w~n"), ResultsR)
        ),
        halt.

:- initialization(main, main).
