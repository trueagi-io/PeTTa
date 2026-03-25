:- ensure_loaded(metta).

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
                             ( memberchk(silent, RawArgs) ; memberchk("--silent", RawArgs) ; memberchk("-s", RawArgs)
                             -> true
                             ; maplist(swrite,Results,ResultsR),
                               maplist(format("~w~n"), ResultsR) )
        ),
        halt.

:- initialization(main, main).
