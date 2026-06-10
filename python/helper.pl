maybe_enable_silent(true) :- !.
maybe_enable_silent(false) :- assertz(silent(true)).

set_working_dir(load_metta_file, File) :-
    clear_imported_files,
    file_directory_name(File, Dir),
    retractall(working_dir(_)),
    assertz(working_dir(Dir)),
    !.
set_working_dir(_, _).

run_metta_helper(Verbose, Predicate, Arg, ResultsR) :-
    maybe_enable_silent(Verbose),
    set_working_dir(Predicate, Arg),
    call(Predicate, Arg, Results),
    maplist(swrite,Results,ResultsR).
