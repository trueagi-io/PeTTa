:- ensure_loaded(metta).
:- use_module(library(optparse)).
:- use_module(library(option)).

options_spec(
    [ [opt(mode), meta(mode), type(atom), default('COMPILER'),
        shortflags([m]),   longflags(['mode']),
        help(['The operation mode of the PeTTa compiler:'
             ,'   COMPILER: Output only the compiled Prolog clauses, ignoring any runnables.'
             ,'   INTERPRETER: Interpret the provided MeTTa program and print the result of all runnables.'
             ,'   TEST_INTEROP:  Run the Prolog interop example'
             ,'   TEST_MORK: Run the Mork test'])]
      , [opt(silent_opt), type(atom), default(true),
        shortflags([s]), longflags(['silent']),
        help(['Whether to print each MeTTa form as it is parsed, followed by its Prolog translation.'])]
      , [opt(output_file), meta(file), type(atom), default('NADA'),
        shortflags([o]), longflags(['output']),
        help(['Where to store the compiled MeTTa code'])]
    ]).

prologfunc(X,Y) :- Y is X+1.

prolog_interop_example :- register_fun(prologfunc),
                          process_metta_string("(= (mettafunc $x) (prologfunc $x))", _),
                          listing(mettafunc),
                          mettafunc(30, R),
                          format("mettafunc(30) = ~w~n", [R]).

% Globals:
% * silent(true|false): Whether to print each MeTTa form as it is parsed, followed by its Prolog translation.
% * execute(true|false): Whether to execute any runnables parsed in the MeTTa program.
main :- current_prolog_flag(argv, Args),
        options_spec(Spec),
        opt_parse(Spec, Args, Opts, PositionalArgs),
        option(mode(Mode), Opts),
        option(silent_opt(Silent), Opts),
        assertz(silent(Silent)),
        % format("Options: ~w~n", Opts),
        % format("PositionalArgs: ~w~n", PositionalArgs),
        (Mode = 'TEST_INTEROP' ->
                prolog_interop_example
        ; (Mode = 'TEST_MORK') ->
                prolog_interop_example,
                mork_test
        ; ([] = PositionalArgs) ->
                format("Expected at least 1 positional argument with the MeTTa program to read.~n")
        ; (Mode = 'INTERPRETER', [File|_] = PositionalArgs) ->
                file_directory_name(File, Dir),
                assertz(working_dir(Dir)),
                assertz(execute(true)),
                load_metta_file(File,Results, _),
                maplist(swrite,Results,ResultsR),
                maplist(format("~w~n"), ResultsR)
        ; (Mode = 'COMPILER', [File|_] = PositionalArgs) ->
                option(output_file(OutputFile), Opts),
                file_directory_name(File, Dir),
                assertz(working_dir(Dir)),
                assertz(execute(false)),
                load_metta_file(File,_, Output),
                ( OutputFile = 'NADA' ->
                    write(current_output, Output)
                ; open(OutputFile, write, OutputFd),
                  write(OutputFd, Output)
                )),
        halt.

:- initialization(main, main).
