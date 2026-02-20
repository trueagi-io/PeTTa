%Translate a MeTTa S-expression file (no code, no bangs) to prolog predicates to load:
metta_file_to_prolog(Input, Space, Output) :- setup_call_cleanup( open(Input, read, In),
                                                                  setup_call_cleanup( open(Output, write, Out),
                                                                                      (
                                                                                          format(Out, ":- multifile '~w'/3.~n", [Space]),
                                                                                          format(Out, ":- discontiguous '~w'/3.~n~n", [Space]),
                                                                                          convert_stream(In, Out, Space)
                                                                                      ),
                                                                                      close(Out) ),
                                                                  close(In) ).

%Process stream line by line
convert_stream(In, Out, Space) :- read_line_to_string(In, Line),
                                  ( Line == end_of_file
                                    -> true
                                     ; convert_line(Line, Space, Out),
                                       convert_stream(In, Out, Space) ).

%Perform simple transformation from S-Expression to space-rel predicate:
convert_line(Line0, Space, Out) :- sub_string(Line0, 1, _, 1, Inner0),
                                   replace_all("(", "[", Inner0, Inner1),
                                   replace_all(")", "]", Inner1, Inner2),
                                   replace_all(" ", ",", Inner2, Inner3),
                                   format(Out, "'~w'(~w).~n", [Space, Inner3]).

%Helper predicate for string repkacement:
replace_all(P, R, S, O) :- split_string(S, P, "", Parts),
                           atomic_list_concat(Parts, R, O).

%The static import function that allows loading static data files fast:
'static-import!'(Space, File, true) :- style_check(-discontiguous),
                                       atom_string(File, SFile),
                                       working_dir(Base),
                                       atomic_list_concat([Base, '/', SFile, '.qlf'], QlfFile),
                                       atomic_list_concat([Base, '/', SFile, '.pl'], PlFile),
                                       atomic_list_concat([Base, '/', SFile, '.metta'], MettaFile),
                                       ( exists_file(QlfFile)
                                         -> % Case 1: .qlf exists → load fastest
                                            consult(QlfFile)
                                          ; exists_file(PlFile)
                                         -> % Case 2: .pl exists → compile to qlf and load
                                            qcompile(PlFile),
                                            consult(QlfFile)
                                          ; % Case 3: .pl does not exist → generate from .metta then compile
                                            metta_file_to_prolog(MettaFile, Space, PlFile),
                                            qcompile(PlFile),
                                            consult(QlfFile) ).


'use-module!'(Module, true) :- use_module(library(Module)).

%%% Git Import: %%%
'git-import!'(GitPath, true) :- 'git-import!'(GitPath, '', './repos', true).
     
'git-import!'(GitPath, BuildCmd, true) :- 'git-import!'(GitPath, BuildCmd, './repos', true).
     
'git-import!'(GitPath, BuildCmd, BaseDir, true) :- ( exists_directory(BaseDir) -> true
                                                                                 ; format("What ~w~n", [BaseDir]), make_directory_path(BaseDir) ),
                                                   repo_name_from_git(GitPath, Name),
                                                   directory_file_path(BaseDir, Name, LocalDir),
                                                   ( exists_directory(LocalDir) -> true
                                                                                 ; clone_repo(GitPath, LocalDir),
                                                                                   run_build_step(LocalDir, BuildCmd) ),
                                                   asserta(library_path(LocalDir)).

%Extract "repo" from ".../repo.git" or "...:repo.git":
repo_name_from_git(GitPath, Name) :- atom_string(GitPath, S),
                                     split_string(S, "/:", "/:", Parts),
                                     last(Parts, Last0),
                                     ( sub_string(Last0, _, 4, 0, ".git")
                                       -> sub_string(Last0, 0, _, 4, Last)
                                        ; Last = Last0 ),
                                     atom_string(Name, Last).

clone_repo(GitPath, LocalDir) :- format("Cloning ~w into ~w~n", [GitPath, LocalDir]),
                                 process_create(path(git),
                                                ['clone', '--depth', '1', GitPath, LocalDir],
                                                [stdout(pipe(Out)), stderr(pipe(Err))]),
                                 read_string(Out, _, _),
                                 read_string(Err, _, _),
                                 close(Out), close(Err).

run_build_step(_, BuildCmd) :- (BuildCmd = '' ; BuildCmd = ""), !.
run_build_step(LocalDir, BuildCmd) :- format("Running build: ~w in ~w~n", [BuildCmd, LocalDir]),
                                      process_create(path(sh),
                                                     [BuildCmd],
                                                     [cwd(LocalDir),
                                                      stdout(pipe(Out)), stderr(pipe(Err))]),
                                      read_string(Out, _, _),
                                      read_string(Err, _, _),
                                      close(Out), close(Err).
