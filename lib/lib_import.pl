:- use_module(library(filesex)).
:- use_module(library(process)).
:- use_module(library(random)).

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
                                   string_chars(Inner0, Chars),
                                   transform_chars(Chars, false, NewChars),
                                   string_chars(Inner3, NewChars),
                                   format(Out, "'~w'(~w).~n", [Space, Inner3]).

%End of recursion
transform_chars([], _, []) :- !.

%Toggle quote mode ON
transform_chars(['"'|T], false, ['"'|R]) :- !, 
                                            transform_chars(T, true, R).

%Toggle quote mode OFF
transform_chars(['"'|T], true, ['"'|R]) :- !,
                                           transform_chars(T, false, R).

%Replace ( with [ only outside quotes
transform_chars(['('|T], false, ['['|R]) :- !, 
                                            transform_chars(T, false, R).

%Replace ) with ] only outside quotes
transform_chars([')'|T], false, [']'|R]) :- !,
                                            transform_chars(T, false, R).

%Replace spaces with commas only outside quotes
transform_chars([' '|T], false, [','|R]) :- !,
                                            transform_chars(T, false, R).

%Keep all other characters unchanged
transform_chars([H|T], Q, [H|R]) :- transform_chars(T, Q, R).
%Helper predicate for string replacement:
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
                                                                                 ; make_directory_path(BaseDir) ),
                                                   repo_name_from_git(GitPath, Name),
                                                   directory_file_path(BaseDir, Name, LocalDir),
                                                   ( exists_directory(LocalDir) -> true
                                                                                 ; clone_repo(GitPath, LocalDir),
                                                                                   run_build_step(LocalDir, BuildCmd) ),
                                                   register_git_library_path(LocalDir).

register_git_library_path(LocalDir) :- absolute_file_name(LocalDir, CanonDir, [file_type(directory), file_errors(fail)]),
                                       ( current_predicate(library_path/1), library_path(CanonDir)
                                         -> true
                                          ; asserta(library_path(CanonDir)) ).

% Reproducible import: URL, build command, base directory, full commit SHA.
'git-import!'(GitPath, BuildCmd, BaseDir, CommitSHA, true) :-
    validate_commit_sha(CommitSHA, Commit),
    make_directory_path(BaseDir),
    repo_name_from_git(GitPath, Name),
    directory_file_path(BaseDir, Name, LocalDir),
    atom_concat(LocalDir, '.lock', LockFile),
    setup_call_cleanup(open(LockFile, append, Lock, [lock(write)]),
                       pinned_import_locked(GitPath, BuildCmd, BaseDir, Name, LocalDir, Commit),
                       close(Lock)),
    register_git_library_path(LocalDir).

validate_commit_sha(CommitSHA, Commit) :-
    atom_string(CommitSHA, CommitString),
    string_length(CommitString, 40),
    string_codes(CommitString, Codes),
    maplist(hex_code, Codes), !,
    string_lower(CommitString, Lower),
    atom_string(Commit, Lower).
validate_commit_sha(CommitSHA, _) :-
    throw(error(domain_error(full_git_commit_sha, CommitSHA),
                context('git-import!'/5,
                        'commit SHA must be exactly 40 hexadecimal characters'))).

hex_code(Code) :- between(0'0, 0'9, Code), !.
hex_code(Code) :- between(0'a, 0'f, Code), !.
hex_code(Code) :- between(0'A, 0'F, Code).

pinned_import_locked(GitPath, BuildCmd, BaseDir, Name, LocalDir, Commit) :-
    ( exists_directory(LocalDir)
      -> pinned_existing_checkout(GitPath, BuildCmd, LocalDir, Commit)
       ; exists_file(LocalDir)
      -> throw(error(permission_error(create, git_checkout, LocalDir),
                     context('git-import!'/5, 'target exists and is not a directory')))
       ; pinned_fresh_checkout(GitPath, BuildCmd, BaseDir, Name, LocalDir, Commit) ).

pinned_fresh_checkout(GitPath, BuildCmd, BaseDir, Name, LocalDir, Commit) :-
    create_staging_directory(BaseDir, Name, StagingRoot),
    directory_file_path(StagingRoot, Name, StagingDir),
    setup_call_cleanup(
        true,
        ( checked_process('clone repository', path(git),
                          [clone, '--no-checkout', GitPath, StagingDir], []),
          resolve_requested_commit(StagingDir, Commit),
          checkout_detached(StagingDir, Commit),
          verify_head(StagingDir, Commit),
          run_checked_build_step(StagingDir, BuildCmd),
          rename_file(StagingDir, LocalDir) ),
        delete_directory_and_contents(StagingRoot)).

create_staging_directory(BaseDir, Name, StagingRoot) :-
    between(1, 100, _),
    random_between(100000000, 999999999, Suffix),
    format(atom(TempName), '.~w.git-import-~d', [Name, Suffix]),
    directory_file_path(BaseDir, TempName, Candidate),
    catch(make_directory(Candidate), error(existence_error(_, _), _), fail), !,
    StagingRoot = Candidate.
create_staging_directory(_, Name, _) :-
    throw(error(resource_error(temporary_directory), context('git-import!'/5, Name))).

pinned_existing_checkout(GitPath, BuildCmd, LocalDir, Commit) :-
    ensure_git_checkout(LocalDir),
    ensure_expected_origin(LocalDir, GitPath),
    git_output('resolve current HEAD', LocalDir, ['rev-parse', '--verify', 'HEAD^{commit}'], Head),
    ( Head == Commit
      -> true
       ; ensure_clean_checkout(LocalDir),
         resolve_requested_commit(LocalDir, Commit),
         checkout_detached(LocalDir, Commit),
         verify_head(LocalDir, Commit),
         run_checked_build_step(LocalDir, BuildCmd) ).

ensure_git_checkout(LocalDir) :-
    catch(git_output('validate Git checkout', LocalDir,
                     ['rev-parse', '--is-inside-work-tree'], IsGit), _, fail),
    IsGit == true, !.
ensure_git_checkout(LocalDir) :-
    throw(error(domain_error(git_checkout, LocalDir),
                context('git-import!'/5, 'existing target is not a Git checkout'))).

ensure_expected_origin(LocalDir, GitPath) :-
    git_output('read origin URL', LocalDir, [remote, 'get-url', origin], Actual),
    atom_string(GitPath, ExpectedString),
    atom_string(Expected, ExpectedString),
    ( Actual == Expected -> true
    ; throw(error(domain_error(git_origin, Actual),
                  context('git-import!'/5, expected(Expected)))) ).

ensure_clean_checkout(LocalDir) :-
    git_output('check checkout cleanliness', LocalDir,
               [status, '--porcelain', '--untracked-files=all'], Status),
    ( Status == '' -> true
    ; throw(error(permission_error(modify, dirty_git_checkout, LocalDir),
                  context('git-import!'/5, Status))) ).

resolve_requested_commit(LocalDir, Commit) :-
    checked_process('fetch requested commit', path(git),
                    [fetch, '--no-tags', origin, Commit], [cwd(LocalDir)]),
    atom_concat(Commit, '^{commit}', CommitObject),
    checked_process('resolve requested commit object', path(git),
                    ['cat-file', '-e', CommitObject], [cwd(LocalDir)]).

checkout_detached(LocalDir, Commit) :-
    checked_process('checkout requested commit', path(git),
                    [checkout, '--detach', Commit], [cwd(LocalDir)]).

verify_head(LocalDir, Commit) :-
    git_output('verify checked out HEAD', LocalDir,
               ['rev-parse', '--verify', 'HEAD^{commit}'], Head),
    ( Head == Commit -> true
    ; throw(error(consistency_error(git_head, Commit, Head),
                  context('git-import!'/5, LocalDir))) ).

git_output(Operation, LocalDir, Args, OutputAtom) :-
    checked_process_output(Operation, path(git), Args, [cwd(LocalDir)], Output),
    normalize_space(atom(OutputAtom), Output).

run_checked_build_step(_, BuildCmd) :- (BuildCmd = '' ; BuildCmd = ""), !.
run_checked_build_step(LocalDir, BuildCmd) :-
    format("Running build: ~w in ~w~n", [BuildCmd, LocalDir]),
    checked_process('build imported repository', path(sh), [BuildCmd], [cwd(LocalDir)]).

checked_process(Operation, Executable, Args, Options) :-
    checked_process_output(Operation, Executable, Args, Options, _).

checked_process_output(Operation, Executable, Args, Options, Output) :-
    tmp_file(git_import_stdout, OutFile),
    tmp_file(git_import_stderr, ErrFile),
    open(OutFile, write, Out),
    open(ErrFile, write, Err),
    setup_call_cleanup(
        true,
        ( setup_call_cleanup(
              true,
              run_logged_process(Operation, Executable, Args, Options, Out, Err, Status),
              (close(Out), close(Err))),
          read_process_log(OutFile, Stdout),
          read_process_log(ErrFile, Stderr) ),
        cleanup_process_logs(OutFile, ErrFile)),
    string_concat(Stdout, Stderr, Output),
    ( Status == exit(0) -> true
    ; throw(error(process_error(Operation, Status, Output),
                  context('git-import!'/5, Args))) ).

read_process_log(File, Output) :-
    setup_call_cleanup(open(File, read, In), read_string(In, _, Output), close(In)).

cleanup_process_logs(OutFile, ErrFile) :-
    ( exists_file(OutFile) -> delete_file(OutFile) ; true ),
    ( exists_file(ErrFile) -> delete_file(ErrFile) ; true ).

run_logged_process(Operation, Executable, Args, Options, Out, Err, Status) :-
    append(Options, [stdout(stream(Out)), stderr(stream(Err)), process(PID)], ProcessOptions),
    catch(process_create(Executable, Args, ProcessOptions), Error,
          throw(error(process_error(Operation, Error), context('git-import!'/5, Args)))),
    process_wait(PID, Status).

%Extract "repo" from ".../repo.git" or "...:repo.git":
repo_name_from_git(GitPath, Name) :- atom_string(GitPath, S),
                                     split_string(S, "/:", "/:", Parts),
                                     last(Parts, Last0),
                                     ( sub_string(Last0, _, 4, 0, ".git")
                                       -> sub_string(Last0, 0, _, 4, Last)
                                        ; Last = Last0 ),
                                     atom_string(Name, Last).

clone_repo(GitPath, LocalDir) :- format("Cloning ~w into ~w~n", [GitPath, LocalDir]),
                                 run_git_import_process(path(git),
                                                        ['clone', '--depth', '1', GitPath, LocalDir],
                                                        [],
                                                        clone(GitPath)).

run_build_step(_, BuildCmd) :- (BuildCmd = '' ; BuildCmd = ""), !.
run_build_step(LocalDir, BuildCmd) :- format("Running build: ~w in ~w~n", [BuildCmd, LocalDir]),
                                      run_git_import_process(path(sh),
                                                             [BuildCmd],
                                                             [cwd(LocalDir)],
                                                             build(BuildCmd)).

run_git_import_process(Executable, Args, Options, Operation) :-
    process_create(Executable, Args, [process(PID)|Options]),
    process_wait(PID, Status),
    ( Status = exit(0)
      -> true
       ; throw(error(process_error(Operation, Status), context('git-import!', Operation))) ).
