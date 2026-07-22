%Declarative, commit-pinned git dependencies, acquired before a file's forms run.
%A plain (git-dependency url rev [build [basedir]]) form declares that the program
%needs the repository at exactly that commit. Declarations are collected after a
%file is parsed and satisfied before any of its forms execute, so by the time an
%import resolves a (library ...) path the checkout exists at the pinned revision,
%on every machine, no matter what an earlier run left on disk. The runnable
%git-import! in lib_import.pl remains for dynamic acquisition.

:- dynamic acquired_git_dep/2.

%Collect and satisfy the git-dependency declarations of one parsed file:
acquire_declared_dependencies(ParsedForms) :-
    forall(member(parsed(expression, _, ['git-dependency'|Args]), ParsedForms),
           acquire_git_declaration(Args)).

acquire_git_declaration([Url, Rev]) :- !, acquire_git_declaration([Url, Rev, ""]).
acquire_git_declaration([Url, Rev, Build]) :- !, acquire_git_declaration([Url, Rev, Build, "./repos"]).
acquire_git_declaration([Url0, Rev0, Build0, Base0]) :- !,
    atom_string(Url, Url0),
    atom_string(Build, Build0),
    atom_string(Base, Base0),
    acquire_git_dependency(Url, Rev0, Build, Base).
acquire_git_declaration(Args) :-
    throw(error(domain_error(git_dependency_declaration, Args),
                context('git-dependency', 'expected (git-dependency url rev [build [basedir]])'))).

%Acquire one pinned dependency. Idempotent per (url, rev); the same url pinned to
%two different revisions in one process is a conflict, because both would claim
%the same checkout directory and library name:
acquire_git_dependency(Url, Rev0, BuildCmd, BaseDir) :-
    git_dep_validate_sha(Rev0, Rev),
    ( acquired_git_dep(Url, PrevRev)
      -> ( PrevRev == Rev
           -> true
            ; throw(error(domain_error(conflicting_git_dependency, Url),
                          context('git-dependency', two_revisions(PrevRev, Rev)))) )
       ; make_directory_path(BaseDir),
         git_dep_repo_name(Url, Name),
         directory_file_path(BaseDir, Name, LocalDir),
         atom_concat(LocalDir, '.lock', LockFile),
         setup_call_cleanup(open(LockFile, append, Lock, [lock(write)]),
                            git_dep_locked(Url, BuildCmd, BaseDir, Name, LocalDir, Rev),
                            close(Lock)),
         git_dep_register_library_path(LocalDir),
         assertz(acquired_git_dep(Url, Rev)),
         acquire_manifest_dependencies(LocalDir) ).

%A checkout may declare its own dependencies in deps.metta at its root:
acquire_manifest_dependencies(LocalDir) :-
    directory_file_path(LocalDir, 'deps.metta', Manifest),
    ( exists_file(Manifest)
      -> read_file_to_string(Manifest, S, []),
         string_codes(S, Cs),
         strip(Cs, 0, Codes),
         phrase(top_forms(Forms, 1), Codes),
         forall(( member(form(FormStr), Forms),
                  sread(FormStr, Term),
                  Term = ['git-dependency'|Args] ),
                acquire_git_declaration(Args))
       ; true ).

git_dep_validate_sha(Rev0, Rev) :-
    atom_string(Rev0, RevString),
    string_length(RevString, 40),
    string_codes(RevString, Codes),
    maplist(git_dep_hex_code, Codes), !,
    string_lower(RevString, Lower),
    atom_string(Rev, Lower).
git_dep_validate_sha(Rev0, _) :-
    throw(error(domain_error(full_git_commit_sha, Rev0),
                context('git-dependency', 'commit must be exactly 40 hexadecimal characters'))).

git_dep_hex_code(Code) :- between(0'0, 0'9, Code), !.
git_dep_hex_code(Code) :- between(0'a, 0'f, Code), !.
git_dep_hex_code(Code) :- between(0'A, 0'F, Code).

%Extract "repo" from ".../repo.git", "...:repo.git" or a plain ".../repo" path:
git_dep_repo_name(Url, Name) :- atom_string(Url, S),
                                split_string(S, "/:", "/:", Parts),
                                exclude(==(""), Parts, NonEmpty),
                                last(NonEmpty, LastPart),
                                ( string_concat(Base, ".git", LastPart) -> true ; Base = LastPart ),
                                atom_string(Name, Base).

git_dep_register_library_path(LocalDir) :-
    absolute_file_name(LocalDir, CanonDir, [file_type(directory), file_errors(fail)]),
    ( library_path(CanonDir) -> true ; asserta(library_path(CanonDir)) ).

git_dep_locked(Url, BuildCmd, BaseDir, Name, LocalDir, Rev) :-
    ( exists_directory(LocalDir)
      -> git_dep_existing_checkout(Url, BuildCmd, LocalDir, Rev)
       ; exists_file(LocalDir)
      -> throw(error(permission_error(create, git_checkout, LocalDir),
                     context('git-dependency', 'target exists and is not a directory')))
       ; git_dep_fresh_checkout(Url, BuildCmd, BaseDir, Name, LocalDir, Rev) ).

%Clone into a staging directory, pin, build, then atomically rename into place,
%so a failed acquisition leaves no partial checkout behind:
git_dep_fresh_checkout(Url, BuildCmd, BaseDir, Name, LocalDir, Rev) :-
    git_dep_staging_directory(BaseDir, Name, StagingRoot),
    directory_file_path(StagingRoot, Name, StagingDir),
    setup_call_cleanup(
        true,
        ( git_dep_process('clone repository', path(git),
                          [clone, '--no-checkout', Url, StagingDir], []),
          git_dep_fetch_commit(StagingDir, Rev),
          git_dep_checkout_detached(StagingDir, Rev),
          git_dep_verify_head(StagingDir, Rev),
          git_dep_build_step(StagingDir, BuildCmd),
          rename_file(StagingDir, LocalDir) ),
        delete_directory_and_contents(StagingRoot)).

git_dep_staging_directory(BaseDir, Name, StagingRoot) :-
    between(1, 100, _),
    random_between(100000000, 999999999, Suffix),
    format(atom(TempName), '.~w.git-dependency-~d', [Name, Suffix]),
    directory_file_path(BaseDir, TempName, Candidate),
    catch(make_directory(Candidate), error(existence_error(_, _), _), fail), !,
    StagingRoot = Candidate.
git_dep_staging_directory(_, Name, _) :-
    throw(error(resource_error(temporary_directory), context('git-dependency', Name))).

%An existing checkout is reused when it is a clean clone of the same origin;
%a different pinned revision retargets it, and local modifications are refused:
git_dep_existing_checkout(Url, BuildCmd, LocalDir, Rev) :-
    git_dep_ensure_checkout(LocalDir),
    git_dep_ensure_origin(LocalDir, Url),
    git_dep_output('resolve current HEAD', LocalDir, ['rev-parse', '--verify', 'HEAD^{commit}'], Head),
    ( Head == Rev
      -> true
       ; git_dep_ensure_clean(LocalDir),
         git_dep_fetch_commit(LocalDir, Rev),
         git_dep_checkout_detached(LocalDir, Rev),
         git_dep_verify_head(LocalDir, Rev),
         git_dep_build_step(LocalDir, BuildCmd) ).

git_dep_ensure_checkout(LocalDir) :-
    catch(git_dep_output('validate git checkout', LocalDir,
                         ['rev-parse', '--is-inside-work-tree'], IsGit), _, fail),
    IsGit == true, !.
git_dep_ensure_checkout(LocalDir) :-
    throw(error(domain_error(git_checkout, LocalDir),
                context('git-dependency', 'existing target is not a git checkout'))).

git_dep_ensure_origin(LocalDir, Url) :-
    git_dep_output('read origin url', LocalDir, [remote, 'get-url', origin], Actual),
    atom_string(Expected, Url),
    ( Actual == Expected -> true
    ; throw(error(domain_error(git_origin, Actual),
                  context('git-dependency', expected(Expected)))) ).

git_dep_ensure_clean(LocalDir) :-
    git_dep_output('check checkout cleanliness', LocalDir,
                   [status, '--porcelain', '--untracked-files=all'], Status),
    ( Status == '' -> true
    ; throw(error(permission_error(modify, dirty_git_checkout, LocalDir),
                  context('git-dependency', Status))) ).

git_dep_fetch_commit(LocalDir, Rev) :-
    git_dep_process('fetch pinned commit', path(git),
                    [fetch, '--no-tags', origin, Rev], [cwd(LocalDir)]),
    atom_concat(Rev, '^{commit}', CommitObject),
    git_dep_process('resolve pinned commit object', path(git),
                    ['cat-file', '-e', CommitObject], [cwd(LocalDir)]).

git_dep_checkout_detached(LocalDir, Rev) :-
    git_dep_process('checkout pinned commit', path(git),
                    [checkout, '--detach', Rev], [cwd(LocalDir)]).

git_dep_verify_head(LocalDir, Rev) :-
    git_dep_output('verify checked out HEAD', LocalDir,
                   ['rev-parse', '--verify', 'HEAD^{commit}'], Head),
    ( Head == Rev -> true
    ; throw(error(consistency_error(git_head, Rev, Head),
                  context('git-dependency', LocalDir))) ).

git_dep_output(Operation, LocalDir, Args, OutputAtom) :-
    git_dep_process_output(Operation, path(git), Args, [cwd(LocalDir)], Output),
    normalize_space(atom(OutputAtom), Output).

git_dep_build_step(_, BuildCmd) :- (BuildCmd = '' ; BuildCmd = ""), !.
git_dep_build_step(LocalDir, BuildCmd) :-
    format("Running build: ~w in ~w~n", [BuildCmd, LocalDir]),
    git_dep_process('build dependency', path(sh), [BuildCmd], [cwd(LocalDir)]).

git_dep_process(Operation, Executable, Args, Options) :-
    git_dep_process_output(Operation, Executable, Args, Options, _).

%Run a process, capture stdout and stderr, and throw with both on a non-zero exit:
git_dep_process_output(Operation, Executable, Args, Options, Output) :-
    tmp_file(git_dep_stdout, OutFile),
    tmp_file(git_dep_stderr, ErrFile),
    open(OutFile, write, Out),
    open(ErrFile, write, Err),
    setup_call_cleanup(
        true,
        ( setup_call_cleanup(
              true,
              git_dep_run_logged(Operation, Executable, Args, Options, Out, Err, Status),
              (close(Out), close(Err))),
          git_dep_read_log(OutFile, Stdout),
          git_dep_read_log(ErrFile, Stderr) ),
        git_dep_cleanup_logs(OutFile, ErrFile)),
    string_concat(Stdout, Stderr, Output),
    ( Status == exit(0) -> true
    ; throw(error(process_error(Operation, Status, Output),
                  context('git-dependency', Args))) ).

git_dep_read_log(File, Output) :-
    setup_call_cleanup(open(File, read, In), read_string(In, _, Output), close(In)).

git_dep_cleanup_logs(OutFile, ErrFile) :-
    ( exists_file(OutFile) -> delete_file(OutFile) ; true ),
    ( exists_file(ErrFile) -> delete_file(ErrFile) ; true ).

git_dep_run_logged(Operation, Executable, Args, Options, Out, Err, Status) :-
    append(Options, [stdout(stream(Out)), stderr(stream(Err)), process(PID)], ProcessOptions),
    catch(process_create(Executable, Args, ProcessOptions), Error,
          throw(error(process_error(Operation, Error), context('git-dependency', Args)))),
    process_wait(PID, Status).
