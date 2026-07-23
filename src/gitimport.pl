:- use_module(library(filesex)).
:- use_module(library(process)).
:- use_module(library(random)).

:- dynamic acquired_git_dep/2.
:- dynamic git_library_path/2.

% Runtime git-import! is a core primitive.  Declarative git-dependency forms use
% the same acquisition machinery before the file's runnable forms execute.
'git-import!'(Url, true) :-
    'git-import!'(Url, '', './repos', true).
'git-import!'(Url, Build, true) :-
    'git-import!'(Url, Build, './repos', true).
'git-import!'(Url0, Build0, Base0, true) :-
    maplist(git_atom, [Url0, Build0, Base0], [Url, Build, Base]),
    acquire_unpinned_repository('git-import!', Url, Build, Base, Name, LocalDir),
    register_git_library_path(Name, LocalDir).
'git-import!'(Url0, Build0, Base0, Rev0, true) :-
    maplist(git_atom, [Url0, Build0, Base0], [Url, Build, Base]),
    git_validate_sha('git-import!', Rev0, Rev),
    acquire_pinned_repository('git-import!', Url, Build, Base, Rev, Name, LocalDir),
    register_git_library_path(Name, LocalDir),
    acquire_manifest_dependencies(LocalDir).

% Collect and satisfy the pinned dependencies declared by one parsed file.
acquire_declared_dependencies(ParsedForms) :-
    forall(member(parsed(expression, _, ['git-dependency'|Args]), ParsedForms),
           acquire_git_declaration(Args)).

acquire_git_declaration([Url, Rev]) :- !,
    acquire_git_declaration([Url, Rev, ""]).
acquire_git_declaration([Url, Rev, Build]) :- !,
    acquire_git_declaration([Url, Rev, Build, "./repos"]).
acquire_git_declaration([Url0, Rev0, Build0, Base0]) :- !,
    maplist(git_atom, [Url0, Build0, Base0], [Url, Build, Base]),
    git_validate_sha('git-dependency', Rev0, Rev),
    acquire_git_dependency(Url, Rev, Build, Base).
acquire_git_declaration(Args) :-
    throw(error(domain_error(git_dependency_declaration, Args),
                context('git-dependency',
                        'expected (git-dependency url rev [build [basedir]])'))).

% A declarative URL has one revision per process.  This prevents two manifests
% from silently retargeting the same checkout beneath already-loaded code.
acquire_git_dependency(Url, Rev, Build, Base) :-
    ( acquired_git_dep(Url, Previous)
      -> ( Previous == Rev
           -> true
            ; throw(error(domain_error(conflicting_git_dependency, Url),
                          context('git-dependency', two_revisions(Previous, Rev)))) )
       ; acquire_pinned_repository('git-dependency', Url, Build, Base, Rev,
                                   Name, LocalDir),
         register_git_library_path(Name, LocalDir),
         assertz(acquired_git_dep(Url, Rev)),
         acquire_manifest_dependencies(LocalDir) ).

% A checkout can declare transitive pinned dependencies in deps.metta.
acquire_manifest_dependencies(LocalDir) :-
    directory_file_path(LocalDir, 'deps.metta', Manifest),
    ( exists_file(Manifest)
      -> read_file_to_string(Manifest, Source, []),
         string_codes(Source, Codes0),
         strip(Codes0, 0, Codes),
         phrase(top_forms(Forms, 1), Codes),
         forall(( member(form(FormSource), Forms),
                  sread(FormSource, Term),
                  Term = ['git-dependency'|Args] ),
                acquire_git_declaration(Args))
       ; true ).

% The name is recorded explicitly with the canonical checkout root.  library/3
% can therefore perform an exact lookup instead of guessing from path suffixes.
register_git_library_path(Name0, LocalDir) :-
    git_atom(Name0, Name),
    absolute_file_name(LocalDir, CanonDir,
                       [file_type(directory), file_errors(fail)]),
    ( git_library_path(Name, CanonDir)
      -> true
       ; git_library_path(Name, Existing)
      -> throw(error(permission_error(register, git_library, Name),
                     context(register_git_library_path/2,
                             conflicting_paths(Existing, CanonDir))))
       ; assertz(git_library_path(Name, CanonDir)) ).

acquire_unpinned_repository(Context, Url, Build, Base, Name, LocalDir) :-
    make_directory_path(Base),
    git_repository_name(Url, Name),
    directory_file_path(Base, Name, LocalDir),
    atom_concat(LocalDir, '.lock', LockFile),
    setup_call_cleanup(
        open(LockFile, append, Lock, [lock(write)]),
        acquire_unpinned_locked(Context, Url, Build, Base, Name, LocalDir),
        close(Lock)).

acquire_unpinned_locked(Context, Url, Build, Base, Name, LocalDir) :-
    ( exists_directory(LocalDir)
      -> ensure_git_checkout(Context, LocalDir),
         ensure_git_origin(Context, LocalDir, Url)
       ; exists_file(LocalDir)
      -> throw(error(permission_error(create, git_checkout, LocalDir),
                     context(Context, 'target exists and is not a directory')))
       ; create_staging_directory(Context, Base, Name, StagingRoot),
         directory_file_path(StagingRoot, Name, StagingDir),
         setup_call_cleanup(
             true,
             ( git_process(Context, 'clone repository', path(git),
                           [clone, '--depth', '1', Url, StagingDir], []),
               run_git_build(Context, StagingDir, Build),
               rename_file(StagingDir, LocalDir) ),
             delete_directory_and_contents(StagingRoot)) ).

acquire_pinned_repository(Context, Url, Build, Base, Rev, Name, LocalDir) :-
    make_directory_path(Base),
    git_repository_name(Url, Name),
    directory_file_path(Base, Name, LocalDir),
    atom_concat(LocalDir, '.lock', LockFile),
    setup_call_cleanup(
        open(LockFile, append, Lock, [lock(write)]),
        acquire_pinned_locked(Context, Url, Build, Base, Name, LocalDir, Rev),
        close(Lock)).

acquire_pinned_locked(Context, Url, Build, Base, Name, LocalDir, Rev) :-
    ( exists_directory(LocalDir)
      -> ensure_git_checkout(Context, LocalDir),
         ensure_git_origin(Context, LocalDir, Url),
         git_output(Context, 'resolve current HEAD', LocalDir,
                    ['rev-parse', '--verify', 'HEAD^{commit}'], Head),
         ( Head == Rev
           -> true
            ; ensure_clean_checkout(Context, LocalDir),
              fetch_git_commit(Context, LocalDir, Rev),
              checkout_git_commit(Context, LocalDir, Rev),
              verify_git_head(Context, LocalDir, Rev),
              run_git_build(Context, LocalDir, Build) )
       ; exists_file(LocalDir)
      -> throw(error(permission_error(create, git_checkout, LocalDir),
                     context(Context, 'target exists and is not a directory')))
       ; create_staging_directory(Context, Base, Name, StagingRoot),
         directory_file_path(StagingRoot, Name, StagingDir),
         setup_call_cleanup(
             true,
             ( git_process(Context, 'clone repository', path(git),
                           [clone, '--no-checkout', Url, StagingDir], []),
               fetch_git_commit(Context, StagingDir, Rev),
               checkout_git_commit(Context, StagingDir, Rev),
               verify_git_head(Context, StagingDir, Rev),
               run_git_build(Context, StagingDir, Build),
               rename_file(StagingDir, LocalDir) ),
             delete_directory_and_contents(StagingRoot)) ).

git_repository_name(Url, Name) :-
    atom_string(Url, UrlString),
    split_string(UrlString, "/:", "/:", Parts0),
    exclude(==(""), Parts0, Parts),
    last(Parts, Last),
    ( string_concat(NameString, ".git", Last) -> true ; NameString = Last ),
    atom_string(Name, NameString).

git_validate_sha(_Context, Rev0, Rev) :-
    git_atom(Rev0, Candidate),
    atom_codes(Candidate, Codes),
    length(Codes, 40),
    maplist(git_hex_code, Codes), !,
    downcase_atom(Candidate, Rev).
git_validate_sha(Context, Rev, _) :-
    throw(error(domain_error(full_git_commit_sha, Rev),
                context(Context,
                        'commit must be exactly 40 hexadecimal characters'))).

git_hex_code(Code) :- between(0'0, 0'9, Code), !.
git_hex_code(Code) :- between(0'a, 0'f, Code), !.
git_hex_code(Code) :- between(0'A, 0'F, Code).

git_atom(Value, Atom) :- atom(Value), !, Atom = Value.
git_atom(Value, Atom) :- string(Value), !, atom_string(Atom, Value).
git_atom(Value, _) :- throw(error(type_error(text, Value), none)).

create_staging_directory(_Context, Base, Name, StagingRoot) :-
    between(1, 100, _),
    random_between(100000000, 999999999, Suffix),
    format(atom(TempName), '.~w.git-acquire-~d', [Name, Suffix]),
    directory_file_path(Base, TempName, Candidate),
    catch(make_directory(Candidate), error(existence_error(_, _), _), fail), !,
    StagingRoot = Candidate.
create_staging_directory(Context, _, Name, _) :-
    throw(error(resource_error(temporary_directory), context(Context, Name))).

ensure_git_checkout(Context, LocalDir) :-
    catch(git_output(Context, 'validate Git checkout', LocalDir,
                     ['rev-parse', '--is-inside-work-tree'], IsGit), _, fail),
    IsGit == true, !.
ensure_git_checkout(Context, LocalDir) :-
    throw(error(domain_error(git_checkout, LocalDir),
                context(Context, 'existing target is not a Git checkout'))).

ensure_git_origin(Context, LocalDir, Url) :-
    git_output(Context, 'read origin URL', LocalDir,
               [remote, 'get-url', origin], Actual),
    ( Actual == Url
      -> true
       ; throw(error(domain_error(git_origin, Actual),
                     context(Context, expected(Url)))) ).

ensure_clean_checkout(Context, LocalDir) :-
    git_output(Context, 'check checkout cleanliness', LocalDir,
               [status, '--porcelain', '--untracked-files=all'], Status),
    ( Status == ''
      -> true
       ; throw(error(permission_error(modify, dirty_git_checkout, LocalDir),
                     context(Context, Status))) ).

fetch_git_commit(Context, LocalDir, Rev) :-
    git_process(Context, 'fetch requested commit', path(git),
                [fetch, '--no-tags', origin, Rev], [cwd(LocalDir)]),
    atom_concat(Rev, '^{commit}', CommitObject),
    git_process(Context, 'resolve requested commit object', path(git),
                ['cat-file', '-e', CommitObject], [cwd(LocalDir)]).

checkout_git_commit(Context, LocalDir, Rev) :-
    git_process(Context, 'checkout requested commit', path(git),
                [checkout, '--detach', Rev], [cwd(LocalDir)]).

verify_git_head(Context, LocalDir, Rev) :-
    git_output(Context, 'verify checked out HEAD', LocalDir,
               ['rev-parse', '--verify', 'HEAD^{commit}'], Head),
    ( Head == Rev
      -> true
       ; throw(error(consistency_error(git_head, Rev, Head),
                     context(Context, LocalDir))) ).

run_git_build(_, _, Build) :- Build == '', !.
run_git_build(Context, LocalDir, Build) :-
    format("Running build: ~w in ~w~n", [Build, LocalDir]),
    git_process(Context, 'build imported repository', path(sh),
                [Build], [cwd(LocalDir)]).

git_output(Context, Operation, LocalDir, Args, OutputAtom) :-
    git_process_output(Context, Operation, path(git), Args,
                       [cwd(LocalDir)], Output),
    normalize_space(atom(OutputAtom), Output).

git_process(Context, Operation, Executable, Args, Options) :-
    git_process_output(Context, Operation, Executable, Args, Options, _).

git_process_output(Context, Operation, Executable, Args, Options, Output) :-
    tmp_file(git_acquire_stdout, OutFile),
    tmp_file(git_acquire_stderr, ErrFile),
    open(OutFile, write, Out),
    open(ErrFile, write, Err),
    setup_call_cleanup(
        true,
        ( setup_call_cleanup(
              true,
              run_logged_git_process(Context, Operation, Executable, Args,
                                     Options, Out, Err, Status),
              (close(Out), close(Err))),
          read_git_process_log(OutFile, Stdout),
          read_git_process_log(ErrFile, Stderr) ),
        cleanup_git_process_logs(OutFile, ErrFile)),
    string_concat(Stdout, Stderr, Output),
    ( Status == exit(0)
      -> true
       ; throw(error(process_error(Operation, Status, Output),
                     context(Context, Args))) ).

run_logged_git_process(Context, Operation, Executable, Args, Options,
                       Out, Err, Status) :-
    append(Options,
           [stdout(stream(Out)), stderr(stream(Err)), process(PID)],
           ProcessOptions),
    catch(process_create(Executable, Args, ProcessOptions), Error,
          throw(error(process_error(Operation, Error),
                      context(Context, Args)))),
    process_wait(PID, Status).

read_git_process_log(File, Output) :-
    setup_call_cleanup(open(File, read, In),
                       read_string(In, _, Output),
                       close(In)).

cleanup_git_process_logs(OutFile, ErrFile) :-
    ( exists_file(OutFile) -> delete_file(OutFile) ; true ),
    ( exists_file(ErrFile) -> delete_file(ErrFile) ; true ).
