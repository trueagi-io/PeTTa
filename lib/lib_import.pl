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

%Perform simple transformation from S-Expression to space-rel predicate.
%Unlike the upstream version this never emits broken Prolog: every value is
%either written raw (safe tokens only) or quoted/escaped via writeq, stray
%whitespace is trimmed, blank and non-parenthesized lines are skipped as
%comments instead of aborting the whole conversion.
convert_line(Line0, Space, Out) :- string_trim(Line0, Trimmed),
                                   ( Trimmed == ""
                                     -> format(Out, "%% skipped blank line~n", [])
                                      ; sub_string(Trimmed, 0, 1, _, "("),
                                        sub_string(Trimmed, _, 1, 0, ")")
                                        -> sub_string(Trimmed, 1, _, 1, Inner0),
                                           string_chars(Inner0, Chars),
                                           transform_chars(Chars, false, NewChars),
                                           format(Out, "'~w'(", [Space]),
                                           emit_chars(NewChars, false, [], no, Out),
                                           format(Out, ").~n", [])
                                         ; format(Out, "%% skipped non-parenthesized line: ~s~n", [Trimmed])
                                   ).

%Emit the transformed character list as Prolog term arguments.
%Field is the reversed character buffer of the current value, Pend is
%yes/no: a comma is owed before the next value at this nesting level.
%Emission rules guarantee the written clause always parses:
%  - double-quoted segments are copied verbatim (upstream parity)
%  - plain fields are parsed back; numbers stay raw, everything else is
%    written with writeq so apostrophes, %-signs, leading underscores and
%    uppercase keys become proper quoted atoms instead of syntax errors
%    or silent Prolog variables
%  - empty fields (double spaces in the source) are dropped, not emitted
emit_chars([], false, Field, Pend, Out) :- !, flush_field(Field, Pend, Out).
emit_chars([], true, _Field, Pend, Out) :- !, put_char(Out, '"'), flush_field([], Pend, Out).
emit_chars(['"'|T], false, Field, Pend0, Out) :- !,
                                                flush_field(Field, Pend0, Out, Pend1),
                                                maybe_comma(Pend1, Out),
                                                put_char(Out, '"'),
                                                emit_chars(T, true, [], yes, Out).
emit_chars(['"'|T], true, _Field, _Pend, Out) :- !,
                                                 put_char(Out, '"'),
                                                 emit_chars(T, false, [], yes, Out).
emit_chars([C|T], true, Field, Pend, Out) :- !,
                                             put_char(Out, C),
                                             emit_chars(T, true, Field, Pend, Out).
emit_chars(['['|T], false, Field, Pend0, Out) :- !,
                                                 flush_field(Field, Pend0, Out, Pend1),
                                                 maybe_comma(Pend1, Out),
                                                 put_char(Out, '['),
                                                 emit_chars(T, false, [], no, Out).
emit_chars([']'|T], false, Field, Pend0, Out) :- !,
                                                 flush_field(Field, Pend0, Out, _),
                                                 put_char(Out, ']'),
                                                 emit_chars(T, false, [], yes, Out).
emit_chars([','|T], false, Field, Pend0, Out) :- !,
                                                 flush_field(Field, Pend0, Out, Pend1),
                                                 emit_chars(T, false, [], Pend1, Out).
emit_chars([C|T], false, Field, Pend, Out) :- emit_chars(T, false, [C|Field], Pend, Out).

%Flush the pending field: write it (with owed comma) and report the new
%pending-comma state.
flush_field([], Pend, _Out, Pend) :- !.
flush_field(Field, Pend0, Out, yes) :- maybe_comma(Pend0, Out),
                                       write_field(Field, Out).

flush_field(Field, Pend, Out) :- flush_field(Field, Pend, Out, _).

maybe_comma(yes, Out) :- !, put_char(Out, ',').
maybe_comma(no, _Out).

%Write one field: parse it back; numbers are written raw, valid atoms and
%operators with writeq (which quotes anything unsafe), unparseable text
%(apostrophes, %-signs, digit-leading tokens, ...) becomes a quoted atom.
write_field(Field, Out) :- reverse(Field, Chars),
                           atomic_list_concat(Chars, A),
                           ( catch(read_term_from_atom(A, T, []), _, fail),
                             T \== end_of_file
                             -> ( ( integer(T) ; float(T) )
                                  -> write(Out, T)
                                   ; var(T)
                                  -> writeq(Out, A)
                                   ; writeq(Out, T) )
                              ; writeq(Out, A)
                           ).

%Trim leading/trailing whitespace (spaces, tabs, CR) from a string.
string_trim(S0, S) :- string_codes(S0, C0),
                      strip_ws(C0, C1),
                      reverse(C1, C2),
                      strip_ws(C2, C3),
                      reverse(C3, C4),
                      ( C4 == [] -> S = "" ; string_codes(S, C4) ).

strip_ws([], []).
strip_ws([C|T], R) :- is_ws_code(C), !, strip_ws(T, R).
strip_ws(L, L).

is_ws_code(0' ).
is_ws_code(0'\t).
is_ws_code(0'\r).
is_ws_code(0'\n).

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
                                                   register_library_path(LocalDir).

register_library_path(Path0) :- absolute_file_name(Path0, Path, [file_type(directory), file_errors(fail)]),
                                ( library_path(Path) -> true ; asserta(library_path(Path)) ).

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
    register_library_path(LocalDir).

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
