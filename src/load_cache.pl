:- module(load_cache, [load_metta_file_cached/2, load_metta_file_cached/3]).

/** <module> Cached MeTTa loads

load_metta_file_cached(File, Results, Space) has the effect of
load_metta_file(File, Results, Space), but replays a stored snapshot of that
effect when the same load already ran from the same state.

A load is a function of the runtime state it starts from, the files it reads
and the PeTTa runtime. A snapshot records a fingerprint of the starting
state and of the runtime sources, the digest of every file the load read and
the source listing of each directory it read MeTTa files from. It is replayed
only when all of these still match; otherwise the file is loaded and the
snapshot rewritten. There is one snapshot per runtime, file, space and
command line, in $PETTA_LOAD_CACHE or else the user's cache directory.

The state is the dynamic database of every module that is not part of
SWI-Prolog itself, the global variables, the loaded source files and the
Prolog flags. The effect of a load is, per dynamic predicate, the clauses it
appended (or its whole clause list when it did more than append), the global
variables it set or deleted, the source files it loaded, the flags it changed
and the results of its runnables. A clause reference inside a stored term is
saved as the position of its clause and rebound on replay. Output printed
while loading is not replayed, so a cached load suits library files.

A load whose effect cannot be stored, such as one leaving a stream handle in
the database, runs as usual and writes no snapshot; a snapshot that cannot
be written is reported as a warning.
*/

:- use_module(library(sha)).
:- use_module(library(filesex)).
:- use_module(library(fastrw)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(ordsets)).
:- use_module(library(yall)).
:- use_module(library(occurs)).

:- dynamic clause_position/3.

snapshot_format(1).

load_metta_file_cached(File, Results) :- load_metta_file_cached(File, Results, '&self').

load_metta_file_cached(File, Results, Space) :-
    absolute_file_name(File, Path, [access(read)]),
    runtime_root(Root),
    snapshot_file(Root, Path, Space, SnapshotFile),
    runtime_state(State0),
    state_fingerprint(State0, Fingerprint),
    runtime_fingerprint(Root, Runtime),
    snapshot_format(Format),
    current_prolog_flag(version, Version),
    Header = header(Format, Version, Runtime, Fingerprint),
    (   replay_snapshot(SnapshotFile, Header, Results)
    ->  true
    ;   user:load_metta_file(Path, Results, Space),
        runtime_state(State1),
        (   state_effect(State0, State1, Results, Effect)
        ->  load_inputs(Path, State0, State1, Inputs),
            catch(write_snapshot(SnapshotFile, Header, Inputs, Effect), Error,
                  print_message(warning, Error))
        ;   true
        )
    ).

%%%%%%%%%% Runtime state %%%%%%%%%%

% state(Predicates, GlobalVariables, Sources, Flags), where Predicates holds
% pred(M:F/A, Properties, Refs) for every dynamic predicate in a fixed order.
runtime_state(state(Preds, Vars, Sources, Flags)) :-
    findall(M:F/A, state_predicate(M, F, A), Keys0),
    sort(Keys0, Keys),
    maplist(predicate_state, Keys, Preds),
    findall(K-V, nb_current(K, V), Vars0),
    sort(1, @<, Vars0, Vars),
    findall(S, source_file(S), Sources),
    findall(N-V, ( current_prolog_flag(N, V), \+ volatile_flag(N, V) ), Flags0),
    sort(Flags0, Flags).

% The Python bridge's call cache belongs to the host process, not to MeTTa.
state_predicate(M, F, A) :-
    current_prolog_flag(home, Home),
    current_module(M),
    \+ memberchk(M, [load_cache, janus]),
    module_property(M, class(user)),
    \+ ( module_property(M, file(File)), sub_atom(File, 0, _, _, Home) ),
    current_predicate(M:F/A),
    functor(H, F, A),
    predicate_property(M:H, dynamic),
    \+ predicate_property(M:H, imported_from(_)).

predicate_state(M:F/A, pred(M:F/A, Props, Refs)) :-
    functor(H, F, A),
    findall(P, ( member(P, [multifile, discontiguous, thread_local]),
                 predicate_property(M:H, P) ), Props),
    findall(R, clause(M:H, _, R), Refs).

volatile_flag(pid, _).
volatile_flag(_, V) :- blob(V, Type), \+ memberchk(Type, [text, reserved_symbol]).

% The fingerprint covers the stored form of every clause and global variable,
% so it is equal exactly when a load starting here sees the same state. Which
% SWI-Prolog libraries happen to be autoloaded already does not matter.
state_fingerprint(state(Preds, Vars, Sources, Flags), Fingerprint) :-
    with_clause_positions(Preds,
                          ( maplist(predicate_terms, Preds, Terms),
                            storable(Vars, PVars, _, []) )),
    current_prolog_flag(home, Home),
    exclude([S]>>sub_atom(S, 0, _, _, Home), Sources, OwnSources),
    variant_sha1(state(Terms, PVars, OwnSources, Flags), Fingerprint).

predicate_terms(pred(Key, Props, Refs), Key-Props-Clauses) :-
    foldl(stored_clause, Refs, Clauses, _, []).

stored_clause(Ref, Stored, Keys0, Keys) :-
    clause(Head, Body, Ref),
    portable(Head-Body, Stored, Keys0, Keys).

% A clause is at position I of the predicate at position P of Preds.
with_clause_positions(Preds, Goal) :-
    setup_call_cleanup(forall(( nth1(P, Preds, pred(_, _, Refs)), nth1(I, Refs, Ref) ),
                              assertz(clause_position(Ref, P, I))),
                       Goal,
                       retractall(clause_position(_, _, _))).

% A clause reference becomes '$clause_ref'(P, I) for clause I of predicate
% P, and the referenced predicates are collected in a difference list. Any
% other blob cannot be stored.
portable(Term, Portable, Keys0, Keys) :-
    (   var(Term)
    ->  Portable = Term, Keys0 = Keys
    ;   blob(Term, clause)
    ->  clause_position(Term, P, I),
        Portable = '$clause_ref'(P, I),
        Keys0 = [P|Keys]
    ;   blob(Term, Type), \+ memberchk(Type, [text, reserved_symbol])
    ->  fail
    ;   compound(Term)
    ->  compound_name_arguments(Term, Name, Args),
        foldl(portable, Args, PArgs, Keys0, Keys),
        compound_name_arguments(Portable, Name, PArgs)
    ;   Portable = Term, Keys0 = Keys
    ).

% Global variables and results may hold attributed variables, stored as
% their plain copy and the goals that restore the attributes.
storable(Term, Stored, Keys0, Keys) :-
    copy_term(Term, Copy, Goals),
    portable(Copy-Goals, Stored, Keys0, Keys).

%%%%%%%%%% Effect of a load %%%%%%%%%%

% effect(prelude(Keys, Sources, Flags), Predicates,
%        ending(GlobalVariables, Deleted, Results)).
% Keys is keys(Key1, ..., KeyN) over the predicates after the load, and a
% stored clause reference '$clause_ref'(P, I) names clause I of predicate P.
% Predicates holds changed(P, Props, From, Refers, Referenced, Clauses): the
% first From clauses of P are kept from the starting state and Clauses follow
% them. Refers is refers when the clauses hold clause references, Referenced
% is referenced when stored terms reference clauses of P, and every predicate
% comes after the changed predicates its clauses reference.
state_effect(state(Preds0, Vars0, Sources0, Flags0),
             state(Preds1, Vars1, Sources1, Flags1), Results, Effect) :-
    findall(P-Change, ( nth1(P, Preds1, Pred), changed_predicate(Preds0, Pred, Change) ), Changes),
    \+ replaces_referenced(Changes, Preds0, Vars0),
    exclude(kept_variable(Vars0), Vars1, SetVars),
    with_clause_positions(Preds1,
                          ( maplist(stored_change, Changes, Changed0),
                            storable(SetVars, Vars, VarDeps, []),
                            storable(Results, PResults, ResultDeps, []) )),
    findall(Dep, ( member(_-Deps, Changed0), member(Dep, Deps) ), ClauseDeps),
    append([ClauseDeps, VarDeps, ResultDeps], Referenced0),
    sort(Referenced0, Referenced),
    maplist(mark_referenced(Referenced), Changed0, Changed1),
    dependency_order(Changed1, Changed),
    findall(Key, member(pred(Key, _, _), Preds1), KeyList),
    Keys =.. [keys|KeyList],
    findall(K, ( member(K-_, Vars0), \+ memberchk(K-_, Vars1) ), Deleted),
    findall(load(S, M, Options), ( member(S, Sources1), \+ memberchk(S, Sources0),
                                   source_file_property(S, load_context(M, _, Options)) ),
            Sources),
    subtract(Flags1, Flags0, Flags),
    Effect = effect(prelude(Keys, Sources, Flags), Changed, ending(Vars, Deleted, PResults)).

kept_variable(Vars0, K-V) :- memberchk(K-V0, Vars0), V0 =@= V.

changed_predicate(Preds0, pred(Key, Props, Refs), change(Key, Props, From, New)) :-
    (   memberchk(pred(Key, _, Refs0), Preds0)
    ->  Refs \== Refs0,
        (   append(Refs0, New, Refs)
        ->  length(Refs0, From)
        ;   From = 0, New = Refs
        )
    ;   From = 0, New = Refs
    ).

% Replaying a predicate whose clauses were not only appended to rewrites all
% of them, which source files and clause references kept from the starting
% state could not follow.
replaces_referenced(Changes, Preds0, Vars0) :-
    member(_-change(Key, _, 0, Refs), Changes),
    (   member(Ref, Refs), clause_property(Ref, file(_))
    ;   memberchk(pred(Key, _, Refs0), Preds0),
        Refs0 \== [],
        (   member(pred(_, _, Kept), Preds0), member(Ref, Kept), clause(H, B, Ref),
            Term = H-B
        ;   member(_-Term, Vars0)
        ),
        sub_term(Sub, Term), blob(Sub, clause), memberchk(Sub, Refs0)
    ), !.

stored_change(P-change(Key, Props, From, Refs), changed(P, Key-Props, From, Refers, _, Clauses)-Deps) :-
    foldl(stored_clause, Refs, Clauses, Deps0, []),
    sort(Deps0, Deps),
    (   Deps == []
    ->  Refers = plain
    ;   Refers = refers
    ).

mark_referenced(Referenced, Change-Deps, Change-Deps) :-
    Change = changed(P, _, _, _, Mark, _),
    (   ord_memberchk(P, Referenced)
    ->  Mark = referenced
    ;   Mark = unreferenced
    ).

% Plain predicates reference nothing and come first; the few that hold clause
% references are then ordered among themselves.
dependency_order(Changes, Ordered) :-
    partition([changed(_, _, _, Refers, _, _)-_]>>(Refers == plain), Changes, Plain0, Referring),
    pairs_keys(Plain0, Plain),
    order_referring(Referring, Ordered0),
    append(Plain, Ordered0, Ordered).

order_referring([], []) :- !.
order_referring(Pending, [Change|Ordered]) :-
    select(Change-Deps, Pending, Rest),
    Change = changed(P, _, _, _, _, _),
    \+ ( member(Dep, Deps), Dep \== P, memberchk(changed(Dep, _, _, _, _, _)-_, Rest) ), !,
    order_referring(Rest, Ordered).

%%%%%%%%%% Inputs %%%%%%%%%%

% inputs(Files, Directories): every MeTTa and Prolog file the load read with
% its digest, and the MeTTa and Prolog names in each directory it read MeTTa
% files from.
load_inputs(Path, state(_, _, Sources0, _), state(_, _, Sources1, _), inputs(Files, Dirs)) :-
    findall(F, user:metta_source_functions_started(F), Metta),
    findall(S, ( member(S, Sources1), \+ memberchk(S, Sources0) ), Prolog),
    append([[Path], Metta, Prolog], Files0),
    sort(Files0, FileList),
    maplist(file_digest, FileList, Files),
    findall(D, ( member(F, [Path|Metta]), file_directory_name(F, D) ), Dirs0),
    sort(Dirs0, DirList),
    maplist(directory_listing, DirList, Dirs).

file_digest(File, File-Digest) :-
    read_file_to_codes(File, Codes, [type(binary)]),
    sha_hash(Codes, Hash, [algorithm(sha256)]),
    hash_atom(Hash, Digest).

directory_listing(Dir, Dir-Names) :-
    directory_files(Dir, Entries),
    include(source_name, Entries, Names0),
    sort(Names0, Names).

source_name(Name) :- file_name_extension(_, Ext, Name), memberchk(Ext, [metta, pl]).

inputs_unchanged(inputs(Files, Dirs)) :-
    forall(member(File-Digest, Files),
           ( exists_file(File), file_digest(File, File-Digest) )),
    forall(member(Dir-Names, Dirs),
           ( exists_directory(Dir), directory_listing(Dir, Dir-Names) )).

%%%%%%%%%% Snapshot files %%%%%%%%%%

% One snapshot file per runtime, loaded file, space and command line, so
% that a changed input replaces its snapshot instead of adding one.
snapshot_file(Root, Path, Space, File) :-
    current_prolog_flag(argv, Argv),
    variant_sha1(snapshot(Root, Path, Space, Argv), Key),
    cache_directory(Dir),
    file_name_extension(Key, snapshot, Name),
    directory_file_path(Dir, Name, File).

cache_directory(Dir) :-
    (   getenv('PETTA_LOAD_CACHE', Dir)
    ->  true
    ;   getenv('XDG_CACHE_HOME', Base)
    ->  directory_file_path(Base, 'petta/loads', Dir)
    ;   expand_file_name('~/.cache/petta/loads', [Dir])
    ).

% The runtime tree this module was loaded from.
runtime_root(Root) :-
    module_property(load_cache, file(Self)),
    file_directory_name(Self, Src),
    file_directory_name(Src, Root).

runtime_fingerprint(Root, Fingerprint) :-
    findall(F, ( member(Sub, [src, lib, python]),
                 directory_file_path(Root, Sub, Dir),
                 exists_directory(Dir),
                 directory_member(Dir, F, [recursive(true), extensions([pl, metta, py])]) ), Files0),
    sort(Files0, Files),
    maplist(file_digest, Files, Digests),
    variant_sha1(Digests, Fingerprint).

% The changed predicates are separate terms, so a replay holds one of them
% in memory at a time.
write_snapshot(File, Header, Inputs, effect(Prelude, Changed, Ending)) :-
    file_directory_name(File, Dir),
    make_directory_path(Dir),
    current_prolog_flag(pid, Pid),
    format(atom(Tmp), '~w.~w.tmp', [File, Pid]),
    setup_call_cleanup(open(Tmp, write, Out, [type(binary)]),
                       ( maplist(fast_write(Out), [Header, Inputs, Prelude|Changed]),
                         fast_write(Out, Ending) ),
                       close(Out)),
    rename_file(Tmp, File).

replay_snapshot(File, Header, Results) :-
    exists_file(File),
    setup_call_cleanup(open(File, read, In, [type(binary)]),
                       ( fast_read(In, Header),
                         fast_read(In, Inputs),
                         inputs_unchanged(Inputs),
                         replay_effect(File, In, Results) ),
                       close(In)).

% Once a replay has started, failing would leave a partial load behind.
replay_effect(File, In, Results) :-
    (   fast_read(In, Prelude),
        apply_prelude(Prelude, Keys, Table),
        fast_read(In, Next),
        apply_rest(Next, In, Keys, Table, Results)
    ->  true
    ;   throw(error(load_cache_replay_failed(File), _))
    ).

%%%%%%%%%% Replay %%%%%%%%%%

% Table holds, at the position of each predicate replayed so far, the term
% refs(Ref1, ..., RefN) of its clause references.
apply_prelude(prelude(Keys, Sources, Flags), Keys, Table) :-
    forall(member(load(S, M, Options), Sources), load_files(M:S, Options)),
    forall(member(N-V, Flags), set_prolog_flag(N, V)),
    functor(Keys, _, NKeys),
    functor(Table, table, NKeys).

apply_rest(Change, In, Keys, Table, Results) :-
    Change = changed(_, _, _, _, _, _), !,
    apply_change(Keys, Table, Change),
    fast_read(In, Next),
    apply_rest(Next, In, Keys, Table, Results).
apply_rest(ending(Vars, Deleted, PResults), _, Keys, Table, Results) :-
    restored(Keys, Table, Vars, VarList),
    forall(member(K-V, VarList), nb_setval(K, V)),
    forall(member(K, Deleted), nb_delete(K)),
    restored(Keys, Table, PResults, Results).

apply_change(Keys, Table, changed(P, (M:F/A)-Props, From, Refers, Referenced, Clauses)) :-
    functor(H, F, A),
    (   predicate_property(M:H, dynamic)
    ->  true
    ;   dynamic(M:F/A),
        forall(member(Prop, Props), declare(Prop, M:F/A))
    ),
    forall(( nth_clause(M:H, I, R), I > From ), erase(R)),
    maplist(assert_stored(Refers, Keys, Table, M), Clauses),
    (   Referenced == referenced
    ->  findall(R, clause(M:H, _, R), RefList),
        Refs =.. [refs|RefList],
        setarg(P, Table, Refs)
    ;   true
    ).

declare(multifile, P) :- multifile(P).
declare(discontiguous, P) :- discontiguous(P).
declare(thread_local, P) :- thread_local(P).

assert_stored(plain, _, _, M, Head-Body) :-
    assertz(M:(Head :- Body)).
assert_stored(refers, Keys, Table, M, Stored) :-
    bind_refs(Keys, Table, Stored, Head-Body),
    assertz(M:(Head :- Body)).

bind_refs(Keys, Table, Stored, Term) :-
    (   var(Stored)
    ->  Term = Stored
    ;   Stored = '$clause_ref'(P, I)
    ->  bound_ref(Keys, Table, P, I, Term)
    ;   compound(Stored)
    ->  compound_name_arguments(Stored, Name, Args),
        maplist(bind_refs(Keys, Table), Args, BArgs),
        compound_name_arguments(Term, Name, BArgs)
    ;   Term = Stored
    ).

restored(Keys, Table, Stored, Term) :-
    bind_refs(Keys, Table, Stored, Term-Goals),
    maplist(call, Goals).

% A predicate the load left unchanged keeps its clauses from the starting
% state.
bound_ref(Keys, Table, P, I, Ref) :-
    arg(P, Table, Refs),
    (   nonvar(Refs)
    ->  arg(I, Refs, Ref)
    ;   arg(P, Keys, M:F/A),
        functor(H, F, A),
        findall(R, clause(M:H, _, R), Rs),
        nth1(I, Rs, Ref)
    ).
