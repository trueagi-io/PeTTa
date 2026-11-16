:- ensure_loaded('../src/metta').
:- use_module(library(plunit)).
:- use_module(library(aggregate)).

:- begin_tests(new_space).

allocate_unused(N) :-
    forall(between(1,N,_), user:'new-space'(_)).

handle_atom_count(N) :-
    aggregate_all(count,
                  (current_atom(A), sub_atom(A,0,7,_,'&space(')), N).

held_handle_count(N, Count) :-
    findall(S, (between(1,N,_),user:'new-space'(S)), Handles),
    handle_atom_count(Count),
    length(Handles, N).

test(gensym_reset_does_not_reuse_a_handle) :-
    user:'new-space'(Before),
    reset_gensym,
    reset_gensym('&space('),
    user:'new-space'(After),
    assertion(Before \== After).

test(unused_handles_are_reclaimed) :-
    allocate_unused(100), garbage_collect, garbage_collect_atoms,
    handle_atom_count(Before),
    held_handle_count(5000, Peak),
    garbage_collect, garbage_collect_atoms,
    handle_atom_count(After),
    assertion(Peak >= 5000),
    assertion(After =< Before+2).

test(store_survives_atom_gc) :-
    user:'new-space'(Space),
    user:add_sexp(Space, [kept,42]),
    garbage_collect, garbage_collect_atoms, garbage_collect_clauses,
    findall(X, user:match(Space,[kept,X],X,X), Rows),
    assertion(Rows == [42]).

test(handle_text_has_no_literal_spelling) :-
    user:'new-space'(Space),
    user:swrite([wrap,Space,end], Text),
    assertion(\+ sub_string(Text,_,_,_,"&space ")),
    assertion(sub_string(Text,0,_,_,"(wrap &space(")),
    atom_codes(Space, Codes),
    assertion(\+ phrase(user:atom_symbol(_), Codes)).

test(import_only_space_is_not_reused) :-
    flag('$metta_space_id', Previous, Previous),
    Next is Previous+1,
    format(atom(Imported), '&space(~d)', [Next]),
    source_file(plunit_new_space:handle_atom_count(_), Self),
    file_directory_name(Self, Dir),
    directory_file_path(Dir, 'fixtures/new_space/empty.metta', File),
    setup_call_cleanup(
        user:'import!'(Imported, File, true),
        ( assertion(\+ current_predicate(Imported/_)),
          assertion(\+ user:space_primitive(Imported,_)),
          assertion(user:metta_import_state(Imported,_,loaded)),
          user:'new-space'(Fresh),
          assertion(Fresh \== Imported)
        ),
        retractall(user:metta_import_state(Imported,_,_))).

:- end_tests(new_space).
