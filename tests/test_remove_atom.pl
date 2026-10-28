:- ensure_loaded('../src/metta.pl').

:- begin_tests(remove_atom).

test(removing_one_duplicate_leaves_one_copy) :-
    Space = '&issue251_duplicate_test',
    Atom = [likes, alice, bob],
    'add-atom'(Space, Atom, true),
    'add-atom'(Space, Atom, true),
    'remove-atom'(Space, Atom, true),
    findall(X, '&issue251_duplicate_test'(likes, X, bob), Remaining),
    assertion(Remaining == [alice]),
    'remove-atom'(Space, Atom, true),
    findall(X, '&issue251_duplicate_test'(likes, X, bob), Empty),
    assertion(Empty == []).

test(removing_function_preserves_identical_definition_in_other_space) :-
    SpaceA = '&issue251_identical_a',
    SpaceB = '&issue251_identical_b',
    TermA = [=, [issue251_identical_fn, _], 42],
    TermB = [=, [issue251_identical_fn, _], 42],
    'add-atom'(SpaceA, TermA, true),
    'add-atom'(SpaceB, TermB, true),
    findall(R, issue251_identical_fn(input, R), Before),
    assertion(Before == [42, 42]),
    'remove-atom'(SpaceA, TermA, _),
    findall(R, issue251_identical_fn(input, R), After),
    assertion(After == [42]),
    findall(Stored,
            '&issue251_identical_b'('=', [issue251_identical_fn, _], Stored),
            RemainingDefinitions),
    assertion(RemainingDefinitions == [42]).

test(removing_function_preserves_different_definition_in_other_space) :-
    SpaceA = '&issue251_different_a',
    SpaceB = '&issue251_different_b',
    TermA = [=, [issue251_different_fn, _], 42],
    TermB = [=, [issue251_different_fn, _], 99],
    'add-atom'(SpaceA, TermA, true),
    'add-atom'(SpaceB, TermB, true),
    'remove-atom'(SpaceA, TermA, _),
    findall(R, issue251_different_fn(input, R), Remaining),
    assertion(Remaining == [99]),
    findall(Stored,
            '&issue251_different_b'('=', [issue251_different_fn, _], Stored),
            RemainingDefinitions),
    assertion(RemainingDefinitions == [99]).

test(removing_last_definition_makes_function_unavailable) :-
    Space = '&issue251_last_test',
    Term = [=, [issue251_last_fn, _], 42],
    'add-atom'(Space, Term, true),
    findall(R, issue251_last_fn(input, R), Before),
    assertion(Before == [42]),
    'remove-atom'(Space, Term, Removed),
    assertion(Removed == true),
    findall(R, issue251_last_fn(input, R), After),
    assertion(After == []).

:- end_tests(remove_atom).
