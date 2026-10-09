:- begin_tests(parser_swrite).

:- ensure_loaded('../src/parser.pl').

test(repeated_variable_round_trip) :-
        once(swrite([pair, X, X], Text)),
        once(sread(Text, Parsed)),
        Parsed = [pair, A, B],
        A == B.

test(distinct_variables_round_trip) :-
        once(swrite([pair, _X, _Y], Text)),
        once(sread(Text, Parsed)),
        Parsed = [pair, A, B],
        A \== B.

% Counter-minted names do not depend on stack addresses, so the printed
% text is identical before and after a collection.
test(text_stable_across_gc) :-
        Term = [pair, X, X],
        once(swrite(Term, Before)),
        garbage_collect,
        once(swrite(Term, After)),
        Before == After.

% Many distinct variables must stay distinct after a round trip: if any
% two printed under one name, sread would merge them.
test(many_distinct_variables_stay_distinct) :-
        length(Vs, 4000),
        once(swrite([vs|Vs], Text)),
        once(sread(Text, Parsed)),
        Parsed = [vs|Ps],
        sort(Ps, Sorted),
        length(Ps, N), length(Sorted, N).

% The naming attribute must not survive the call.
test(no_attribute_residue) :-
        once(swrite([pair, X, X], _)),
        \+ attvar(X).

test(unrelated_attribute_survives) :-
        freeze(X, throw(unexpected_wakeup)),
        frozen(X, Before),
        once(swrite([pair, X, X], _)),
        frozen(X, After),
        Before =@= After.

test(output_may_alias_input_variable) :-
        once(swrite([pair, X], X)),
        string(X),
        once(sread(X, Parsed)),
        Parsed = [pair, Y],
        var(Y).

:- end_tests(parser_swrite).
