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

% The text of each tricky term, as the code-list writer printed it: the
% string-stream writer must reproduce it byte for byte.
test(tricky_terms_text, Texts == Expected) :-
        Terms = [ a, 'a b', 'True', '$x', 'héllo', '✓', [], '[]', '{}', '', 'x"y',
                  "", "say \"hi\"", "back\\slash", "\\\"", "new\nline\ttab", "héllo ✓ 😀",
                  0, -7, 123456789012345678901234567890, 1.0, -1.5, 0.1, -0.0,
                  1.0e10, 1.0e-10, 1.0Inf, -1.0Inf, 1.5NaN,
                  [a, [b, [c, []]], "s", 1.5], [x|_], [a, b|c], [a|"s"],
                  f(a, b), g([1, 2], "q"), [f(x, Y), Y], [X, X, Z, [Z, W], W],
                  [[]], [[], [[]]], ['(', ')', ' '] ],
        Expected = [ "a", "a b", "True", "$x", "héllo", "✓", "()", "[]", "{}", "", "x\"y",
                     "\"\"", "\"say \\\"hi\\\"\"", "\"back\\\\slash\"", "\"\\\\\\\"\"",
                     "\"new\nline\ttab\"", "\"héllo ✓ 😀\"",
                     "0", "-7", "123456789012345678901234567890", "1.0", "-1.5", "0.1", "-0.0",
                     "1.0e+10", "1.0e-10", "1.0Inf", "-1.0Inf", "1.5NaN",
                     "(a (b (c ())) \"s\" 1.5)", "(cons x $_0)", "(cons a (cons b c))", "(cons a \"s\")",
                     "(f a b)", "(g (1 2) \"q\")", "((f x $_0) $_0)", "($_0 $_0 $_1 ($_1 $_2) $_2)",
                     "(())", "(() (()))", "(( )  )" ],
        maplist(swrite, Terms, Texts).

% Printing commits: a result printer that left a choice point per printed list
% kept every frame below it alive until the answers were consumed.
test(leaves_no_choice_point) :-
        Term = [a, [b, c], [d], "s", X, f(X), [e|_]],
        call_cleanup(swrite(Term, _), Det = true),
        Det == true.

:- end_tests(parser_swrite).
