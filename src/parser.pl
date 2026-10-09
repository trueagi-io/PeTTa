:- use_module(library(dcg/basics)). %blanks/0, number/1, string_without/2

%Generate a MeTTa S-expression string from the Prolog list (inverse parsing):
%Variable names are minted from a per-call counter in first-occurrence order and
%cached on the variable itself (an attribute travels with the variable through
%GC, unlike SWI's internal display name, and lookup is O(1)).  Counter names
%make the two printing invariants hold by construction: one variable never
%prints under two names, and two variables never print under one.
swrite(Term, String) :-
        setup_call_cleanup(true,
            ( phrase(swrite_exp(Term, 0, _), Codes),
              string_codes(String, Codes) ),
            swrite_cleanup(Term)).

swrite_cleanup(Term) :-
        term_attvars(Term, Vars),
        maplist([V]>>del_attr(V, petta_swrite_name), Vars).

%The attribute carries serializer-local identity only.  A caller may alias the
%output with an input variable, so unification may reach this hook; it imposes
%no constraint on the value.
petta_swrite_name:attr_unify_hook(_, _).

swrite_exp(Var, C0, C)   --> { var(Var) }, !, "$_",
                              { (   get_attr(Var, petta_swrite_name, N)
                                ->  C = C0
                                ;   N = C0, C is C0 + 1,
                                    put_attr(Var, petta_swrite_name, N)
                                ),
                                number_codes(N, Cs) }, Cs.
swrite_exp(Num, C, C)    --> { number(Num) }, !, { number_codes(Num, Cs) }, Cs.
swrite_exp(Str, C, C)    --> { string(Str) }, !, "\"",
                              { string_codes(Str, Cs), escape_quotes(Cs, Es) }, Es, "\"".
swrite_exp(Atom, C, C)   --> { atom(Atom) }, !, atom(Atom).
swrite_exp([H|T], C0, C) --> { \+ is_list([H|T]) }, !, "(", atom(cons), " ",
                              swrite_exp(H, C0, C1), " ", swrite_exp(T, C1, C), ")".
swrite_exp([H|T], C0, C) --> !, "(", swrite_seq([H|T], C0, C), ")".
swrite_exp([], C, C)     --> !, "()".
swrite_exp(Term, C0, C)  --> { Term =.. [F|Args] }, "(", atom(F),
                              ( { Args == [] } -> { C = C0 }
                              ; " ", swrite_seq(Args, C0, C) ), ")".
swrite_seq([X], C0, C)    --> swrite_exp(X, C0, C).
swrite_seq([X|Xs], C0, C) --> swrite_exp(X, C0, C1), " ", swrite_seq(Xs, C1, C).
escape_quotes([], []).
escape_quotes([0'\\|T], [0'\\,0'\\|R]) :- !, escape_quotes(T, R).
escape_quotes([0'"|T], [0'\\,0'"|R]) :- !, escape_quotes(T, R).
escape_quotes([H|T], [H|R]) :- escape_quotes(T, R).

%Read S string or atom, extract codes, and apply DCG (parsing):
sread(S, T) :- ( atom_string(A, S),
                 atom_codes(A, Cs),
                 phrase(sexpr(T, [], _), Cs)
               -> true ; format(atom(Msg), 'Parse error in form: ~w', [S]), throw(error(syntax_error(Msg), none)) ).

%An S-Expression is a parentheses-nesting of S-Expressions that are either numbers, variables, sttrings, or atoms:
sexpr(S,E,E)  --> blanks, string_lit(S), blanks, !.
sexpr(T,E0,E) --> blanks, "(", blanks, seq(T,E0,E), blanks, ")", blanks, !.
sexpr(N,E,E)  --> blanks, number(N), ( lookahead_any(" ()\t\n\r") ; \+ [_] ), blanks, !.
sexpr(V,E0,E) --> blanks, var_symbol(V,E0,E), blanks, !.
sexpr(A,E,E)  --> blanks, atom_symbol(A), blanks.

%Helper for strange atoms that aren't numbers, e.g. 1_2_3:
lookahead_any(Terms, S, E) :- string_codes(Terms,SC), S = [Head | _], member(Head,SC), !, S = E.

%Recursive processing of S-Expressions within S-Expressions:
seq([X|Xs],E0,E2) --> sexpr(X,E0,E1), blanks, seq(Xs,E1,E2).
seq([],E,E)       --> [].

%Variables start with $, and keep track of them: re-using exising Prolog variables for variables of same name:
var_symbol(V,E0,E) --> "$", token(Cs), { atom_chars(N, Cs), ( N == '_' -> V = _, E = E0 ; memberchk(N-V0, E0) -> V = V0, E = E0 ; V = _, E = [N-V|E0] ) }.

%Atoms are derived from tokens:
atom_symbol(A) --> token(Cs), { string_codes("\"", [Q]), ( Cs = [Q|_] -> append([Q|Body], [Q], Cs), %"str" as string
                                                                         string_codes(A, Body)
                                                                       ; atom_codes(R, Cs),         %others are atoms
                                                                         ( R = 'True' -> A = true
                                                                                       ; R = 'False'
                                                                                         -> A = false
                                                                                          ; A = R ))}.

%A token is a non-empty string without whitespace:
token(Cs) --> string_without(" \t\r\n()", Cs), { Cs \= [] }.

%Just string literal handling from here-on:
string_lit(S) --> "\"", string_chars(Cs), "\"", { string_codes(S, Cs) }.
string_chars([]) --> [].
string_chars([C|Cs]) --> [C], { C =\= 0'", C =\= 0'\\ }, !, string_chars(Cs).
string_chars([C|Cs]) --> "\\", [X], { (X=0'n->C=10; X=0't->C=9; X=0'r->C=13; C=X) }, string_chars(Cs).
