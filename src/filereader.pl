:- use_module(library(readutil)). % read_file_to_string/3
:- use_module(library(pcre)). % re_replace/4
:- current_prolog_flag(argv, Args), ( (memberchk(silent, Args) ; memberchk('--silent', Args) ; memberchk('-s', Args))
                                      -> assertz(silent(true)) ; assertz(silent(false)) ).
:- dynamic working_dir/1.
:- dynamic translated_from/2.

push_working_dir(Filename) :- file_directory_name(Filename, Dir0),
                              ( absolute_file_name(Dir0, Dir, [file_type(directory), file_errors(fail)])
                                -> true
                                 ; Dir = Dir0 ),
                              asserta(working_dir(Dir)).

pop_working_dir :- retract(working_dir(_)), !.
pop_working_dir.

%Read Filename into string S and process it (S holds MeTTa code):
load_metta_file(Filename, Results) :- load_metta_file(Filename, Results, '&self').
load_metta_file(Filename, Results, Space) :- catch(load_metta_file_impl(Filename, Results, Space),
                                                   Error,
                                                   rethrow_metta_file_error(Filename, Error)).

load_metta_file_impl(Filename, Results, Space) :- setup_call_cleanup(push_working_dir(Filename),
                                                                     ( read_file_to_string(Filename, S, []),
                                                                       process_metta_string(S, Results, Space) ),
                                                                     pop_working_dir).

rethrow_metta_file_error(_, Error) :- Error = error(_, context(_, _)), !,
                                      throw(Error).
rethrow_metta_file_error(Filename, error(Type, _)) :- !,
                                                      throw(error(Type, context(Filename, 'while loading MeTTa file'))).
rethrow_metta_file_error(_, Error) :- throw(Error).

%Extract function definitions, call invocations, and S-expressions part of &self space:
process_metta_string(S, Results) :- process_metta_string(S, Results, '&self').
process_metta_string(S, Results, Space) :- string_codes(S, Cs),
                                           strip(Cs, 0, Codes),
                                           phrase(top_forms(Forms, 1), Codes),
                                           maplist(parse_form, Forms, ParsedForms),
                                           %Pinned git dependencies declared in this file are
                                           %fetched before any of its forms run (gitimport.pl):
                                           acquire_declared_dependencies(ParsedForms),
                                           maplist(process_form(Space), ParsedForms, ResultsList), !,
                                           append(ResultsList, Results).

%Arity must be known before register_fun/1, because a late registration recompiles
%stored definitions and the translator consults arity/2 while compiling their bodies:
register_function_signature(F, Arity) :- ( catch(arity(F, Arity), _, fail) -> true ; assertz(arity(F, Arity)) ),
                                         warn_if_executed_as_symbol(F),
                                         register_fun(F).

%An expression that already executed compiled F as plain data; that execution cannot
%be repaired retroactively, so flag it when F now arrives through a parsed definition:
warn_if_executed_as_symbol(F) :- \+ fun(F), symbol_head(F, runnable), !,
                                 format(user_error, "Warning: ~w is defined or imported after already being used; earlier expressions treat it as a plain symbol. Move the import or definition above the first use.~n", [F]).
warn_if_executed_as_symbol(_).

%A function arriving after its name was already compiled as plain data in stored
%definitions: recompile those definitions from their source terms, so import order
%cannot change what a definition means:
repair_after_late_registration(F) :- ( symbol_head(F, clause) -> repair_stale_definitions(F) ; true ).

repair_stale_definitions(F) :- findall(G, ( translated_from(_, [=, [G|_], Body]),
                                            atom(G),
                                            uses_as_data(F, Body) ), Gs0),
                               sort(Gs0, Gs),
                               forall(member(G, Gs), recompile_function(G)).

%Rebuild every clause of G from its stored source terms. Erasing and re-appending each
%tracked clause in assertion order keeps their relative order; clauses asserted through
%Prolog interop are not tracked and would end up before the rebuilt ones:
recompile_function(G) :- findall(Ref-Term, ( translated_from(Ref, Term),
                                             Term = [=, [G0|_], _],
                                             G0 == G ), Pairs),
                         forall(member(Ref-Term, Pairs),
                                ( erase(Ref),
                                  retract(translated_from(Ref, Term)),
                                  copy_term(Term, Fresh),
                                  once(translate_clause(Fresh, Clause)),
                                  assertz(Clause, NewRef),
                                  assertz(translated_from(NewRef, Term)) )),
                         invalidate_specializations(G).

%True if the term contains a call-shaped (list-head) occurrence of F:
uses_as_data(F, Term) :- nonvar(Term),
                         Term = [H|Args],
                         ( H == F -> true
                         ; uses_as_data(F, H) -> true
                         ; uses_as_data_args(F, Args) ).
uses_as_data_args(F, Args) :- nonvar(Args),
                              Args = [A|Rest],
                              ( uses_as_data(F, A) -> true ; uses_as_data_args(F, Rest) ).

%First pass to convert MeTTa to Prolog Terms and register functions:
parse_form(form(S), parsed(T, S, Term)) :- sread(S, Term),
                                           ( Term = [=, [F|W], _], atom(F) -> length(W, N),
                                                                              Arity is N + 1,
                                                                              register_function_signature(F, Arity),
                                                                              T=function
                                                                            ; T=expression ).
parse_form(runnable(S), parsed(runnable, S, Term)) :- sread(S, Term).

%Second pass to compile / run / add the Terms:
process_form(Space, parsed(expression, _, Term), []) :- 'add-atom'(Space, Term, true),
                                                        ( silent(true) -> true ; swrite(Term,STerm),
                                                                                 format("\e[33m--> metta sexpr -->~n\e[36m~w~n", [STerm]),
                                                                                 format("\e[33m^^^^^^^^^^^^^^^^^^^~n\e[0m") ).
process_form(_, parsed(runnable, FormStr, Term), Result) :- translate_runnable_expr([collapse, Term], Goals, Result),
                                                            ( silent(true) -> true ; format("\e[33m--> metta runnable  -->~n\e[36m!~w~n\e[33m-->  prolog goal  -->\e[35m ~n", [FormStr]),
                                                                                     forall(member(G, Goals), portray_clause((:- G))),
                                                                                     format("\e[33m^^^^^^^^^^^^^^^^^^^^^^^~n\e[0m") ),
                                                            call_goals(Goals).
process_form(Space, parsed(function, FormStr, Term), []) :- add_sexp(Space, Term),
                                                            translate_clause(Term, Clause),
                                                            assertz(Clause, Ref),
                                                            assertz(translated_from(Ref, Term)),
                                                            ( silent(true) -> true ; format("\e[33m--> metta function -->~n\e[36m~w~n\e[33m--> prolog clause -->~n\e[32m", [FormStr]),
                                                                                     clause(Head, Body, Ref),
                                                                                     ( Body == true -> Show = Head; Show = (Head :- Body) ),
                                                                                     portray_clause(current_output, Show),
                                                                                     format("\e[33m^^^^^^^^^^^^^^^^^^^^^^~n\e[0m") ).
process_form(_, In, _) :- format(atom(Msg), "failed to process form: ~w", [In]), throw(error(syntax_error(Msg), none)).

%Like blanks but counts newlines:
newlines(C0, C2) --> blanks_to_nl, !, {C1 is C0+1}, newlines(C1,C2).
newlines(C, C) --> blanks.

%Collect characters until all parentheses are balanced (depth 0), accumulating codes, and also counting newlines:
grab_until_balanced(D, Acc, Cs, LC0, LC2, InS) --> [C], { ( C=0'" -> InS1 is 1-InS ; InS1 = InS ),
                                                                     ( InS = 0 -> ( C=0'( -> D1 is D+1
                                                                                           ; C=0') -> D1 is D-1
                                                                                                    ; D1 = D )
                                                                                ; D1 = D ),
                                                                     Acc1=[C|Acc],
                                                                     ( C=10 -> LC1 is LC0+1 ; LC1 = LC0 ) },
                                                          ( { D1=:=0, InS1=0 } -> { reverse(Acc1,Cs) , LC2 = LC1 }
                                                                                ; grab_until_balanced(D1,Acc1,Cs,LC1,LC2,InS1) ).

%Read a balanced (...) block if available, turn into string, then continue with rest, ignoring comments:
top_forms([],_) --> blanks, eos.
top_forms([Term|Fs], LC0) --> newlines(LC0, LC1),
                              ( "!" -> {Tag = runnable} ; {Tag = form} ),
                              ( "(" -> [] ; string_without("\n", Rest), { format(atom(Msg), "expected '(' or '!(', line ~w:~n~s", [LC1, Rest]), throw(error(syntax_error(Msg), none)) } ),
                              ( grab_until_balanced(1, [0'(], Cs, LC1, LC2, 0)
                                -> { true } ; string_without("\n", Rest), { format(atom(Msg), "missing ')', starting at line ~w:~n~s", [LC1, Rest]), throw(error(syntax_error(Msg), none)) } ),
                              { string_codes(FormStr, Cs), Term =.. [Tag, FormStr] },
                              top_forms(Fs, LC2).

%Strip off code that is commented out, while tracking when inside of string:
strip([], _, []).
strip([0'"|R], 0, [0'"|O]) :- !, strip(R, 1, O).
strip([0'"|R], 1, [0'"|O]) :- !, strip(R, 0, O).
strip([0'\n|R], In, [0'\n|O]) :- !, strip(R, In, O).
strip([0';|R], 0, Out) :- !, (append(_, [0'\n|Rest], R) -> strip(Rest, 0, Out) ; Out = []).
strip([C|R], In, [C|O]) :- strip(R, In, O).
