:- use_module(library(readutil)). % read_file_to_string/3
:- use_module(library(pcre)). % re_replace/4

% Read Filename into string S and compile it to Prolog (S holds MeTTa code)
compile_metta_file(Filename, Output) :- compile_metta_file(Filename, Output, '&self').
compile_metta_file(Filename, Output, Space) :-
  read_file_to_string(Filename, S, []),
  compile_metta_string(S, Output, Space).

% Read Filename into string S and process it:
load_metta_file(Filename, Results) :- load_metta_file(Filename, Results, '&self').
load_metta_file(Filename, Results, Space) :-
  read_file_to_string(Filename, S, []),
  process_metta_string(S, Results, Space).

% Compile function definitions, invocations and S-expression part of &self space
compile_metta_string(S, Output) :- compile_metta_string(S, Output, '&self').
compile_metta_string(S, Output, Space) :-
  extract_forms(S, Forms),
  maplist(parse_form, Forms, ParsedForms),
  maplist(process_form(false, Space), ParsedForms, _, CompiledForms), !,
  Prologue = [
    ':- ensure_loaded(\'src/metta\').\n',
    ':- use_module(library(memfile)).\n',
    ':- new_memory_file(MF), open_memory_file(MF, write, ResultsStream), assertz(resultsMemFile(MF)), assertz(resultsStream(ResultsStream)).\n',
    'write_result(Result) :- resultsStream(ResultsStream), format(ResultsStream, "~w~n", [Result]).\n'],
  Epilogue = [
    ':- resultsStream(S), close(S), resultsMemFile(MF), memory_file_to_string(MF, ResultsOutput), write(ResultsOutput), free_memory_file(MF).\n'
  ],
  append([Prologue, CompiledForms, Epilogue], OutputsList),
  atomic_list_concat(OutputsList, Output).

%Extract function definitions, call invocations, and S-expressions part of &self space:
process_metta_string(S, Results) :- process_metta_string(S, Results, '&self').
process_metta_string(S, Results, Space) :-
  extract_forms(S, Forms),
  maplist(parse_form, Forms, ParsedForms),
  maplist(process_form(true, Space), ParsedForms, ResultsList, _), !,
  append(ResultsList, Results).

% Extract top forms from MeTTa string
extract_forms(S, Forms) :-
  string_codes(S, Cs),
  strip(Cs, 0, Codes),
  phrase(top_forms(Forms, 1), Codes).

%First pass to convert MeTTa to Prolog Terms and register functions:
parse_form(form(S), parsed(T, S, Term)) :- sread(S, Term),
                                           ( Term = [=, [F|W], _], atom(F) -> register_fun(F), length(W, N), Arity is N + 1, assertz(arity(F,Arity)), T=function
                                                                            ; T=expression ).
parse_form(runnable(S), parsed(runnable, S, Term)) :- sread(S, Term).

% Second pass to compile / run / add the Terms.
% Output will always contain the compilation output as a result.
% If Execute = true, the goals are run and return in Results.
% If Execute = false, no goals are run and Results = [].
process_form(Execute, Space, parsed(expression, _, Term), [], Output) :-
  ( silent(false) ->
      swrite(Term, STerm),
      format("\e[33m--> metta sexpr -->~n\e[36m~w~n", [STerm]),
      format("\e[33m^^^^^^^^^^^^^^^^^^^~n\e[0m")
    ; true),
  ( Execute ->
    'add-atom'(Space, Term, true)
    ; true),
  with_output_to(string(Output), portray_clause(:- 'add-atom'(Space, Term, true))).

process_form(Execute, _, parsed(runnable, FormStr, Term), Result, Output) :-
  translate_expr([collapse, Term], true, true, Goals, Result), 
  % Conditional goal execution
  ( Execute ->
      call_goals(Goals)
  ; true),
  % We generate all goals and print them to Output.
  findall(
    GoalOutput,
    (member(G, Goals), with_output_to(string(GoalOutput), portray_clause((:- G)))),
    GoalOutputs),
  
  atomic_list_concat(GoalOutputs, Output),
  % We must print to the console at the end, in case the goals were actually called.
  ( silent(false) ->
      format("\e[33m--> metta runnable  -->~n\e[36m!~w~n\e[33m-->  prolog goal  -->\e[35m ~n", [FormStr]),
      forall(member(G, Goals), portray_clause((:- G))),
      format("\e[33m^^^^^^^^^^^^^^^^^^^^^^^~n\e[0m")
  ; true).

process_form(Execute, Space, parsed(function, FormStr, Term), [], Output) :-
  translate_clause(Term, Clause),
  assertz(Clause, Ref),
  assertz(translated_from(Ref, Term)),
  clause(Head, Body, Ref),
  ( Body == true -> Show = Head; Show = (Head :- Body) ),
  with_output_to(string(Output), portray_clause(Show)),
  ( silent(false) ->
      format("\e[33m--> metta function -->~n\e[36m~w~n\e[33m--> prolog clause -->~n\e[32m", [FormStr]),
      write(current_output, Output),
      format("\e[33m^^^^^^^^^^^^^^^^^^^^^^~n\e[0m")
  ; true),
  ( Execute ->
      add_sexp(Space, Term)
  ; true
  ).

process_form(_, In, _, _) :- format(atom(Msg), "failed to process form: ~w", [In]), throw(error(syntax_error(Msg), none)).

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
strip([0';|R], 0, Out) :- !, append(_, [0'\n|Rest], R), strip(Rest, 0, Out).
strip([C|R], In, [C|O]) :- strip(R, In, O).
