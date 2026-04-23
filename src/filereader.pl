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
  % Clean slate for specializations and translated_from facts
  retractall(ho_specialization(_, _)),
  retractall(translated_from(_, _)),
  % Process forms and collect all goals (both functions and runnables) in order
  maplist(process_form_for_compile(Space), ParsedForms, AllGoals), !,
  % Now collect any specialized functions that were generated during compilation
  findall(SpecGoals, collect_specialization_goals(SpecGoals), SpecializationGoalsList),
  % Flatten and combine specialization goals with main goals
  append(SpecializationGoalsList, SpecGoals),
  append([SpecGoals, AllGoals], CombinedGoals),
  % Build the main/0 predicate with all goals
  build_main_predicate(CombinedGoals, MainClause),
  Prologue = [
    ':- working_directory(CWD, CWD), string_concat(CWD_NOSLASH, "/", CWD), assertz(working_dir(CWD_NOSLASH)).\n',
    ':- getenv("PETTA_HOME", PETTA_HOME) -> assertz(petta_home(PETTA_HOME)) ; assertz(petta_home("")).\n',
    ':- assertz(silent(true)).\n',
    ':- petta_home(PETTA_HOME), string_concat(PETTA_HOME, "/src/metta", METTA_LIB), ensure_loaded(METTA_LIB).\n',
    ':- dynamic translated_from/2.\n\n'
  ],
  Initialization = [
    '\n:- initialization(main).\n'
  ],
  append([Prologue, [MainClause], Initialization], OutputsList),
  atomic_list_concat(OutputsList, Output).

%Extract function definitions, call invocations, and S-expressions part of &self space:
process_metta_string(S, Results) :- process_metta_string(S, Results, '&self').
process_metta_string(S, Results, Space) :-
  extract_forms(S, Forms),
  maplist(parse_form, Forms, ParsedForms),
  maplist(process_form(Space), ParsedForms, ResultsList, _), !,
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

% Process form for compilation: return goals to be placed in main/0
process_form_for_compile(Space, parsed(expression, _, Term), Goals) :-
  Goals = ['add-atom'(Space, Term, true)].

process_form_for_compile(_, parsed(runnable, FormStr, Term), Goals) :-
  translate_expr([collapse, Term], false, TranslatedGoals, Result),
  % Convert goals list to conjunction
  list_to_conjunction(TranslatedGoals, GoalsConj),
  % Wrap in a goal that collects results and appends to accumulator
  % We flatten the results to avoid nested lists
  Goals = [findall(Result, GoalsConj, ResultsList),
           flatten(ResultsList, FlatResults),
           nb_getval(all_results, PrevResults),
           append(PrevResults, FlatResults, NewResults),
           nb_setval(all_results, NewResults)],
  debug_print_goals(TranslatedGoals, FormStr).

process_form_for_compile(Space, parsed(function, FormStr, Term), Goals) :-
  Goals = ['add-atom'(Space, Term, _)],
  ( silent(false) ->
      translate_clause(Term, Clause),
      clause(Clause, Body),
      ( Body == true -> Show = Clause; Show = (Clause :- Body) ),
      format("\e[33m--> metta function -->~n\e[36m~w~n\e[33m--> prolog clause -->~n\e[32m", [FormStr]),
      portray_clause(Show),
      format("\e[33m^^^^^^^^^^^^^^^^^^^^^^~n\e[0m")
  ; true).

% Second pass to run / add the Terms.
% Output will contain the compilation output as a result.
% The goals are run and return in Results.
process_form(Space, parsed(expression, _, Term), [], Output) :-
  ( silent(false) ->
      swrite(Term, STerm),
      format("\e[33m--> metta sexpr -->~n\e[36m~w~n", [STerm]),
      format("\e[33m^^^^^^^^^^^^^^^^^^^~n\e[0m")
    ; true),
  'add-atom'(Space, Term, true),
  with_output_to(string(Output), portray_clause(:- 'add-atom'(Space, Term, true))).

% We need to call the goals. We also don't need to include print
% statements in the translations, as we will print the Result later on.
process_form(_, parsed(runnable, FormStr, Term), Result, Output) :-
  translate_expr([collapse, Term], true, Goals, Result), 
  call_goals(Goals),
  write_to_output(Goals, Output),
  debug_print_goals(Goals, FormStr).

process_form(Space, parsed(function, FormStr, Term), [], Output) :-
  translate_clause(Term, Clause),
  assertz(Clause, Ref),
  assertz(translated_from(Ref, Term)),
  clause(Head, Body, Ref),
  % Just output the clause statically
  ( Body == true -> Show = Head; Show = (Head :- Body) ),
  with_output_to(string(Output), portray_clause(Show)),
  ( silent(false) ->
      format("\e[33m--> metta function -->~n\e[36m~w~n\e[33m--> prolog clause -->~n\e[32m", [FormStr]),
      write(current_output, Output),
      format("\e[33m^^^^^^^^^^^^^^^^^^^^^^~n\e[0m")
  ; true),
  add_sexp(Space, Term).

process_form(_, In, _, _) :- 
  format(atom(Msg), "failed to process form: ~w", [In]), 
  throw(error(syntax_error(Msg), none)).

write_to_output(Goals, Output) :-
  findall(
    GoalOutput,
    (member(G, Goals), with_output_to(string(GoalOutput), portray_clause((:- G)))),
    GoalOutputs),
  atomic_list_concat(GoalOutputs, Output).

debug_print_goals(Goals, FormStr) :-
  (silent(false) -> 
    format("\e[33m--> metta runnable  -->~n\e[36m!~w~n\e[33m-->  prolog goal  -->\e[35m ~n", [FormStr]),
    forall(member(G, Goals), portray_clause((:- G))),
    format("\e[33m^^^^^^^^^^^^^^^^^^^^^^^~n\e[0m")
  ; true).

% Build the main/0 predicate from all goals
build_main_predicate(GoalsList, MainClause) :-
  % Flatten the list of goal lists into a single list
  append(GoalsList, AllGoals),
  % Add goal to print all collected results at the end
  % We'll collect results using a global variable pattern
  PrintGoal = (nb_getval(all_results, AllResults), forall(member(R, AllResults), writeln(R))),
  InitGoal = nb_setval(all_results, []),
  % Combine: initialize, run all goals, print results
  append([[InitGoal], AllGoals, [PrintGoal]], FinalGoals),
  % Create the main clause body
  list_to_conjunction(FinalGoals, MainBody),
  % Format as a clause
  MainPredicate = (main :- MainBody),
  with_output_to(string(MainClause), portray_clause(MainPredicate)).

% Helper to convert list of goals to conjunction
list_to_conjunction([], true).
list_to_conjunction([G], G) :- !.
list_to_conjunction([G|Gs], (G, Rest)) :- list_to_conjunction(Gs, Rest).

% Collect specialized function goals for compilation output
% This finds all specialized functions that were generated and returns goals to assert them
collect_specialization_goals(Goals) :-
  ho_specialization(_, SpecName),
  % Find a clause for this specialized function
  current_predicate(SpecName/Arity),
  functor(Head, SpecName, Arity),
  clause(Head, Body, Ref),
  % Verify this clause was created by the specializer and get its source term
  translated_from(Ref, SourceTerm),
  % Return goals that register the function, assert clause and mapping
  ( Body == true -> Show = Head ; Show = (Head :- Body) ),
  Goals = [register_fun(SpecName), assertz(Show, R), assertz(translated_from(R, SourceTerm))].



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
