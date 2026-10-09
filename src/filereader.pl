:- use_module(library(readutil)). % read_file_to_string/3
:- use_module(library(pcre)). % re_replace/4
:- current_prolog_flag(argv, Args), ( (memberchk(silent, Args) ; memberchk('--silent', Args) ; memberchk('-s', Args))
                                      -> assertz(silent(true)) ; assertz(silent(false)) ).
:- dynamic working_dir/1.
:- dynamic active_metta_load/2.
:- dynamic metta_source_functions_started/1.

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

load_metta_file_impl(Filename, Results, Space) :-
    absolute_file_name(Filename, CanonPath, [access(read), file_errors(fail)]),
    setup_call_cleanup(
        assertz(active_metta_load(Space, CanonPath), Ref),
        load_metta_file_contents(CanonPath, Results, Space),
        erase(Ref)
    ).

load_metta_file_contents(CanonPath, Results, Space) :-
    claim_source_compile(CanonPath, Mode),
    setup_call_cleanup(
        push_working_dir(CanonPath),
        ( read_file_to_string(CanonPath, S, []),
          current_metta_file(Prev),
          setup_call_cleanup(nb_setval('$metta_file', CanonPath),
                             process_metta_string(S, Results, Space, Mode),
                             nb_setval('$metta_file', Prev)) ),
        pop_working_dir
    ).

claim_source_compile(CanonPath, space_only) :-
    metta_source_functions_started(CanonPath), !.
claim_source_compile(CanonPath, compile_functions) :-
    assertz(metta_source_functions_started(CanonPath)).

%Name the file in an error that carries no context of its own (unbound, or a
%bare tag such as typecheck). Any other context - context/2, a stack-overflow
%dict, a predicate indicator - is the thrower's and is rethrown unchanged:
rethrow_metta_file_error(Filename, error(Type, Context)) :- ( var(Context) ; atom(Context) ), !,
                                                           throw(error(Type, context(Filename, 'while loading MeTTa file'))).
rethrow_metta_file_error(_, Error) :- throw(Error).

current_metta_file(File) :- catch(nb_getval('$metta_file', File), _, File = '<string>').

%Run Goal as though it were being compiled from File. Dependency revalidation
%uses this interface without owning the filereader's thread-local state.
in_metta_file(File, Goal) :-
    current_metta_file(Prev),
    setup_call_cleanup(nb_setval('$metta_file', File),
                       Goal,
                       nb_setval('$metta_file', Prev)).

%Parse MeTTa source into its balanced top-level forms:
metta_string_forms(S, Forms) :- string_codes(S, Cs),
                                strip(Cs, 0, Codes),
                                phrase(top_forms(Forms, 1), Codes).

%Extract function definitions, call invocations, and S-expressions part of &self space:
process_metta_string(S, Results) :- process_metta_string(S, Results, '&self').
process_metta_string(S, Results, Space) :-
    process_metta_string(S, Results, Space, compile_functions).
process_metta_string(S, Results, Space, Mode) :- metta_string_forms(S, Forms),
                                           maplist(parse_form, Forms, ParsedForms),
                                           %declaration prepass: every function type declaration in the
                                           %file is visible to every definition in it, independent of order
                                           with_decl_notifications_batched(
                                               forall(member(parsed(expression, FormStr, Line, Decl), ParsedForms),
                                                      with_form_location(Line, FormStr,
                                                                         precache_fn_type_decl(Space, Decl)))),
                                           %clause-BODY prepass, same rationale: the output-certificate
                                           %prover (output_cert/3) may need a later definition's bodies
                                           %while an earlier one is validated - mutually recursive
                                           %bound-Bool functions in source order. Keyed by file so a
                                           %nested import cannot clobber the outer file's set, and
                                           %cleaned on the way out, error or not:
                                           current_metta_file(File),
                                           setup_call_cleanup(
                                               precache_pending_bodies(File, ParsedForms),
                                               with_unified_file_analysis_for_mode(
                                                   Mode, ParsedForms,
                                                   ( %exhaustiveness is a property of the whole clause set, so it
                                                     %is judged here, once the file's clauses and declarations are
                                                     %all visible, and before any of its forms runs:
                                                     det_exhaustiveness_prepass(ParsedForms),
                                                     maplist(process_form(Space, Mode), ParsedForms, ResultsList) )),
                                               retractall(pending_clause_body(File, _, _, _))), !,
                                           append(ResultsList, Results).

% Re-importing a source into another MeTTa space only replays its atoms.  Its
% functions remain the already-compiled clauses from the first load, so a
% second whole-file IR solve has no consumer and cannot affect code generation.
with_unified_file_analysis_for_mode(compile_functions, ParsedForms, Goal) :- !,
    with_unified_file_analysis(ParsedForms, Goal).
with_unified_file_analysis_for_mode(_, _, Goal) :-
    call(Goal).

register_function_signature(F, Arity) :- warn_if_used_as_symbol(F),
                                         register_fun(F),
                                         ( catch(arity(F, Arity), _, fail) -> true ; assertz(arity(F, Arity)) ).

%A function arriving after expressions already compiled its name as a plain symbol
%cannot be called by those expressions anymore, which usually means an import came too late:
warn_if_used_as_symbol(F) :- \+ fun(F), symbol_head(F), !,
                             format(user_error, "Warning: ~w is defined or imported after already being used; earlier expressions treat it as a plain symbol. Move the import or definition above the first use.~n", [F]).
warn_if_used_as_symbol(_).

precache_pending_bodies(File, ParsedForms) :-
    forall(( member(parsed(function, _, _, Term), ParsedForms),
             Term = [=, [F|Args], Body], atom(F) ),
           ( length(Args, N),
             assertz(pending_clause_body(File, F, N, Body)) )).

%First pass to convert MeTTa to Prolog Terms and register functions:
parse_form(form(S, L), parsed(T, S, L, Term)) :- sread(S, Term),
                                                 ( Term = [=, [F|W], _], atom(F) -> length(W, N), Arity is N + 1,
                                                                                    register_function_signature(F, Arity), T=function
                                                                                  ; T=expression ).
parse_form(runnable(S, L), parsed(runnable, S, L, Term)) :- sread(S, Term).

%Report where a static type/determinism error was raised before rethrowing it,
%and publish the same source location while declaration caching runs so the
%canonical declaration record retains its provenance.
with_form_location(Line, FormStr, Goal) :-
    current_metta_file(File),
    setup_call_cleanup(
        asserta(declaration_provenance(source(File, Line)), Ref),
        catch(Goal, error(E, Ctx),
              ( ( nonvar(Ctx), static_error_ctx(Ctx)
                  -> format(user_error, "Type error at ~w:~w in:~n  ~w~n",
                            [File, Line, FormStr])
                ; true ),
                throw(error(E, Ctx)) )),
        erase(Ref)).

static_error_ctx(typecheck).
static_error_ctx(determinism).

%Second pass to compile / run / add the Terms:
process_form(Space, _, parsed(expression, FormStr, Line, Term), []) :-
                                                           with_form_location(
                                                               Line, FormStr,
                                                               with_unified_preanalyzed_form(
                                                                   'add-atom'(Space, Term, true))),
                                                           ( silent(true) -> true ; swrite(Term,STerm),
                                                                                    format("\e[33m--> metta sexpr -->~n\e[36m~w~n", [STerm]),
                                                                                    format("\e[33m^^^^^^^^^^^^^^^^^^^~n\e[0m") ).
process_form(_, _, parsed(runnable, FormStr, Line, Term), Result) :- with_form_location(Line, FormStr,
                                                                                     translate_expr([collapse, Term], Goals, Result)),
                                                                  ( silent(true) -> true ; format("\e[33m--> metta runnable  -->~n\e[36m!~w~n\e[33m-->  prolog goal  -->\e[35m ~n", [FormStr]),
                                                                                           forall(member(G, Goals), portray_clause((:- G))),
                                                                                           format("\e[33m^^^^^^^^^^^^^^^^^^^^^^^~n\e[0m") ),
                                                                  call_goals(Goals).
process_form(Space, space_only, parsed(function, _, _, Term), []) :- !, add_sexp(Space, Term).
process_form(Space, compile_functions, parsed(function, FormStr, Line, Term), []) :- add_sexp(Space, Term),
                                                                  with_form_location(Line, FormStr,
                                                                                     translate_clause(Term, Clause, true,
                                                                                                      Dependencies)),
                                                                  assertz(Clause, Ref),
                                                                  assertz(translated_from(Ref, Term)),
                                                                  Term = [=, [Fn|Args], _],
                                                                  length(Args, N),
                                                                  record_compiled_dependencies(Ref, Fn/N, Dependencies),
                                                                  notify_mutation(
                                                                      clause_changed(Fn/N,
                                                                                     prevalidated)),
                                                                  metta_on_function_changed(Fn),
                                                                  ( silent(true) -> true ; format("\e[33m--> metta function -->~n\e[36m~w~n\e[33m--> prolog clause -->~n\e[32m", [FormStr]),
                                                                                           clause(Head, Body, Ref),
                                                                                           ( Body == true -> Show = Head; Show = (Head :- Body) ),
                                                                                           portray_clause(current_output, Show),
                                                                                           format("\e[33m^^^^^^^^^^^^^^^^^^^^^^~n\e[0m") ).
process_form(_, _, In, _) :- format(atom(Msg), "failed to process form: ~w", [In]), throw(error(syntax_error(Msg), none)).

%Like blanks but counts newlines:
newlines(C0, C2) --> blanks_to_nl, !, {C1 is C0+1}, newlines(C1,C2).
newlines(C, C) --> blanks.

%Collect characters until all parentheses are balanced (depth 0), accumulating codes, and also counting newlines:
grab_until_balanced(D, Acc, Cs, LC0, LC2, InS) --> [C],
    ( { InS = 1, C = 0'\\ } -> [E], { ( E=10 -> LC1 is LC0+1 ; LC1 = LC0 ) },
                               grab_until_balanced(D, [E, C|Acc], Cs, LC1, LC2, 1)
    ; { ( C=0'" -> InS1 is 1-InS ; InS1 = InS ),
        ( InS = 0 -> ( C=0'( -> D1 is D+1
                     ; C=0') -> D1 is D-1
                              ; D1 = D )
                   ; D1 = D ),
        Acc1=[C|Acc],
        ( C=10 -> LC1 is LC0+1 ; LC1 = LC0 ) },
      ( { D1=:=0, InS1=0 } -> { reverse(Acc1,Cs) , LC2 = LC1 }
                            ; grab_until_balanced(D1,Acc1,Cs,LC1,LC2,InS1) ) ).

%Read a balanced (...) block if available, turn into string, then continue with rest, ignoring comments:
top_forms([],_) --> blanks, eos.
top_forms([Term|Fs], LC0) --> newlines(LC0, LC1),
                              ( "!" -> {Tag = runnable} ; {Tag = form} ),
                              ( "(" -> [] ; string_without("\n", Rest), { format(atom(Msg), "expected '(' or '!(', line ~w:~n~s", [LC1, Rest]), throw(error(syntax_error(Msg), none)) } ),
                              ( grab_until_balanced(1, [0'(], Cs, LC1, LC2, 0)
                                -> { true } ; string_without("\n", Rest), { format(atom(Msg), "missing ')', starting at line ~w:~n~s", [LC1, Rest]), throw(error(syntax_error(Msg), none)) } ),
                              { string_codes(FormStr, Cs), Term =.. [Tag, FormStr, LC1] },
                              top_forms(Fs, LC2).

%Strip off code that is commented out, while tracking when inside of string:
strip([], _, []).
strip([0'\\, C|R], 1, [0'\\, C|O]) :- !, strip(R, 1, O).
strip([0'"|R], 0, [0'"|O]) :- !, strip(R, 1, O).
strip([0'"|R], 1, [0'"|O]) :- !, strip(R, 0, O).
strip([0'\n|R], In, [0'\n|O]) :- !, strip(R, In, O).
strip([0';|R], 0, Out) :- !, (append(_, [0'\n|Rest], R) -> strip(Rest, 0, Out) ; Out = []).
strip([C|R], In, [C|O]) :- strip(R, In, O).
