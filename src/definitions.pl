%%% Definitions: stored translations and their refresh %%%
%
% A stored clause is the translation of one source equation, together with the
% observations its translation consumed. Every program edit ends in
% program_changed/1, which translates again each stored clause whose
% observations no longer hold, so a call enters the code a fresh translation at
% its entry would produce. Code that is already running keeps its clauses
% (SWI-Prolog's logical update view).
%
% Dependencies describe translation observations, not occurrences in quoted data.
% A nested lambda records its own observations, independently of its creator.
:- dynamic translated_from/2, translation_use/4, translation_decision/3, pending_signature/3,
           no_equations_clause/3, native_function/1, native_arity/2, equation_arity/3,
           program_generation/1.
program_generation(0).

compile_equation(Term, Clause, Uses) :-
    ( nb_current(petta_translation_uses, Previous) -> true ; Previous = '$absent' ),
    Box = uses([]),
    setup_call_cleanup(nb_linkval(petta_translation_uses, Box),
        ( copy_term(Term, Input), once(translate_clause(Input, Clause)),
          arg(1, Box, Raw), sort(Raw, Uses) ),
        ( Previous == '$absent' -> nb_delete(petta_translation_uses)
        ; nb_linkval(petta_translation_uses, Previous) )).

% Single shared compile-time observation interface. Translation consumes the
% value returned by observe/2 through read_obs/2; refresh validates recorded
% reads through the exact same observe/2 predicate.
obs_owner(program_generation, '$program') :- !.
obs_owner(Key, Owner) :- arg(1, Key, Owner).

read_obs(Key, Value) :-
    observe(Key, Value),
    remember_obs(Key, Value).

remember_obs(Key, Value) :-
    ( nb_current(petta_translation_uses, Box), Box = uses(_),
      obs_owner(Key, Owner), atom(Owner)
      -> ( ground(Value) -> Stored = Value ; copy_term(Value, Stored) ),
         arg(1, Box, Old),
         nb_setarg(1, Box, [use(Owner, Key, Stored)|Old])
    ; true ).

observe(program_generation, Gen) :-
    ( program_generation(Gen) -> true ; Gen = 0 ).
observe(body_mode(F), Mode) :-
    ( declared_output_type(F, 'Atom') -> Mode = held ; Mode = evaluated ).
observe(pattern_callable(F), IsCallable) :-
    ( fun(F) -> IsCallable = true ; IsCallable = false ).
observe(eta_target(F, N), Target) :-
    ( eta_expansion_arity(F, N, Arity) -> Target = some(Arity) ; Target = none ).
observe(call_goal(F, N), Template) :-
    call_goal_template(F, N, Template).
observe(rule_dispatch(F, N), Value) :-
    ( rule_expands(F, N)
      -> rule_arg_types(F, N, ArgTypes),
         Value = expand(ArgTypes)
    ; Value = none ).
observe(app_dispatch(F, N), Value) :-
    ( observe(rule_dispatch(F, N), expand(ArgTypes))
      -> Value = rule(ArgTypes)
    ; callable_dispatch(F, N, Value) ).
observe(partial_dispatch(F, N), Value) :-
    callable_dispatch(F, N, Value).

translation_use_valid(Owner, Key, Saved) :-
    observe(Key, Now),
    ( Now =@= Saved -> true
    % An old complete, untyped direct call can keep calling a data-returning fallback.
    ; Key = app_dispatch(Owner, N),
      Now == data,
      Saved == call([], direct),
      A is N + 1,
      no_equations_clause(Owner, A, _) ).

record_translation(Ref, Term, Uses) :- assertz(translated_from(Ref, Term)),
    forall(member(use(F, Use, Key), Uses),
        ( assertz(translation_use(F, Ref, Use, Key)),
          ( translation_decision(F, Use, Stored), Stored =@= Key -> true
          ; assertz(translation_decision(F, Use, Key)) ) )).
forget_translation(Ref) :-
    findall(use(F, Use, Key), translation_use(F, Ref, Use, Key), Uses),
    retractall(translated_from(Ref, _)), retractall(translation_use(_, Ref, _, _)),
    retractall(equation_arity(_, _, Ref)),
    forall(member(use(F, Use, Key), Uses),
        ( translation_use(F, _, Use, Remaining), Remaining =@= Key -> true
        ; forall((clause(translation_decision(F, Use, Stored), true, R), Stored =@= Key), erase(R)) )).

% Only registration/publication changes the program. Compiled calls have no
% runtime lookup added to them. SWI's logical update view still applies at
% each callee entry, including in an already-running caller.
register_signature(Owner, F, A) :-
    assertz(pending_signature(F, A, Owner)),
    ( fun(F) -> true ; assertz(fun(F)) ),
    ( arity(F, A) -> true ; assertz(arity(F, A)) ),
    no_equations_answer(F, A).

register_file_signatures(space_only, _, space_only) :- !.
register_file_signatures(compile_functions, Forms, compile_functions(Owner)) :-
    flag(metta_file_signatures, Owner, Owner + 1),
    with_mutex(petta_definitions, transaction(
        ( findall(F-A, ( member(parsed(function, _, [=, [F|Args], _]), Forms),
                         length(Args, N), A is N + 1 ), Signatures),
          forall(member(F-A, Signatures), register_signature(Owner, F, A)),
          findall(F, member(F-_, Signatures), Functions),
          program_changed(Functions) ) )).

% A failed/aborted load must not leave provisional arities for forms it never
% reached. Nested imports have independent owners.
finish_file_signatures(space_only) :- !.
finish_file_signatures(compile_functions(Owner)) :- with_mutex(petta_definitions, transaction(
    ( findall(F, pending_signature(F, _, Owner), Raw), sort(Raw, Fs),
      retractall(pending_signature(_, _, Owner)),
      forall(member(F, Fs), sync_function_arities(F)),
      program_changed(Fs) ) )).

install_definition(Space, Term, Owner, Clause) :- install_definition(Space, Term, Owner, Clause, _).
install_definition(Space, Term, Owner, Clause, Ref) :-
    with_mutex(petta_definitions, transaction(
        ( Term = [=, [F|Args], _], must_be(list, Args), length(Args, N), A is N + 1,
          ( Owner == runtime -> register_signature(Owner, F, A) ; true ),
          add_sexp(Space, Term),
          compile_equation(Term, Clause, Uses),
          assert_equation(Term, Clause, Uses, Ref),
          once(retract(pending_signature(F, A, Owner))),
          sync_function_arities(F), program_changed([F]),
          notify_translation_change(F) ) )).

assert_equation(Term, Clause, Uses, Ref) :-
    Clause = (Head :- _), functor(Head, F, A),
    ( retract(no_equations_clause(F, A, Empty)) -> erase(Empty) ; true ),
    assertz(Clause, Ref), assertz(equation_arity(F, A, Ref)), record_translation(Ref, Term, Uses).

install_lambda(Term, Clause) :- with_mutex(petta_definitions, transaction(
    ( compile_equation(Term, Clause, Uses), assert_equation(Term, Clause, Uses, _),
      Term = [=, [F|_], _], sync_function_arities(F) ) )).

user_dynamic_head(F, A, H) :- current_predicate(F/A), functor(H, F, A),
    predicate_property(H, dynamic), \+ predicate_property(H, imported_from(_)).

has_equations(F, A) :- equation_arity(F, A, _), !.

no_equations_answer(F, A) :-
    ( native_arity(F, A) ; has_equations(F, A) ; no_equations_clause(F, A, _) ), !.
no_equations_answer(F, A) :-
    N is A - 1, length(Args, N),
    append(Args, [[F|Args]], HeadArgs), Head =.. [F|HeadArgs],
    assertz(Head, Ref), assertz(no_equations_clause(F, A, Ref)).

sync_function_arities(F) :-
    % Registration/translation add candidate arities. Probe each candidate once
    % rather than enumerating every equation of a large multi-clause function.
    forall(arity(F, A),
        ( (native_arity(F, A); pending_signature(F, A, _); equation_arity(F, A, _))
          -> true ; retractall(arity(F, A)) )),
    ( ( native_function(F) ; arity(F, _) )
      -> ( fun(F) -> true ; assertz(fun(F)) )
    ; retractall(fun(F)) ).

% program_generation/1 is a dynamic predicate, so SWI-Prolog's transaction/1
% rolls it back together with clauses and metadata on abort.
bump_program_generation :-
    ( retract(program_generation(G0)) -> G1 is G0 + 1 ; G1 = 1 ),
    assertz(program_generation(G1)).

% Every program edit ends here, with the symbols whose rows it changed. An edit
% that changed no symbol changes nothing.
program_changed([]) :- !.
program_changed(Symbols) :-
    bump_program_generation,
    sort(Symbols, Changed),
    forall(( member(F, Changed), atom(F) ), refresh_users(F, [])),
    refresh_users('$program', []).

refresh_users(F, Path) :-
    % Check each distinct decision once, even when thousands of callers share it.
    findall(G, ( translation_decision(F, Use, Key), \+ translation_use_valid(F, Use, Key),
                 translation_use(F, Ref, Use, Stored), Stored =@= Key,
                 translated_from(Ref, [=, [G|_], _]) ), Raw),
    sort(Raw, Users),
    forall(member(G, Users),
        ( memberchk(G, Path) -> throw(error(domain_error(finite_application_interface, [G|Path]), none))
        ; recompile_function(G), refresh_users(G, [G|Path]) )).

% Rebuild a whole predicate in clause order; publication and its metadata are
% one transaction. Never inspect native or imported clauses with clause/3.
recompile_function(F) :-
    % Source records preserve order even when clauses move between arities.
    findall(Ref-Term, ( Term = [=, [F|_], _], translated_from(Ref, Term),
                       clause(H, _, Ref), functor(H, F, A), user_dynamic_head(F, A, H) ), Pairs),
    findall(A, equation_arity(F, A, _), OldArities),
    maplist(recompile_equation, Pairs, Compiled),
    forall(member(Ref-_, Pairs), ( erase(Ref), forget_translation(Ref) )),
    forall(member(compiled(Term, Clause, Uses), Compiled),
           assert_equation(Term, Clause, Uses, _)),
    forall(member(A, OldArities), no_equations_answer(F, A)),
    sync_function_arities(F), notify_translation_change(F).

notify_translation_change(F) :-
    forall(metta_on_function_changed(F), true).

recompile_equation(_-Term, compiled(Term, Clause, Uses)) :- compile_equation(Term, Clause, Uses).

removed_equations(F, Refs) :-
    forall(member(Ref-A, Refs), ( erase(Ref), forget_translation(Ref), no_equations_answer(F, A) )),
    sync_function_arities(F), program_changed([F]),
    notify_translation_change(F),
    ( fun(F) -> true ; forall(metta_on_function_removed(F), true) ).
