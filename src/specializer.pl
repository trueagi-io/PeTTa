:- dynamic ho_specialization/2.
:- dynamic ho_specialization_failed/3.

%Reuse an existing specialization, including one being compiled. A recursive request for a new key
%of the same function is refused below, so growing keys such as (evolve (twice $r) $g) cannot make
%translation diverge:
maybe_specialize_call(HV, AVs, Out, Goal) :-
    catch(nb_getval('$spec_stack', Stack), _, Stack = []),
    ( Stack == [] -> nb_linkval('$spec_created', []) ; true ),
    nb_getval('$spec_created', Before),
    setup_call_catcher_cleanup(
        ( catch(nb_getval(specneeded, Prev), _, Prev = []),
          nb_setval(specneeded, false), nb_setval('$spec_stack', [HV|Stack]) ),
        specialize_call(HV, AVs, Out, Goal),
        Catcher,
        ( ( Catcher == exit -> true ; forget_specialization_symbols(Before), nb_setval(specneeded, Prev) ),
          ( Stack == [] -> nb_delete('$spec_created') ; true ),
          nb_setval('$spec_stack', Stack),
          ( Prev == true -> nb_setval(specneeded, Prev) ; true ) ) ).

%Keep only symbols created by this attempt; nested successes still belong to their outer attempt.
remember_specialization_symbol(Name) :-
    nb_getval('$spec_created', Before),
    nb_linkval('$spec_created', [Name|Before]).

forget_specialization_symbols(Before) :-
    nb_getval('$spec_created', Created),
    forget_specialization_symbols(Created, Before),
    nb_linkval('$spec_created', Before).

forget_specialization_symbols(Created, Before) :- Created == Before, !.
forget_specialization_symbols([Name|Created], Before) :-
    forget_symbol(Name),
    retractall(ho_specialization(_, Name)),
    forget_specialization_symbols(Created, Before).

% Build a stable, variant-normalized specialization key.
%
% This intentionally descends through arbitrary compound terms (including
% partial/2 closures).  A shallow list-only variable replacement leaves fresh
% Prolog variable ids inside compound terms, producing unstable specialization
% names such as app_Spec_[partial(lambda_1,[_17896])].
normalize_specialization_key(Term, Normalized) :-
    copy_term(Term, Normalized),
    numbervars(Normalized, 0, _, [singletons(true)]).

%Specialize a call by creating and translating a specialized version of the MeTTa code:
specialize_call(HV, AVs, Out, Goal) :- %1. Retrieve a copy of all meta-clauses stored for HV:
                                       catch(nb_getval(HV, MetaList0), _, fail),
                                       copy_term(MetaList0, MetaList),
                                       %2. Copy all clause variables eligible for specialization across all meta-clauses:
                                       bagof(HoVar, ArgsNorm^BodyExpr^HoBinds^HoBindsPerArg^
                                                    ( member(fun_meta(ArgsNorm, BodyExpr), MetaList),
                                                      maplist(specializable_vars(BodyExpr), AVs, ArgsNorm, HoBinds),
                                                      member(HoBindsPerArg, HoBinds),
                                                      member(HoVar, HoBindsPerArg),
                                                      nonvar(HoVar) ), BindSet),
                                       %3. Build the specialization name from the concrete higher-order bind set:
                                       normalize_specialization_key(BindSet, CleanBindSet),
                                       length(AVs, N),
                                       Arity is N + 1,
                                       \+ ho_specialization_failed(HV, Arity, CleanBindSet),
                                       format(atom(SpecName), "~w_Spec_~w",[HV, CleanBindSet]),
                                       %4. Specialize, but only if not already specialized. While HV is being specialized, a nested call
                                       %   calls the specialization its key already has, such as the one being made, and makes no new one:
                                       ( ho_specialization(HV, SpecName)
                                         ; nb_getval('$spec_stack', [HV|Outer]), memberchk(HV, Outer), !, nb_setval(specneeded, false), fail
                                         ; ( %4.1. Otherwise register the specialization:
                                             remember_specialization_symbol(SpecName),
                                             register_fun(SpecName),
                                             assertz(ho_specialization(HV, SpecName)),
                                             assertz(arity(SpecName, Arity)),
                                             ( %4.2. Re-use the type definition of the parent function for the specialization:
                                               findall(TypeChain, catch(match('&self', [':', HV, TypeChain], TypeChain, TypeChain), _, fail), TypeChains),
                                               forall(member(TypeChain, TypeChains), add_sexp('&self', [':', SpecName, TypeChain])),
                                               %4.3 Translate specialized MeTTa clauseses to Prolog, keeping track of the function we are compiling through recursion:
                                               maplist({SpecName}/[fun_meta(ArgsNorm,BodyExpr),clause_info(Input,Clause)]>>
                                                       ( Input = [=,[SpecName|ArgsNorm],BodyExpr], translate_clause(Input,Clause,false) ), MetaList, ClauseInfos),
                                               %4.4 Only proceeed specializing if this or any recursive call profited from specialization with the specialized function at head position:
                                               nb_getval(specneeded, true),
                                               %4.5 Assert and print each of the created specializations:
                                               forall(member(clause_info(Input, Clause), ClauseInfos),
                                               ( asserta(Clause, Ref),
                                                 assertz(translated_from(Ref, Input)),
                                                 add_sexp('&self', Input),
                                                 format(atom(Label), "metta specialization (~w)", [SpecName]),
                                                 maybe_print_compiled_clause(Label, Input, Clause) ))
                                               %4.6 Remember a refused key; the enclosing attempt cleans up its generated symbols:
                                               -> true ; ( silent(true) -> true ; format("Not specialized ~w~n", [SpecName/Arity]) ),
                                                         ( ho_specialization_failed(HV, Arity, CleanBindSet)
                                                           -> true
                                                            ; assertz(ho_specialization_failed(HV, Arity, CleanBindSet)) ),
                                                         fail ))), !,
                                       %5. Generate call to the specialized function:
                                       append(AVs, [Out], CallArgs),
                                       Goal =.. [SpecName|CallArgs].

%Extracts clause-head variables and their call-site copies, producing eligible Var–Copy pairs for specialization:
specializable_vars(BodyExpr, Value, Arg, HoVars) :- term_variables(Arg, Vars),
                                                    copy_term(Arg-Vars, ArgCopy-VarsCopy),
                                                    traverse_list([A,V]>>(nonvar(V) ->  V = A;  true), ArgCopy, Value),
                                                    eligible_var_pairs(Vars, VarsCopy, BodyExpr, HoVars).

traverse_list(Pred, From, Into) :- (is_list(From),is_list(Into) -> maplist(traverse_list(Pred),From,Into)
                                                                 ; call(Pred, From, Into)).

%Selects and unifies variable–argument pairs that act as higher-order or head operands in the body:
eligible_var_pairs([], [], _, []).
eligible_var_pairs([Var|Vars], [Copy|Copies], BodyExpr, HoVars) :- ( specializable_arg(Copy), (var_use_check(head, Var, BodyExpr) ; var_use_check(ho, Var, BodyExpr))
                                                                     -> Var = Copy,
                                                                        HoVars = [Var|RestHoVars]
                                                                      ; HoVars = RestHoVars ),
                                                                   eligible_var_pairs(Vars, Copies, BodyExpr, RestHoVars).

%If Var appears at list head it means function call, meaning specialization is needed, and detect when used as HOL arg
var_use_check(head, Var, [Head|_]) :- Var == Head,
                                      nb_setval(specneeded, true).
var_use_check(ho, Var, [Head|Args]) :- specializable_arg(Head),
                                       member(Arg, Args),
                                       ( Var == Arg
                                       ; is_list(Arg),
                                         var_use_check(ho, Var, Arg) ).
var_use_check(Mode, Var, L) :- is_list(L),
                               member(E, L),
                               is_list(E),
                               var_use_check(Mode, Var, E).

%Tests whether an argument represents a specializable function or partial application:
specializable_arg(Arg) :- nonvar(Arg), 
                          ( fun(Arg) ; Arg = partial(_, _) ).

%Forget function symbol:
forget_symbol(Name) :- retractall('&self'(=, [Name|_], _)),
                       retractall('&self'(:, Name, _)),
                       findall(Ref, ( current_predicate(Name/A), functor(H, Name, A), clause(H, _, Ref) ), Refs),
                       forall(member(R, Refs), (erase(R), retractall(translated_from(R, _)))),
                       metta_on_function_removed(Name),
                       retractall(arity(Name,_)),
                       retractall(fun(Name)),
                       catch(nb_delete(Name), _, true),
                       retractall(ho_specialization(Name,_)).

%Invalidate all specializations:
invalidate_specializations(F) :-
    retractall(ho_specialization_failed(_,_,_)),
    findall(Spec, ho_specialization(F, Spec), Specs),
    forall(member(S, Specs), invalidate_specializations(S)),
    forall(member(S, Specs), forget_symbol(S)),
    retractall(ho_specialization(F,_)).
