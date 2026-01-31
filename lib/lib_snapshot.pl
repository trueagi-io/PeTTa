%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^%
%                         Continuation (de)serializer by Siyuan Chen:                                              %
% https://swi prolog.discourse.group/t/is there a way to serialize continuation or program execution state/5548/19 %
%vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv%

%Serialize continuation:
serialize(Term, STerm), blob(Term, clause) => nth_clause(Pred, Index, Term),  
                                              STerm = '$CLAUSE'(Pred, Index).
serialize(Term, STerm), \+ compound(Term) => STerm = Term.
serialize(Term, STerm), is_list(Term) => maplist([In, Out]>>serialize(In, Out), Term, STerm).
serialize(Term, STerm), Term =.. [Name | Args] => maplist([In, Out]>>serialize(In, Out), Args, SArgs),
                                                  STerm =.. [Name | SArgs].

%Deserialize continuaton:
deserialize('$CLAUSE'(Pred, Index), Term) => nth_clause(Pred, Index, Term).
deserialize(STerm, Term), \+ compound(STerm) => Term = STerm.
deserialize(STerm, Term), is_list(STerm) => maplist([In, Out]>>deserialize(In, Out), STerm, Term).
deserialize(STerm, Term), STerm =.. [Name | SArgs] => maplist([In, Out]>>deserialize(In, Out), SArgs, Args),
                                                      Term =.. [Name | Args].

%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^%
%                         PeTTa compute state save and restore via file by Patrick Hammer:                         %
%vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv%

%qsave_program but with saved continuation:
qsave_program_continuation(Name, Cont, Var, Resume, true) :- string_concat(Name, ".continuation", ContFile),
                                                             string_concat(Name, ".outputvariable", VarFile),
                                                             %Serialize and save continuation
                                                             serialize(Cont, SCont),
                                                             open(ContFile, write, Out1),
                                                             write_term(Out1, SCont, [quoted(true), fullstop(true), nl(true)]),
                                                             close(Out1),
                                                             %Save output variable index:
                                                             term_variables(Cont, Vars),
                                                             nth0(Index, Vars, V),
                                                             V == Var,
                                                             open(VarFile, write, Out2),
                                                             write_term(Out2, Index, [fullstop(true), nl(true)]),
                                                             close(Out2),
                                                             %Save clauses:
                                                             qsave_program(Name, [goal(load_saved_continuation(Name, Resume))]).

%Restoring continuation after qsaved program gets re-invoked:
load_saved_continuation(Name, Resume) :- string_concat(Name, ".continuation", ContFile),
                                         string_concat(Name, ".outputvariable", VarFile),
                                         %Load and deserialize continuation:
                                         open(ContFile, read, In1),
                                         read_term(In1, SCont, []),
                                         close(In1),
                                         deserialize(SCont, Cont),
                                         %Retrieve continuation variable:
                                         open(VarFile, read, In2),
                                         read_term(In2, Index, []),
                                         close(In2),
                                         term_variables(Cont, Vars),
                                         nth0(Index, Vars, OutVar),
                                         %Resume continuation:
                                         call(Resume, Cont, OutVar, X).

%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^%
