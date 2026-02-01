%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^%
%                         Continuation (de)serializer by Siyuan Chen:                                              %
% https://swi prolog.discourse.group/t/is there a way to serialize continuation or program execution state/5548/19 %
%vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv%

%Serialize continuation:
serialize(Term, STerm), \+ compound(Term) => STerm = Term.
serialize(Term, STerm), is_list(Term) => maplist([In, Out] >> serialize(In, Out), Term, STerm).
serialize(Term, STerm), Term =.. ['$cont$', Module, Clause, PC | Args] => maplist([In, Out] >> serialize(In, Out), Args, SArgs),
                                                                          nth_clause(Pred, Index, Clause),
                                                                          STerm =.. ['$cont$', Module, '$CLAUSE'(Pred, Index), PC | SArgs].
serialize(Term, STerm), Term =.. [Name | Args] => maplist([In, Out] >> serialize(In, Out), Args, SArgs),
                                                  STerm =.. [Name | SArgs].

%Deserialize continuaton:
deserialize(STerm, Term), \+ compound(STerm) => Term = STerm.
deserialize(STerm, Term), is_list(STerm) => maplist([In, Out] >> deserialize(In, Out), STerm, Term).
deserialize(STerm, Term), STerm =.. ['$cont$', Module, '$CLAUSE'(Pred, Index), PC | SArgs] => maplist([In, Out] >> deserialize(In, Out), SArgs, Args),
                                                                                              nth_clause(Pred, Index, Clause),
                                                                                              Term =.. ['$cont$', Module, Clause, PC | Args].
deserialize(STerm, Term), STerm =.. [Name | SArgs] => maplist([In, Out] >> deserialize(In, Out), SArgs, Args),
                                                      Term =.. [Name | Args].

%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^%
%                         PeTTa compute state save and restore via file by Patrick Hammer:                         %
%vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv%

atomic_write_term(File, Term, Options) :- string_concat(File, ".tmp", Tmp),
                                          open(Tmp, write, Out),
                                          write_term(Out, Term, Options),
                                          flush_output(Out),
                                          close(Out),
                                          rename_file(Tmp, File).

%qsave_program but with saved continuation:
qsave_program_continuation(Name, Cont, Var, Resume, true) :- string_concat(Name, ".continuation", ContFile),
                                                             string_concat(Name, ".outputvariable", VarFile),
                                                             string_concat(Name, ".commit", CommitFile),
                                                             string_concat(Name, ".commit.new", NewCommitFile),
                                                             %Serialize and save continuation
                                                             serialize(Cont, SCont),
                                                             atomic_write_term(ContFile, SCont, [quoted(true), fullstop(true), nl(true)]),
                                                             %Save output variable index:
                                                             term_variables(Cont, Vars),
                                                             nth0(Index, Vars, V),
                                                             V == Var,
                                                             atomic_write_term(VarFile, Index, [fullstop(true), nl(true)]),
                                                             atomic_write_term(NewCommitFile, ok, [fullstop(true), nl(true)]),
                                                             rename_file(NewCommitFile, CommitFile),
                                                             %Save clauses:
                                                             string_concat(Name, ".new", TmpExe),
                                                             qsave_program(TmpExe, [goal(load_saved_continuation(Name, Resume))]),
                                                             rename_file(TmpExe, Name).

%Restoring continuation after qsaved program gets re-invoked:
load_saved_continuation(Name, Resume) :- string_concat(Name, ".continuation", ContFile),
                                         string_concat(Name, ".outputvariable", VarFile),
                                         string_concat(Name, ".commit", CommitFile),
                                         ( exists_file(CommitFile) -> true ; throw(error(no_valid_snapshot, _)) ),
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
