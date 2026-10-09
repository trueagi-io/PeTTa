#!/usr/bin/env swipl
/*  Seeded, self-contained program generator for fuzz_matrix.sh.

    Each generated source form occupies one line.  Besides keeping artifacts
    easy to read, this gives the runner a deliberately simple form-level
    deletion shrinker without making it parse MeTTa.
*/

:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(random)).

:- initialization(main, main).

main(Argv) :-
    ( Argv = [OutDir, SeedText, CountText],
      atom_number(SeedText, Seed),
      atom_number(CountText, Count),
      integer(Count), Count > 0
    -> true
    ; format(user_error,
             'usage: swipl -q -s fuzz_generator.pl -- OUTDIR SEED COUNT~n', []),
      halt(2)
    ),
    set_random(seed(Seed)),
    make_directory_path(OutDir),
    directory_file_path(OutDir, 'manifest.tsv', Manifest),
    setup_call_cleanup(
        open(Manifest, write, MS),
        forall(between(1, Count, Id), generate_one(OutDir, MS, Id)),
        close(MS)).

generate_one(OutDir, MS, Id) :-
    case_class(Id, Class),
    type_layer(Id, TypeForms, Model),
    ( Class == mutation
    -> mutation_program(Id, TypeForms, MainForms, FreshForms),
       case_path(OutDir, Id, incremental, MainPath),
       case_path(OutDir, Id, fresh, FreshPath),
       write_forms(MainPath, MainForms),
       write_forms(FreshPath, FreshForms),
       format(MS, '~d\tmutation\t~w\t~w~n',
              [Id, MainPath, FreshPath])
    ; function_program(Id, Class, Model, TypeForms, Forms),
      case_path(OutDir, Id, Class, Path),
      write_forms(Path, Forms),
      format(MS, '~d\t~w\t~w\t-~n', [Id, Class, Path])
    ).

case_class(Id, strict_det) :- 1 is Id mod 3, !.
case_class(Id, default) :- 2 is Id mod 3, !.
case_class(_, mutation).

case_path(Dir, Id, Suffix, Path) :-
    format(atom(Base), 'case_~|~`0t~d~4+_~w.metta', [Id, Suffix]),
    directory_file_path(Dir, Base, Path).

write_forms(Path, Forms) :-
    setup_call_cleanup(
        open(Path, write, S),
        forall(member(Form, Forms),
               ( render_form(Form, Text), format(S, '~w~n', [Text]) )),
        close(S)).

render_form(fmt(F, A), Text) :- !, format(string(Text), F, A).
render_form(Text, Text).

% ---------------------------------------------------------------------------
% Type layer

type_layer(Id, Forms, Model) :-
    random_between(1, 3, TypeCount),
    findall(Type,
            ( between(1, TypeCount, I),
              format(atom(Type), 'FuzzT~d_~d', [Id, I]) ),
            Types),
    maplist(nominal_type(Id, Types), Types, Blocks, Nominals),
    append(Blocks, NominalForms),
    optional_type_forms(Id, Extra),
    append([[fmt('; fuzz case ~d: randomized type layer', [Id])],
            NominalForms, Extra],
           Forms),
    Model = model(Types, Nominals).

nominal_type(Id, Types, Type, Forms, nominal(Type, Ctors)) :-
    random_between(1, 3, NCtors),
    nth1(TypeI, Types, Type),
    findall(Ctor,
            ( between(1, NCtors, CI),
              random_between(0, 3, Arity),
              random_field_types(Arity, Types, Fields),
              format(atom(Name), 'FuzzC~d_~d_~d', [Id, TypeI, CI]),
              Ctor = ctor(Name, Fields) ),
            Ctors),
    maplist(ctor_decl(Type), Ctors, CtorForms),
    Forms = CtorForms.

random_field_types(0, _, []) :- !.
random_field_types(N, Types, [T|Ts]) :-
    random_field_type(Types, T),
    N1 is N - 1,
    random_field_types(N1, Types, Ts).

random_field_type(Types, T) :-
    random_between(0, 5, Pick),
    ( Pick =:= 0 -> T = 'Number'
    ; Pick =:= 1 -> T = 'String'
    ; Pick =:= 2 -> T = 'Bool'
    ; Pick =:= 3 -> random_member(E, ['Number','Bool']), type_list(E, T)
    ; random_member(T, Types)
    ).

type_list(E, T) :- format(atom(T), '(List ~w)', [E]).

ctor_decl(Type, ctor(Name, []), Form) :-
    Form = fmt('(: ~w ~w)', [Name, Type]).
ctor_decl(Type, ctor(Name, Fields), Form) :-
    Fields \== [],
    atomic_list_concat(Fields, ' ', ArgText),
    Form = fmt('(: ~w (-[det]-> ~w ~w))', [Name, ArgText, Type]).

optional_type_forms(Id, Forms) :-
    random_between(0, 7, Bits),
    findall(Form,
            ( between(0, 2, B),
              1 is (Bits >> B) /\ 1,
              optional_type_form(B, Id, Form) ),
            Forms).

optional_type_form(0, Id,
                   fmt('(: FuzzAlias~d (Alias (Number Bool)))', [Id])).
optional_type_form(1, Id,
                   fmt('(: FuzzNew~d (Newtype Expression))', [Id])).
optional_type_form(2, Id,
                   fmt('(: fuzz-union-value-~d (| Number String))', [Id])).

% ---------------------------------------------------------------------------
% Function layer

function_program(Id, strict_det, Model, TypeForms, Forms) :-
    ( 0 is Id mod 10
    -> effect_poly_program(Id, TypeForms, Forms)
    ; standard_program(Id, strict_det, Model, TypeForms, Forms)
    ).
function_program(Id, default, Model, TypeForms, Forms) :-
    standard_program(Id, default, Model, TypeForms, Forms).

standard_program(Id, Class, Model, TypeForms, Forms) :-
    template(Id, Model, InputType, Heads, Query),
    choose_effect(Class, Effect),
    format(atom(F), 'fuzz-f-~d', [Id]),
    arrow_for_effect(Effect, Arrow),
    Decl = fmt('(: ~w (~w ~w Number))', [F, Arrow, InputType]),
    make_clauses(Id, F, Heads, ClauseForms, Multiplicities),
    matched_multiplicity(Heads, Multiplicities, Actual),
    promised_count(Effect, Actual, Expected),
    Test = fmt('!(test (length (collapse (~w ~w))) ~d)',
               [F, Query, Expected]),
    append([TypeForms,
            [fmt('; function template effect=~w query=~w', [Effect, Query])],
            [Decl], ClauseForms, [Test]],
           Forms).

choose_effect(strict_det, Effect) :-
    random_member(Effect, [det, semidet, nondet]).
choose_effect(default, Effect) :-
    random_member(Effect, [det, semidet, nondet, unspecified]).

arrow_for_effect(det, '-[det]->').
arrow_for_effect(semidet, '-[semidet]->').
arrow_for_effect(nondet, '-[nondet]->').
arrow_for_effect(unspecified, '->').

promised_count(det, _, 1).
promised_count(semidet, Actual, Expected) :-
    ( Actual =:= 0 -> Expected = 0 ; Expected = 1 ).
promised_count(nondet, Actual, Actual).
promised_count(unspecified, Actual, Actual).

%The body helper forms are returned separately by body_variant.  Flattening
%them before their clause keeps declarations visible to the file prepass.
make_clauses(Id, F, Heads, Forms, Multiplicities) :-
    make_clause_records(Id, F, Heads, 1, Records, Multiplicities),
    findall(P, (member(record(Ps,_), Records), member(P, Ps)), Prefixes),
    findall(C, member(record(_,C), Records), Clauses),
    append(Prefixes, Clauses, Forms).

make_clause_records(_, _, [], _, [], []).
make_clause_records(Id, F, [head(Pattern, _)|Hs], I,
                    [record(Prefix, Clause)|Rs], [M|Ms]) :-
    Base is Id * 10 + I,
    body_variant(Id, I, Base, Prefix, Body, M),
    format(string(Clause), '(= (~w ~w) ~w)', [F, Pattern, Body]),
    I1 is I + 1,
    make_clause_records(Id, F, Hs, I1, Rs, Ms).

body_variant(Id, I, Base, Prefix, Body, Multiplicity) :-
    random_between(0, 6, Kind),
    body_kind(Kind, Id, I, Base, Prefix, Body, Multiplicity).

body_kind(0, _, _, Base, [], Body, 1) :-
    format(atom(Body), '~d', [Base]).
body_kind(1, _, _, Base, [], Body, 1) :-
    format(atom(Body), '(+ ~d 1)', [Base]).
body_kind(2, Id, I, Base, Prefix, Body, 1) :-
    format(atom(H), 'fuzz-helper-~d-~d', [Id, I]),
    Prefix = [fmt('(: ~w (-[det]-> Number))', [H]),
              fmt('(= (~w) ~d)', [H, Base])],
    format(atom(Body), '(~w)', [H]).
body_kind(3, _, _, Base, [], Body, 2) :-
    B2 is Base + 100,
    format(atom(Body), '(superpose (~d ~d))', [Base, B2]).
body_kind(4, _, _, Base, [], Body, 1) :-
    B2 is Base + 1,
    format(atom(Body), '(if True ~d ~d)', [Base, B2]).
body_kind(5, _, _, Base, [], Body, 1) :-
    B2 is Base + 1,
    format(atom(Body), '(case True ((True ~d) (False ~d)))', [Base, B2]).
body_kind(6, _, _, Base, [], Body, 1) :-
    format(atom(Body), '(let $fuzz-y ~d (+ $fuzz-y 1))', [Base]).

matched_multiplicity(Heads, Ms, Total) :-
    findall(M,
            ( nth1(I, Heads, head(_, yes)), nth1(I, Ms, M) ),
            Matches),
    sum_list(Matches, Total).

template(Id, Model, Input, Heads, Query) :-
    Kind is Id mod 6,
    template_kind(Kind, Model, Input, Heads, Query).

template_kind(0, _, 'Number', [head('$x', yes)], Query) :-
    random_member(Query, ['2', '$free']).
template_kind(1, _, 'Bool',
              [head('True', MT), head('False', MF)], Query) :-
    random_member(Mode, [true, false, unbound]),
    bool_query(Mode, Query, MT, MF).
template_kind(2, _, 'Bool', [head('True', M)], Query) :-
    random_member(Mode, [true, false, unbound]),
    partial_bool_query(Mode, Query, M).
template_kind(3, _, 'Number',
              [head('0', M0), head('$x', yes)], Query) :-
    random_member(Mode, [zero, one, unbound]),
    numeric_query(Mode, Query, M0).
template_kind(4, _, '(List Number)',
              [head('()', MN), head('(cons $h $t)', MC)], Query) :-
    random_member(Mode, [nil, cons, partial, unbound]),
    list_query(Mode, Query, MN, MC).
template_kind(5, Model, Input, Heads, Query) :-
    nominal_template(Model, Input, Heads, Query).

bool_query(true, 'True', yes, no).
bool_query(false, 'False', no, yes).
bool_query(unbound, '$free', yes, yes).

partial_bool_query(true, 'True', yes).
partial_bool_query(false, 'False', no).
partial_bool_query(unbound, '$free', yes).

numeric_query(zero, '0', yes).
numeric_query(one, '1', no).
numeric_query(unbound, '$free', yes).

list_query(nil, '()', yes, no).
list_query(cons, '(cons 1 ())', no, yes).
list_query(partial, '(cons 1 $tail)', no, yes).
list_query(unbound, '$free', yes, yes).

nominal_template(Model, Input, Heads0, Query) :-
    Model = model(_, [nominal(Input, Ctors)|_]),
    random_member(Chosen, Ctors),
    ctor_value(Chosen, Query),
    maplist(ctor_head_for(Chosen), Ctors, Heads0).

ctor_head_for(Chosen, Ctor, head(Pattern, Match)) :-
    Ctor = ctor(Name, Fields),
    length(Fields, N),
    fresh_args(N, '$p', Args),
    application(Name, Args, Pattern),
    ( Ctor == Chosen -> Match = yes ; Match = no ).

fresh_args(0, _, []).
fresh_args(N, Prefix, [A|As]) :-
    format(atom(A), '~w~d', [Prefix, N]),
    N1 is N - 1,
    fresh_args(N1, Prefix, As).

ctor_value(ctor(Name, Fields), Value) :-
    maplist(simple_value, Fields, Args),
    application(Name, Args, Value).

simple_value('Number', '1').
simple_value('String', '"fuzz"').
simple_value('Bool', 'True').
simple_value(T, '()') :- atom(T), sub_atom(T, 0, _, _, '(List '), !.
simple_value(_, 'fuzz-nominal-payload').

application(Name, [], Name) :- !.
application(Name, Args, Text) :-
    atomic_list_concat(Args, ' ', ArgText),
    format(atom(Text), '(~w ~w)', [Name, ArgText]).

effect_poly_program(Id, TypeForms, Forms) :-
    format(atom(Conduit), 'fuzz-conduit-~d', [Id]),
    format(atom(Step), 'fuzz-step-~d', [Id]),
    random_member(StepEffect, [det, nondet]),
    arrow_for_effect(StepEffect, StepArrow),
    ( StepEffect == det
    -> StepBody = '(+ $x 1)', Expected = 1
    ; StepBody = '(superpose ($x (+ $x 1)))', Expected = 2
    ),
    append([TypeForms,
            [fmt('(: ~w (-[$e]-> (-[$e]-> Number Number) Number Number))',
                 [Conduit]),
             fmt('(= (~w $f $x) ($f $x))', [Conduit]),
             fmt('(: ~w (~w Number Number))', [Step, StepArrow]),
             fmt('(= (~w $x) ~w)', [Step, StepBody]),
             fmt('!(test (length (collapse (~w ~w 4))) ~d)',
                 [Conduit, Step, Expected])]],
           Forms).

% ---------------------------------------------------------------------------
% Mutation layer

mutation_program(Id, TypeForms, Incremental, Fresh) :-
    Variant is Id mod 4,
    mutation_variant(Variant, Id, IncCore, FreshCore),
    append(TypeForms, IncCore, Incremental),
    append(TypeForms, FreshCore, Fresh).

mutation_variant(0, Id, Inc, Fresh) :-
    mutation_name(Id, F),
    Inc = [fmt('(= (~w a) 1)', [F]),
           fmt('!(add-atom &self (= (~w b) 2))', [F]),
           fmt('!(remove-atom &self (= (~w a) 1))', [F]),
           fmt('!(test (collapse (~w $arg)) (2))', [F])],
    Fresh = [fmt('(= (~w b) 2)', [F]),
             fmt('!(test (collapse (~w $arg)) (2))', [F])].
mutation_variant(1, Id, Inc, Fresh) :-
    mutation_name(Id, F),
    Inc = [fmt('(: ~w (-[det]-> Atom Number))', [F]),
           fmt('(= (~w a) 1)', [F]),
           fmt('!(remove-atom &self (: ~w (-[det]-> Atom Number)))', [F]),
           fmt('!(add-atom &self (= (~w a) 2))', [F]),
           fmt('!(test (collapse (~w a)) (1 2))', [F])],
    Fresh = [fmt('(= (~w a) 1)', [F]),
             fmt('(= (~w a) 2)', [F]),
             fmt('!(test (collapse (~w a)) (1 2))', [F])].
mutation_variant(2, Id, Inc, Fresh) :-
    mutation_name(Id, F),
    Inc = [fmt('(: ~w (-[nondet]-> Atom Number))', [F]),
           fmt('(= (~w a) 1)', [F]),
           fmt('(= (~w a) 2)', [F]),
           fmt('!(remove-atom &self (= (~w a) 2))', [F]),
           fmt('!(test (collapse (~w a)) (1))', [F])],
    Fresh = [fmt('(: ~w (-[nondet]-> Atom Number))', [F]),
             fmt('(= (~w a) 1)', [F]),
             fmt('!(test (collapse (~w a)) (1))', [F])].
%A determinism declaration arriving at runtime, after the clause was
%compiled without it: the late declaration must recompile the function
%with its commit, so the incremental result equals the fresh load.
mutation_variant(3, Id, Inc, Fresh) :-
    mutation_name(Id, F),
    Inc = [fmt('(= (~w a) 1)', [F]),
           fmt('!(add-atom &self (: ~w (-[det]-> Atom Number)))', [F]),
           fmt('!(test (collapse (~w a)) (1))', [F])],
    Fresh = [fmt('(: ~w (-[det]-> Atom Number))', [F]),
             fmt('(= (~w a) 1)', [F]),
             fmt('!(test (collapse (~w a)) (1))', [F])].

mutation_name(Id, F) :- format(atom(F), 'fuzz-mut-~d', [Id]).

% ---------------------------------------------------------------------------

fmt(Format, Args, String) :- format(string(String), Format, Args).
