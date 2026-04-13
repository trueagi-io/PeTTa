%%%%%%%%%% Dependencies %%%%%%%%%%
library(X, Path) :- library_path(Base), atomic_list_concat([Base, '/', X], Path).
library(X, Y, Path) :- library_path(Base), atomic_list_concat([Base, '/../', X, '/', Y], Path).
:- prolog_load_context(directory, Source),
   directory_file_path(Source, '..', Parent),
   directory_file_path(Parent, 'lib', LibPath),
   asserta(library_path(LibPath)).
:- autoload(library(uuid)).
:- use_module(library(random)).
:- use_module(library(error)).
:- use_module(library(listing)).
:- use_module(library(aggregate)).
:- use_module(library(thread)).
:- use_module(library(lists)).
:- use_module(library(yall), except([(/)/3])).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(process)).
:- use_module(library(filesex)).
:- current_prolog_flag(argv, Argv),
( member(mork, Argv) -> ensure_loaded([parser, utils, translator, specializer, filereader, '../mork_ffi/morkspaces', spaces])
                           ; ensure_loaded([parser, utils, translator, specializer, filereader, spaces])).

%%%%%%%%%% Standard Library for MeTTa %%%%%%%%%%

%%% Representation and parsing conversions: %%%
id(X, X).
repr(Term, R) :- swrite(Term, R).
repra(Term, R) :- term_to_atom(Term, R).
parse(Str, R) :- sread(Str, R).

%%% Arithmetic & Comparison: %%%
%% Arithmetic predicates with direct definitions
'+'(A,B,R) :- R is A + B.
'-'(A,B,R) :- R is A - B.
'*'(A,B,R) :- R is A * B.
'/'(A,B,R) :- R is A / B.
'%'(A,B,R) :- R is A mod B.
min(A,B,R) :- R is min(A,B).
max(A,B,R) :- R is max(A,B).
exp(Arg,R) :- R is exp(Arg).

%% Comparison operations
'<'(A,B,R) :- (A<B -> R=true ; R=false).
'>'(A,B,R) :- (A>B -> R=true ; R=false).
'=='(A,B,R) :- (A==B -> R=true ; R=false).
'!='(A,B,R) :- (A==B -> R=false ; R=true).
'='(A,B,R) :- (A=B -> R=true ; R=false).
'=?'(A,B,R) :- (\+ \+ A=B -> R=true ; R=false).
'=alpha'(A,B,R) :- (A =@= B -> R=true ; R=false).
'=@='(A,B,R) :- (A =@= B -> R=true ; R=false).
'<='(A,B,R) :- (A =< B -> R=true ; R=false).
'>='(A,B,R) :- (A >= B -> R=true ; R=false).

%% CLP(FD) operations
:- use_module(library(clpfd)).
'#+'(A, B, R) :- R #= A + B.
'#-'(A, B, R) :- R #= A - B.
'#*'(A, B, R) :- R #= A * B.
'#div'(A, B, R) :- R #= A div B.
'#//'(A, B, R) :- R #= A // B.
'#mod'(A, B, R) :- R #= A mod B.
'#min'(A, B, R) :- R #= min(A,B).
'#max'(A, B, R) :- R #= max(A,B).
'#<'(A, B, true) :- A #< B, !.
'#<'(_, _, false).
'#>'(A, B, true) :- A #> B, !.
'#>'(_, _, false).
'#='(A, B, true) :- A #= B, !.
'#='(_, _, false).
'#\\='(A, B, true) :- A #\= B, !.
'#\\='(_, _, false).

%% Mathematical functions
'pow-math'(A, B, Out) :- Out is A ** B.
'sqrt-math'(A, Out) :- Out is sqrt(A).
'abs-math'(A, Out) :- Out is abs(A).
'log-math'(Base, X, Out) :- Out is log(X) / log(Base).
'trunc-math'(A, Out) :- Out is truncate(A).
'ceil-math'(A, Out) :- Out is ceil(A).
'floor-math'(A, Out) :- Out is floor(A).
'round-math'(A, Out) :- Out is round(A).
'sin-math'(A, Out) :- Out is sin(A).
'cos-math'(A, Out) :- Out is cos(A).
'tan-math'(A, Out) :- Out is tan(A).
'asin-math'(A, Out) :- Out is asin(A).
'acos-math'(A, Out) :- Out is acos(A).
'atan-math'(A, Out) :- Out is atan(A).
'isnan-math'(A, Out) :- ( A =:= A -> Out = false ; Out = true ).
'isinf-math'(A, Out) :- ( A =:= 1.0Inf ; A =:= -1.0Inf -> Out = true ; Out = false ).
'min-atom'(List, Out) :- min_list(List, Out).
'max-atom'(List, Out) :- max_list(List, Out).

%%% Random Generators: %%%
'random-int'(Min, Max, Result) :- random_between(Min, Max, Result).
'random-int'('&rng', Min, Max, Result) :- random_between(Min, Max, Result).
'random-float'(Min, Max, Result) :- random(R), Result is Min + R * (Max - Min).
'random-float'('&rng', Min, Max, Result) :- random(R), Result is Min + R * (Max - Min).

%%% Boolean Logic: %%%
bool(true).
bool(false).
and(A,B,C) :- bool(A), bool(B), ( A == true -> C = B ; A == false -> C = false ).
or(A,B,C) :- bool(A), bool(B), ( A == true -> C = true ; A == false -> C = B ).
not(A,B) :- bool(A), ( A == true -> B = false ; A == false -> B = true ).
xor(A,B,C) :- bool(A), bool(B), ( A == B -> C = false ; C = true ).
implies(A,B,C) :- bool(A), bool(B), ( A == true -> ( B == true  -> C = true ; B == false -> C = false )
                                                 ; A == false -> C = true ).

%%% Nondeterminism: %%%
superpose(L,X) :- member(X,L).
empty(_) :- fail.

%%% Lists / Tuples: %%%
'cons-atom'(H, T, [H|T]).
'decons-atom'([H|T], [H|[T]]).
'first-from-pair'([A, _], A).
first([A, _], A).
'second-from-pair'([_, A], A).
'unique-atom'(A, B) :- list_to_set(A, B).

%%% Alpha-equivalence unique atom %%%
'alpha-unique-atom'(A, B) :-
    must_be(list, A),
    alpha_list_to_set(A, B).

alpha_list_to_set(List, Set) :-
    empty_assoc(Seen0),
    alpha_list_to_set_assoc(List, Seen0, Set).

alpha_list_to_set_assoc([], _, []).
alpha_list_to_set_assoc([H|T], SeenIn, R) :-
    copy_term(H, HCopy),
    numbervars(HCopy, 0, _),
    term_hash(HCopy, Key),
    ( get_assoc(Key, SeenIn, _) ->
        alpha_list_to_set_assoc(T, SeenIn, R)
    ;
        put_assoc(Key, SeenIn, true, SeenOut),
        R = [H|RT],
        alpha_list_to_set_assoc(T, SeenOut, RT)
    ).

'sort-atom'(List, Sorted) :- msort(List, Sorted).
'size-atom'(List, Size) :- length(List, Size).
'car-atom'([H|_], H).
'cdr-atom'([_|T], T).
decons([H|T], [H|[T]]).
cons(H, T, [H|T]).
'index-atom'(List, Index, Elem) :- nth0(Index, List, Elem).
member(X, L, true) :- member(X, L).
'is-member'(X, List, true) :- member(X, List).
'is-member'(X, List, false) :- \+ member(X, List).
'exclude-item'(A, L, R) :- exclude(==(A), L, R).

%Multisets:
'subtraction-atom'([], _, []).
'subtraction-atom'([H|T], B, Out) :- ( select(H, B, BRest) -> 'subtraction-atom'(T, BRest, Out)
                                                            ; Out = [H|Rest],
                                                              'subtraction-atom'(T, B, Rest) ).
'union-atom'(A, B, Out) :- append(A, B, Out).
'intersection-atom'(A, B, Out) :- intersection(A, B, Out).

%%% Type system: %%%
get_function_type([F|Args], T) :- nonvar(F), match('&self', [':',F,[->|Ts]], _, _),
                                  append(As,[T],Ts),
                                  maplist('get-type',Args,As).

:- dynamic 'get-type'/2.
'get-type'(X, T) :- (get_type_candidate(X, T) *-> true ; T = '%Undefined%' ).
get_type_candidate(X, 'Number')   :- number(X), !.
get_type_candidate(X, _) :- var(X), !.
get_type_candidate(X, 'String')   :- string(X), !.
get_type_candidate(true, 'Bool')  :- !.
get_type_candidate(false, 'Bool') :- !.
get_type_candidate(X, T) :- get_function_type(X,T).
get_type_candidate(X, T) :- \+ get_function_type(X, _),
                            is_list(X),
                            maplist('get-type', X, T).
get_type_candidate(X, T) :- match('&self', [':',X,T], T, _).
'get-metatype'(X, 'Variable') :- var(X), !.
'get-metatype'(X, 'Grounded') :- number(X), !.
'get-metatype'(X, 'Grounded') :- string(X), !.
'get-metatype'(true,  'Grounded') :- !.
'get-metatype'(false, 'Grounded') :- !.
'get-metatype'(X, 'Grounded') :- atom(X), fun(X), !.  % e.g., '+' is a registered fun/1
'get-metatype'(X, 'Expression') :- is_list(X), !.     % e.g., (+ 1 2), (a b)
'get-metatype'(X, 'Symbol') :- atom(X), !.            % e.g., a

'is-var'(A,R) :- var(A) -> R=true ; R=false.
'is-expr'(A,R) :- is_list(A) -> R=true ; R=false.
'is-space'(A,R) :- atom(A), atom_concat('&', _, A) -> R=true ; R=false.

:- dynamic type_declared/2.

declare_type(Fun, TypeChain) :- retractall(type_declared(Fun, _)),
                                assertz(type_declared(Fun, TypeChain)).

get_declared_type(Fun, TypeChain) :- type_declared(Fun, TypeChain), !.
get_declared_type(_, []).

infer_type(Expr, Inferred) :- number(Expr), !, Inferred = 'Number'.
infer_type(Expr, Inferred) :- string(Expr), !, Inferred = 'String'.
infer_type(Expr, Inferred) :- var(Expr), !, Inferred = 'Variable'.
infer_type(Expr, Inferred) :- (Expr == true ; Expr == false), !, Inferred = 'Bool'.
infer_type([Op|Args], Inferred) :- atom(Op), type_declared(Op, [->|TypeChain]),
                                   append(ArgTypes, [OutType], TypeChain),
                                   maplist(infer_type, Args, InferredTypes),
                                   ( InferredTypes = ArgTypes -> Inferred = OutType
                                     ; Inferred = 'Unknown' ).
infer_type([_|Args], Inferred) :- maplist(infer_type, Args, Types),
                                  list_to_set(Types, Unique),
                                  length(Unique, 1), !,
                                  Inferred = Unique.
infer_type(_, Inferred) :- Inferred = 'Unknown'.

%%% Diagnostics / Testing: %%%
'println!'(Arg, true) :- swrite(Arg, RArg),
                         format('~w~n', [RArg]).

'readln!'(Out) :- read_line_to_string(user_input, Str),
                  sread(Str, Out).

test(A,B,true) :- (A =@= B -> E = '✅' ; E = '❌'),
                  swrite(A, RA),
                  swrite(B, RB),
                  format(user_error, "is ~w, should ~w. ~w ~n", [RA, RB, E]),
                  (A =@= B -> true ; halt(1)).

assert(Goal, true) :- ( call(Goal) -> true
                                    ; swrite(Goal, RG),
                                      format("Assertion failed: ~w~n", [RG]),
                                      halt(1) ).

%%% Time Retrieval: %%%
'current-time'(Time) :- get_time(Time).
'format-time'(Format, TimeString) :- get_time(Time), format_time(atom(TimeString), Format, Time).

%%% Python bindings: %%%
'py-call'(SpecList, Result) :- 'py-call'(SpecList, Result, []).
'py-call'([Spec|Args], Result, Opts) :- ( string(Spec) -> atom_string(A, Spec) ; A = Spec ),
                                        must_be(atom, A),
                                        ( sub_atom(A, 0, 1, _, '.')         % ".method"
                                          -> sub_atom(A, 1, _, 0, Fun),
                                             Args = [Obj|Rest],
                                             ( Rest == []
                                               -> compound_name_arguments(Meth, Fun, [])
                                                ; Meth =.. [Fun|Rest] ),
                                             py_call(Obj:Meth, Result, Opts)
                                           ; atomic_list_concat([M,F], '.', A) % "mod.fun"
                                             -> ( Args == []
                                                  -> compound_name_arguments(Call0, F, [])
                                                   ; Call0 =.. [F|Args] ),
                                                py_call(M:Call0, Result, Opts)
                                              ; ( Args == []                      % bare "fun"
                                                  -> compound_name_arguments(Call0, A, [])
                                                   ; Call0 =.. [A|Args] ),
                                                py_call(builtins:Call0, Result, Opts) ).

%%% States: %%%
'bind!'(A, ['new-state', B], C) :- 'change-state!'(A, B, C).
'change-state!'(Var, Value, true) :- nb_setval(Var, Value).
'get-state'(Var, Value) :- nb_getval(Var, Value).

%%% Eval: %%%
eval(C, Out) :- translate_expr(C, Goals, Out),
                call_goals(Goals).

call_goals([]).
call_goals([G|Gs]) :- call(G), 
                      call_goals(Gs).

%%% Higher-Order Functions: %%%
'foldl-atom'([], Acc, _Func, Acc).
'foldl-atom'([H|T], Acc0, Func, Out) :- reduce([Func,Acc0,H], Acc1),
                                        'foldl-atom'(T, Acc1, Func, Out).

'map-atom'([], _Func, []).
'map-atom'([H|T], Func, [R|RT]) :- reduce([Func,H], R),
                                   'map-atom'(T, Func, RT).

'filter-atom'([], _Func, []).
'filter-atom'([H|T], Func, Out) :- ( reduce([Func,H], true) -> Out = [H|RT]
                                                             ; Out = RT ),
                                   'filter-atom'(T, Func, RT).

%%% Prolog interop: %%%
argv(K, Arg) :- current_prolog_flag(argv, Argv), nth0(K, Argv, A), ( atom_number(A, N) -> Arg = N ; Arg = A ).
import_prolog_function(N, true) :- register_fun(N).
'Predicate'([F|Args], Term) :- Term =.. [F|Args].
callPredicate(G, true) :- call(G).
assertzPredicate(G, true) :- assertz(G).
assertaPredicate(G, true) :- asserta(G).
retractPredicate(G, true) :- retract(G), !.
retractPredicate(_, false).

%%% Library / Import: %%%
ensure_metta_ext(Path, Path) :- file_name_extension(_, metta, Path), !.
ensure_metta_ext(Path, PathWithExt) :- file_name_extension(Path, metta, PathWithExt).

'import!'(Space, File, true) :- catch(importer_helper(Space, File), _, fail).
importer_helper(Space, File) :- atom_string(File, SFile),
                                working_dir(Base),
                                ( file_name_extension(ModPath, 'py', SFile)
                                  -> absolute_file_name(SFile, Path, [relative_to(Base)]),
                                     file_directory_name(Path, Dir),
                                     file_base_name(ModPath, ModuleName),
                                     py_call(sys:path:append(Dir), _),
                                     py_call(builtins:'__import__'(ModuleName), _)
                                   ; ( Path = SFile ; atomic_list_concat([Base, '/', SFile], Path) ),
                                     ensure_metta_ext(Path, PathWithExt),
                                     exists_file(PathWithExt), !,
                                     load_metta_file(PathWithExt, _, Space) ).

:- dynamic translator_rule/1.
'add-translator-rule!'(HV, true) :- ( translator_rule(HV)
                                      -> true ; assertz(translator_rule(HV)) ).

'remove-translator-rule!'(HV, true) :- retractall(translator_rule(HV)).

:- dynamic loaded_library/1.

lazy_load_library(Lib) :-
    \+ loaded_library(Lib),
    assertz(loaded_library(Lib)),
    atomic_list_concat(['library/', Lib], LibFile),
    consult(LibFile).

reload_library(Lib) :-
    retractall(loaded_library(Lib)),
    lazy_load_library(Lib).

list_loaded_libraries(Libs) :- findall(Lib, loaded_library(Lib), Libs).

%%% Registration: %%%
:- dynamic fun/1.
register_fun(N) :- (fun(N) -> true ; assertz(fun(N))).
%Pre-assert all builtin functions (avoids maplist/register_fun overhead at startup):
:- assertz(fun(superpose)), assertz(fun(empty)), assertz(fun(let)), assertz(fun('let*')),
   assertz(fun('+')), assertz(fun('-')), assertz(fun('*')), assertz(fun('/')), assertz(fun('%')),
   assertz(fun(min)), assertz(fun(max)), assertz(fun('change-state!')), assertz(fun('get-state')),
   assertz(fun('bind!')),
   assertz(fun('<')), assertz(fun('>')), assertz(fun('==')), assertz(fun('!=')), assertz(fun('=')),
   assertz(fun('=?')), assertz(fun('<=')), assertz(fun('>=')),
   assertz(fun(and)), assertz(fun(or)), assertz(fun(xor)), assertz(fun(implies)), assertz(fun(not)),
   assertz(fun(sqrt)), assertz(fun(exp)), assertz(fun(log)), assertz(fun(cos)), assertz(fun(sin)),
   assertz(fun('first-from-pair')), assertz(fun('second-from-pair')),
   assertz(fun('car-atom')), assertz(fun('cdr-atom')),
   assertz(fun('unique-atom')), assertz(fun('alpha-unique-atom')),
   assertz(fun(repr)), assertz(fun(repra)), assertz(fun(parse)), assertz(fun('println!')),
   assertz(fun('readln!')), assertz(fun(test)), assertz(fun(assert)), assertz(fun('mm2-exec')),
   assertz(fun(atom_concat)), assertz(fun(atom_chars)), assertz(fun(copy_term)), assertz(fun(term_hash)),
   assertz(fun(foldl)), assertz(fun(first)), assertz(fun(last)), assertz(fun(append)),
   assertz(fun(length)), assertz(fun('size-atom')), assertz(fun(sort)), assertz(fun(msort)),
   assertz(fun(member)), assertz(fun('is-member')), assertz(fun('exclude-item')),
   assertz(fun(list_to_set)), assertz(fun(maplist)), assertz(fun(eval)), assertz(fun(reduce)),
   assertz(fun('import!')),
   assertz(fun('add-atom')), assertz(fun('remove-atom')), assertz(fun('get-atoms')),
   assertz(fun(match)), assertz(fun('is-var')), assertz(fun('is-expr')), assertz(fun('is-space')),
   assertz(fun('get-mettatype')),
   assertz(fun(decons)), assertz(fun('decons-atom')), assertz(fun('py-call')),
   assertz(fun('get-type')), assertz(fun('get-metatype')), assertz(fun('=alpha')),
   assertz(fun(concat)), assertz(fun(sread)), assertz(fun(cons)), assertz(fun(reverse)),
   assertz(fun('#+')), assertz(fun('#-')), assertz(fun('#*')), assertz(fun('#div')),
   assertz(fun('#//')), assertz(fun('#mod')), assertz(fun('#min')), assertz(fun('#max')),
   assertz(fun('#<')), assertz(fun('#>')), assertz(fun('#=')), assertz(fun('#\\=')),
   assertz(fun(set_hook)),
   assertz(fun('union-atom')), assertz(fun('cons-atom')), assertz(fun('intersection-atom')),
   assertz(fun('subtraction-atom')), assertz(fun('index-atom')), assertz(fun(id)),
   assertz(fun('pow-math')), assertz(fun('sqrt-math')), assertz(fun('sort-atom')),
   assertz(fun('abs-math')), assertz(fun('log-math')), assertz(fun('trunc-math')),
   assertz(fun('ceil-math')), assertz(fun('floor-math')), assertz(fun('round-math')),
   assertz(fun('sin-math')), assertz(fun('cos-math')), assertz(fun('tan-math')),
   assertz(fun('asin-math')), assertz(fun('random-int')), assertz(fun('random-float')),
   assertz(fun('acos-math')), assertz(fun('atan-math')), assertz(fun('isnan-math')),
   assertz(fun('isinf-math')), assertz(fun('min-atom')), assertz(fun('max-atom')),
   assertz(fun('foldl-atom')), assertz(fun('map-atom')), assertz(fun('filter-atom')),
   assertz(fun('current-time')), assertz(fun('format-time')),
   assertz(fun(library)), assertz(fun(exists_file)),
   assertz(fun(import_prolog_function)), assertz(fun('Predicate')), assertz(fun(callPredicate)),
   assertz(fun(assertaPredicate)), assertz(fun(assertzPredicate)), assertz(fun(retractPredicate)),
   assertz(fun('add-translator-rule!')), assertz(fun('remove-translator-rule!')),
   assertz(fun(argv)), assertz(fun(lazy_load_library)), assertz(fun(reload_library)),
   assertz(fun(list_loaded_libraries)),
   assertz(fun(declare_type)), assertz(fun(get_declared_type)), assertz(fun(infer_type)).

'get-error-location'(error(_ErrType, context(Location, _)), Location).
'get-error-location'(_, none).

'error-type-mismatch'(Expected, Found, Context) :-
    format(atom(Context), 'Type mismatch: expected ~w, got ~w', [Expected, Found]).

'error-undefined-function'(Name, Arity, Suggestions) :-
    findall(Sim, (current_predicate(Sim/Arity), similarity(Name, Sim, Score), Score > 0.5), Suggestions0),
    list_to_set(Suggestions0, Suggestions).

similarity(S1, S2, Score) :-
    atom_chars(S1, Chars1),
    atom_chars(S2, Chars2),
    longest_common_substring(Chars1, Chars2, LCSLen),
    length(Chars1, Len1),
    length(Chars2, Len2),
    MaxLen is max(Len1, Len2),
    ( MaxLen > 0 -> Score is LCSLen / MaxLen ; Score = 0 ).

%Tabling prevents exponential blowup in LCS computation:
:- table longest_common_substring/3.
longest_common_substring([], _, 0) :- !.
longest_common_substring(_, [], 0) :- !.
longest_common_substring([H|T1], [H|T2], Len) :- !,
    longest_common_substring(T1, T2, SubLen),
    Len is SubLen + 1.
longest_common_substring([_|T1], [_|T2], Len) :-
    longest_common_substring(T1, T2, Len1),
    longest_common_substring(T1, [_|T2], Len2),
    longest_common_substring([_|T1], T2, Len3),
    Len is max(max(Len1, Len2), Len3).

'error-syntax'(Location, Detail, Recoverable) :-
    format(atom(Detail), 'Syntax error at ~w', [Location]),
    Recoverable = false.

get_error_type(Err, Type) :- nonvar(Err), Err = error(Type, _), !.
get_error_type(_, unknown).

get_error_context(Err, Ctx) :- nonvar(Err), Err = error(_, Ctx), !.
get_error_context(_, none).
