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
   ( member(mork, Argv) -> ensure_loaded([parser, translator, specializer, filereader, '../mork_ffi/morkspaces', spaces])
                         ; ensure_loaded([parser, translator, specializer, filereader, spaces])).

%%%%%%%%%% Standard Library for MeTTa %%%%%%%%%%

%%% Representation and parsing conversions: %%%
id(X, X).
repr(Term, R) :- swrite(Term, R).
repra(Term, R) :- term_to_atom(Term, R).
parse(Str, R) :- sread(Str, R).

%%% Arithmetic & Comparison: %%%
%% Binary arithmetic operations (A op B -> R)
:- meta_predicate arithmetic_bin_pred(1, 2).
arithmetic_bin_pred(Op, Args) :- Args = (A, B), R is A op B, call(Op, R).
arithmetic_op('+'(A,B,R), R is A + B) :- nonvar(A), nonvar(B).
arithmetic_op('-'(A,B,R), R is A - B) :- nonvar(A), nonvar(B).
arithmetic_op('*'(A,B,R), R is A * B) :- nonvar(A), nonvar(B).
arithmetic_op('/'(A,B,R), R is A / B) :- nonvar(A), nonvar(B).
arithmetic_op('%'(A,B,R), R is A mod B) :- nonvar(A), nonvar(B).
arithmetic_op(min(A,B,R), R is min(A,B)) :- nonvar(A), nonvar(B).
arithmetic_op(max(A,B,R), R is max(A,B)) :- nonvar(A), nonvar(B).
arithmetic_op(exp(A,R), R is exp(A)) :- nonvar(A).

%% Arithmetic predicates with direct definitions for performance
'+'(A,B,R) :- R is A + B.
'-'(A,B,R) :- R is A - B.
'*'(A,B,R) :- R is A * B.
'/'(A,B,R) :- R is A / B.
'%'(A,B,R) :- R is A mod B.
min(A,B,R) :- R is min(A,B).
max(A,B,R) :- R is max(A,B).
exp(Arg,R) :- R is exp(Arg).

%% Comparison operations
comparison_op('<'(A,B,R), (A<B -> R=true ; R=false)).
comparison_op('>'(A,B,R), (A>B -> R=true ; R=false)).
comparison_op('=='(A,B,R), (A==B -> R=true ; R=false)).
comparison_op('!='(A,B,R), (A==B -> R=false ; R=true)).
comparison_op('='(A,B,R), (A=B -> R=true ; R=false)).
comparison_op('=?'(A,B,R), (\+ \+ A=B -> R=true ; R=false)).
comparison_op('=alpha'(A,B,R), (A =@= B -> R=true ; R=false)).
comparison_op('=@='(A,B,R), (A =@= B -> R=true ; R=false)).
comparison_op('<='(A,B,R), (A =< B -> R=true ; R=false)).
comparison_op('>='(A,B,R), (A >= B -> R=true ; R=false)).

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

%% Mathematical functions - generated from template
math_func_op('pow-math'(A, B, Out), Out is A ** B).
math_func_op('sqrt-math'(A, Out), Out is sqrt(A)).
math_func_op('abs-math'(A, Out), Out is abs(A)).
math_func_op('log-math'(Base, X, Out), Out is log(X) / log(Base)).
math_func_op('trunc-math'(A, Out), Out is truncate(A)).
math_func_op('ceil-math'(A, Out), Out is ceil(A)).
math_func_op('floor-math'(A, Out), Out is floor(A)).
math_func_op('round-math'(A, Out), Out is round(A)).
math_func_op('sin-math'(A, Out), Out is sin(A)).
math_func_op('cos-math'(A, Out), Out is cos(A)).
math_func_op('tan-math'(A, Out), Out is tan(A)).
math_func_op('asin-math'(A, Out), Out is asin(A)).
math_func_op('acos-math'(A, Out), Out is acos(A)).
math_func_op('atan-math'(A, Out), Out is atan(A)).
math_func_op('isnan-math'(A, Out), ( A =:= A -> Out = false ; Out = true )).
math_func_op('isinf-math'(A, Out), ( A =:= 1.0Inf ; A =:= -1.0Inf -> Out = true ; Out = false )).

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
                  format("is ~w, should ~w. ~w ~n", [RA, RB, E]),
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
:- maplist(register_fun, [superpose, empty, let, 'let*', '+','-','*','/', '%', min, max, 'change-state!', 'get-state', 'bind!',
                          '<','>','==', '!=', '=', '=?', '<=', '>=', and, or, xor, implies, not, sqrt, exp, log, cos, sin,
                          'first-from-pair', 'second-from-pair', 'car-atom', 'cdr-atom', 'unique-atom', 'alpha-unique-atom',
                          repr, repra, parse, 'println!', 'readln!', test, assert, 'mm2-exec', atom_concat, atom_chars, copy_term, term_hash,
                          foldl, first, last, append, length, 'size-atom', sort, msort, member, 'is-member', 'exclude-item', list_to_set, maplist, eval, reduce, 'import!',
                          'add-atom', 'remove-atom', 'get-atoms', match, 'is-var', 'is-expr', 'is-space', 'get-mettatype',
                          decons, 'decons-atom', 'py-call', 'get-type', 'get-metatype', '=alpha', concat, sread, cons, reverse,
                          '#+','#-','#*','#div','#//','#mod','#min','#max','#<','#>','#=','#\\=','set_hook',
                          'union-atom', 'cons-atom', 'intersection-atom', 'subtraction-atom', 'index-atom', id,
                          'pow-math', 'sqrt-math', 'sort-atom','abs-math', 'log-math', 'trunc-math', 'ceil-math',
                          'floor-math', 'round-math', 'sin-math', 'cos-math', 'tan-math', 'asin-math','random-int','random-float',
                          'acos-math', 'atan-math', 'isnan-math', 'isinf-math', 'min-atom', 'max-atom',
                          'foldl-atom', 'map-atom', 'filter-atom','current-time','format-time', library, exists_file,
                          import_prolog_function, 'Predicate', callPredicate, assertaPredicate, assertzPredicate, retractPredicate,
                          'add-translator-rule!', 'remove-translator-rule!', argv, lazy_load_library, reload_library, list_loaded_libraries,
                          declare_type, get_declared_type, infer_type]).

'get-error-location'(error(ErrType, context(Location, _)), Location).
'get-error-location'(_, none).

'error-type-mismatch'(Expected, Found, Context) :-
    format(atom(Context), 'Type mismatch: expected ~w, got ~w', [Expected, Found]).

'error-undefined-function'(Name, Arity, Suggestions) :-
    findall(Sim, (current_predicate(Sim/Arity), similarity(Name, Sim, Score), Score > 0.5), Suggestions0),
    list_to_set(Suggestions0, Suggestions).

similarity(S1, S2, Score) :-
    atom_chars(S1, Chars1),
    atom_chars(S2, Chars2),
    longest_common_subsubstring(Chars1, Chars2, LCSLen),
    length(Chars1, Len1),
    length(Chars2, Len2),
    MaxLen is max(Len1, Len2),
    MaxLen > 0,
    Score is LCSLen / MaxLen.

longest_common_subsubstring([], _, 0) :- !.
longest_common_subsubstring(_, [], 0) :- !.
longest_common_subsubstring([H|T1], [H|T2], Len) :- !,
    longest_common_subsubstring(T1, T2, SubLen),
    Len is SubLen + 1.
longest_common_subsubstring([_|T1], [_|T2], Len) :-
    longest_common_subsubstring(T1, T2, Len1),
    longest_common_subsubstring(T1, [_|T2], Len2),
    longest_common_subsubstring([_|T1], T2, Len3),
    Len is max(max(Len1, Len2), Len3).
longest_common_subsubstring(_, _, 0).

'error-syntax'(Location, Detail, Recoverable) :-
    format(atom(Detail), 'Syntax error at ~w', [Location]),
    Recoverable = false.

get_error_type(Err, Type) :- nonvar(Err), Err = error(Type, _), !.
get_error_type(_, unknown).

get_error_context(Err, Ctx) :- nonvar(Err), Err = error(_, Ctx), !.
get_error_context(_, none).
