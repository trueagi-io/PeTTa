%%%%%%%%%% Dependencies %%%%%%%%%%
library(X, Path) :- standard_library_path(Base),
                    directory_file_path(Base, X, Path).
library(X, Y, Path) :- git_library_path(X, Base),
                       directory_file_path(Base, Y, Path).
:- prolog_load_context(directory, Source),
   directory_file_path(Source, '..', Parent),
   directory_file_path(Parent, 'lib', LibPath),
   asserta(standard_library_path(LibPath)).
:- autoload(library(uuid)).
:- use_module(library(crypto)).
:- use_module(library(random)).
:- use_module(library(janus)).
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
   ( member(mork, Argv) -> ensure_loaded([ext_points, parser, translator, specializer, filereader, gitimport, '../mork_ffi/morkspaces', spaces])
                         ; ensure_loaded([ext_points, parser, translator, specializer, filereader, gitimport, spaces])).

%%%%%%%%%% Standard Library for MeTTa %%%%%%%%%%

%%% Representation and parsing conversions: %%%
id(X, X).
repr(Term, R) :- swrite(Term, R).
repra(Term, R) :- term_to_atom(Term, R).
parse(Str, R) :- sread(Str, R).

%%% Arithmetic & Comparison: %%%
'+'(A,B,R)  :- R is A + B.
'-'(A,B,R)  :- R is A - B.
'*'(A,B,R)  :- R is A * B.
'/'(A,B,R)  :- R is A / B.
'%'(A,B,R)  :- R is A mod B.
'<'(A,B,R)  :- (A<B -> R=true ; R=false).
'>'(A,B,R)  :- (A>B -> R=true ; R=false).
'=='(A,B,R) :- (A==B -> R=true ; R=false).
'!='(A,B,R) :- (A==B -> R=false ; R=true).
'='(A,B,R) :-  (A=B -> R=true ; R=false).
'=?'(A,B,R) :- (\+ \+ A=B -> R=true ; R=false).
'=alpha'(A,B,R) :- (A =@= B -> R=true ; R=false).
'=@='(A,B,R) :- (A =@= B -> R=true ; R=false).
'<='(A,B,R) :- (A =< B -> R=true ; R=false).
'>='(A,B,R) :- (A >= B -> R=true ; R=false).
min(A,B,R)  :- R is min(A,B).
max(A,B,R)  :- R is max(A,B).
exp(Arg,R) :- R is exp(Arg).
:- use_module(library(clpfd)).
'#+'(A, B, R) :- R #= A + B.
'#-'(A, B, R) :- R #= A - B.
'#*'(A, B, R) :- R #= A * B.
'#div'(A, B, R) :- R #= A div B.
'#//'(A, B, R) :- R #= A // B.
'#mod'(A, B, R) :- R #= A mod B.
'#min'(A, B, R) :- R #= min(A,B).
'#max'(A, B, R) :- R #= max(A,B).
'#<'(A, B, true)  :- A #< B, !.
'#<'(_, _, false).
'#>'(A, B, true)  :- A #> B, !.
'#>'(_, _, false).
'#='(A, B, true)  :- A #= B, !.
'#='(_, _, false).
'#\\='(A, B, true)  :- A #\= B, !.
'#\\='(_, _, false).
'pow-math'(A, B, Out) :- Out is A ** B.
'sqrt-math'(A, Out)   :- Out is sqrt(A).
'abs-math'(A, Out)    :- Out is abs(A).
'log-math'(Base, X, Out) :- Out is log(X) / log(Base).
'trunc-math'(A, Out)  :- Out is truncate(A).
'ceil-math'(A, Out)   :- Out is ceil(A).
'floor-math'(A, Out)  :- Out is floor(A).
'round-math'(A, Out)  :- Out is round(A).
'sin-math'(A, Out)  :- Out is sin(A).
'cos-math'(A, Out)  :- Out is cos(A).
'tan-math'(A, Out)  :- Out is tan(A).
'asin-math'(A, Out) :- Out is asin(A).
'acos-math'(A, Out) :- Out is acos(A).
'atan-math'(A, Out) :- Out is atan(A).
'isnan-math'(A, Out) :- ( A =:= A -> Out = false ; Out = true ).
'isinf-math'(A, Out) :- ( ( A =:= 1.0Inf ; A =:= -1.0Inf ) -> Out = true ; Out = false ).
'min-atom'(List, Out) :- non_list(List), !, Out = [].
'min-atom'(List, Out) :- min_list(List, Out).
'max-atom'(List, Out) :- non_list(List), !, Out = [].
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
'unique-atom'(A, B) :- non_list(A), !, B = [].
'unique-atom'(A, B) :- list_to_set(A, B).

%%% Alpha-equivalence unique atom %%%
'alpha-unique-atom'(A, B) :- non_list(A), !, B = [].
'alpha-unique-atom'(A, B) :- alpha_list_to_set(A, B).

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

%A term that can never become a list, no matter how it gets instantiated:
non_list(X) :- atomic(X), X \== [].
non_list(X) :- compound(X), X \= [_|_].

'sort-atom'(List, Sorted) :- non_list(List), !, Sorted = [].
'sort-atom'(List, Sorted) :- msort(List, Sorted).
'size-atom'(List, Size) :- non_list(List), !, Size = [].
'size-atom'(List, Size) :- length(List, Size).
'car-atom'([H|_], H) :- !.
'car-atom'(_, []).
'cdr-atom'([_|T], T) :- !.
'cdr-atom'(_, []).
decons([H|T], [H|[T]]).
cons(H, T, [H|T]).
'index-atom'(_, Index, _) :- nonvar(Index), \+ integer(Index), !, fail.
'index-atom'(List, Index, Elem) :- nth0(Index, List, Elem).
member(X, L, true) :- member(X, L).
'is-member'(X, List, true) :- member(X, List).
'is-member'(X, List, false) :- \+ member(X, List).

member_alpha(X, [H|_]) :- (var(X) -> var(H) ; true), X = H, !.
member_alpha(X, [_|T]) :- member_alpha(X, T).

'is-alpha-member'(X, List, true) :- member_alpha(X, List), !.
'is-alpha-member'(_, _, false).

'exclude-item'(A, L, R) :- exclude(==(A), L, R).

%Multisets:
'subtraction-atom'([], _, []).
'subtraction-atom'([H|T], B, Out) :- ( select(H, B, BRest) -> 'subtraction-atom'(T, BRest, Out)
                                                            ; Out = [H|Rest],
                                                              'subtraction-atom'(T, B, Rest) ).
'union-atom'(A, B, Out) :- append(A, B, Out).
'intersection-atom'(A, B, Out) :- ( non_list(A) ; non_list(B) ), !, Out = [].
'intersection-atom'([], _, []).
'intersection-atom'([H|T], B, Out) :- ( select(H, B, BRest) -> Out = [H|Rest],
                                                              'intersection-atom'(T, BRest, Rest)
                                                            ; 'intersection-atom'(T, B, Out) ).

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
'is-ground'(A,R) :- ground(A) -> R=true ; R=false.
'is-expr'(A,R) :- is_list(A) -> R=true ; R=false.
'is-space'(A,R) :- atom(A), atom_concat('&', _, A) -> R=true ; R=false.

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
% janus converts Python booleans to @(true)/@(false); normalize them to the
% language booleans so py-call results compose with if, and, or, ==.
py_bool_norm('@'(true), true) :- !.
py_bool_norm('@'(false), false) :- !.
py_bool_norm(R, R).
'py-call'(SpecList, Result) :- 'py-call'(SpecList, Result, []).
'py-call'([Spec|Args], Result, Opts) :- ( string(Spec) -> atom_string(A, Spec) ; A = Spec ),
                                        must_be(atom, A),
                                        ( sub_atom(A, 0, 1, _, '.')         % ".method"
                                          -> sub_atom(A, 1, _, 0, Fun),
                                             Args = [Obj|Rest],
                                             ( py_is_object(Obj)            % on a Python object reference
                                               -> ( Rest == []
                                                    -> compound_name_arguments(Meth, Fun, [])
                                                     ; Meth =.. [Fun|Rest] ),
                                                  py_call(Obj:Meth, R0, Opts), py_bool_norm(R0, Result)
                                                ; py_call(builtins:type(Obj), Ty), % on a converted value (str, int, ...)
                                                  Call =.. [Fun, Obj|Rest],
                                                  py_call(Ty:Call, R0, Opts), py_bool_norm(R0, Result) )
                                           ; atomic_list_concat([M,F], '.', A) % "mod.fun"
                                             -> ( Args == []
                                                  -> compound_name_arguments(Call0, F, [])
                                                   ; Call0 =.. [F|Args] ),
                                                py_call(M:Call0, R0, Opts), py_bool_norm(R0, Result)
                                              ; ( Args == []                      % bare "fun"
                                                  -> compound_name_arguments(Call0, A, [])
                                                   ; Call0 =.. [A|Args] ),
                                                py_call(builtins:Call0, R0, Opts), py_bool_norm(R0, Result) ).

%%% States: %%%
'bind!'(A, ['new-state', B], C) :- 'change-state!'(A, B, C).
'change-state!'(Var, Value, true) :- nb_setval(Var, Value).
'get-state'(Var, Value) :- nb_getval(Var, Value).

%%% Eval: %%%
eval(C, Out) :- translate_runnable_expr(C, Goals, Out),
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

current_working_dir(Base) :- working_dir(Base), !.
current_working_dir(Base) :- absolute_file_name('.', Base, [file_type(directory)]).

import_file_string(File, SFile) :- string(File), !, SFile = File.
import_file_string(File, SFile) :- atom_string(File, SFile).

python_import_file(File) :- import_file_string(File, SFile),
                            file_name_extension(_, py, SFile).

resolve_existing_import_path(Base, RequestedPath, CanonPath) :-
    ( is_absolute_file_name(RequestedPath)
      -> absolute_file_name(RequestedPath, CanonPath,
                            [access(read), file_errors(fail)])
       ; absolute_file_name(RequestedPath, CanonPath,
                            [relative_to(Base), access(read), file_errors(fail)]) ),
    !.

throw_missing_import(File) :-
    throw(error(existence_error(source_sink, File), context('import!', File))).

resolve_metta_import_path(File, CanonPath) :-
    import_file_string(File, SFile),
    \+ python_import_file(SFile),
    current_working_dir(Base),
    ensure_metta_ext(SFile, RequestedPath),
    ( resolve_existing_import_path(Base, RequestedPath, CanonPath)
      -> true
       ; throw_missing_import(File) ).

resolve_python_import_path(File, CanonPath) :-
    import_file_string(File, SFile),
    python_import_file(SFile),
    current_working_dir(Base),
    ( resolve_existing_import_path(Base, SFile, CanonPath)
      -> true
       ; throw_missing_import(File) ).

:- dynamic imported_metta_source/2.

% Assert before loading to break cycles; retain on success and retract on failure.
% The recursive metta_loader mutex serializes the entire loader graph.
import_once(Space, CanonPath, Goal) :-
    ( imported_metta_source(Space, CanonPath)
      -> true
       ; assertz(imported_metta_source(Space, CanonPath)),
         run_new_import(Space, CanonPath, Goal) ).

run_new_import(Space, CanonPath, Goal) :-
    catch(( once(Goal)
            -> true
             ; retractall(imported_metta_source(Space, CanonPath)), fail ),
          Error,
          ( retractall(imported_metta_source(Space, CanonPath)),
            throw(Error) )).

python_module_names(CanonPath, ModuleKey, ModuleName) :-
    crypto_data_hash(CanonPath, Hash, [algorithm(sha256)]),
    atom_concat('_petta_import_', Hash, ModuleKey),
    file_base_name(CanonPath, BaseName),
    file_name_extension(ModuleName, _, BaseName).

load_python_source(CanonPath) :-
    python_module_names(CanonPath, ModuleKey, ModuleName),
    py_call(sys:modules:get(ModuleName), PreviousModule),
    py_call(importlib:util:spec_from_file_location(ModuleKey, CanonPath), Spec),
    py_call(importlib:util:module_from_spec(Spec), Module),
    py_call(sys:modules:'__setitem__'(ModuleKey, Module), _),
    py_call(sys:modules:'__setitem__'(ModuleName, Module), _),
    catch(py_call(Spec:loader:exec_module(Module), _),
          Error,
          ( restore_python_module(ModuleKey, ModuleName, PreviousModule),
            throw(Error) )).

restore_python_module(ModuleKey, ModuleName, PreviousModule) :-
    catch(py_call(sys:modules:pop(ModuleKey), _), _, true),
    ( PreviousModule == @(none)
      -> catch(py_call(sys:modules:pop(ModuleName), _), _, true)
       ; py_call(sys:modules:'__setitem__'(ModuleName, PreviousModule), _) ).

'import!'(Space, File, true) :- importer_helper(Space, File).
importer_helper(Space, File) :-
    ( python_import_file(File)
      -> resolve_python_import_path(File, CanonPath),
         import_once('$python', CanonPath, load_python_source(CanonPath))
       ; resolve_metta_import_path(File, CanonPath),
         import_once(Space, CanonPath,
                     load_imported_metta_file(CanonPath, _, Space)) ).

:- dynamic translator_rule/1.
'add-translator-rule!'(HV, true) :- ( translator_rule(HV)
                                      -> true ; assertz(translator_rule(HV)) ).

'remove-translator-rule!'(HV, true) :- retractall(translator_rule(HV)).

%%% Registration: %%%
:- dynamic fun/1, arity/2.
register_fun(N) :- fun(N), !.
register_fun(N) :- assertz(fun(N)),
                   forall((current_predicate(N/Arity), \+ (current_op(_, _, N), Arity =< 2)),
                          (arity(N, Arity) -> true ; assertz(arity(N, Arity)))),
                   repair_after_late_registration(N).
:- maplist(register_fun, [superpose, empty, let, 'let*', '+','-','*','/', '%', min, max, 'change-state!', 'get-state', 'bind!',
                          '<','>','==', '!=', '=', '=?', '<=', '>=', and, or, xor, implies, not, sqrt, exp, log, cos, sin,
                          'first-from-pair', 'second-from-pair', 'car-atom', 'cdr-atom', 'unique-atom', 'alpha-unique-atom',
                          repr, repra, parse, 'println!', 'readln!', test, assert, 'mm2-exec', atom_concat, atom_chars, copy_term, term_hash,
                          foldl, first, last, append, length, 'size-atom', sort, msort, member, 'is-member', 'is-alpha-member', 'exclude-item', list_to_set, maplist, eval, reduce, 'import!', 'git-import!',
                          'add-atom', 'remove-atom', 'get-atoms', match, 'is-var', 'is-ground', 'is-expr', 'is-space', 'get-mettatype',
                          decons, 'decons-atom', 'py-call', 'get-type', 'get-metatype', '=alpha', concat, sread, cons, reverse,
                          '#+','#-','#*','#div','#//','#mod','#min','#max','#<','#>','#=','#\\=','set_hook',
                          'union-atom', 'cons-atom', 'intersection-atom', 'subtraction-atom', 'index-atom', id,
                          'pow-math', 'sqrt-math', 'sort-atom','abs-math', 'log-math', 'trunc-math', 'ceil-math',
                          'floor-math', 'round-math', 'sin-math', 'cos-math', 'tan-math', 'asin-math','random-int','random-float',
                          'acos-math', 'atan-math', 'isnan-math', 'isinf-math', 'min-atom', 'max-atom',
                          'foldl-atom', 'map-atom', 'filter-atom','current-time','format-time', library, exists_file,
                          import_prolog_function, 'Predicate', callPredicate, assertaPredicate, assertzPredicate, retractPredicate,
                          'add-translator-rule!', 'remove-translator-rule!', argv]).
