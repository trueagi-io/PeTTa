:- ensure_loaded('../src/metta').

:- begin_tests(native_arithmetic).

:- retractall(user:silent(_)), assertz(user:silent(true)).
:- process_metta_string("
(: native-calc (-[det]-> Number Number Number))
(= (native-calc $x $y) (* (+ $x 1) $y))
(: native-inc (-[det]-> Number Number))
(= (native-inc $x) (+ $x 1))
(: native-apply (-[det]-> (-[det]-> Number Number) Number Number))
(= (native-apply $f $x) (* ($f $x) (- $x 1)))
(: native-direct (-[det]-> Number Number))
(= (native-direct $x) (native-apply native-inc $x))
(: native-make (-[det]-> (-[det]-> Number Number)))
(= (native-make) (|-> ($x) (* $x $x)))
(: native-added (-[det]-> Number Number))
",_).

native_mul(PI) :-
    with_output_to(string(VM),vm_list(user:PI)),
    once(sub_string(VM,_,_,_,"a_mul")).

test(ordinary_clause_has_native_arithmetic) :-
    native_mul('native-calc'/3),'native-calc'(2.0,3.0,9.0).

test(source_loading_preserves_host_mode) :- current_prolog_flag(optimise,false).

test(previous_enabled_mode_is_preserved) :-
    setup_call_cleanup(set_prolog_flag(optimise,true),
        (user:assert_compiled_clause(assertz,(native_temp(X,Y):-Y is X*X),Ref),
         current_prolog_flag(optimise,true),erase(Ref)),
        set_prolog_flag(optimise,false)).

test(exception_restores_host_mode) :-
    catch(user:assert_compiled_clause(assertz,42,_),error(_,_),Caught=true),
    Caught==true,current_prolog_flag(optimise,false).

test(placement_and_clause_references_are_preserved) :-
    setup_call_cleanup(
        (user:assert_compiled_clause(assertz,native_order(last),R1),
         user:assert_compiled_clause(asserta,native_order(first),R2)),
        (findall(X,user:native_order(X),Xs),Xs==[first,last],
         clause(user:native_order(first),true,R2)),
        (erase(R1),erase(R2))).

test(recompiled_clause_has_native_arithmetic) :-
    recompile_function_clauses('native-calc'),native_mul('native-calc'/3).

test(lambda_has_native_arithmetic) :-
    'native-make'(F), Goal=..[F,3,R],user:call(Goal),R==9,
    native_mul(F/2).

test(specialization_has_native_arithmetic) :-
    'native-direct'(3,8),ho_specialization('native-apply',Spec),
    native_mul(Spec/3),!.

test(runtime_add_has_native_arithmetic) :-
    'add-atom'('&self',[=,['native-added',X],[*,X,X]],true),
    'native-added'(3,9),native_mul('native-added'/2).

test(failed_add_restores_optimized_clauses) :-
    catch('add-atom'('&self',[=,['native-calc',X,Y],[+,X,Y]],_),
          error(_,determinism),Caught=true),
    Caught==true,'native-calc'(2,3,9),native_mul('native-calc'/3).

test(snapshot_marks_compiled_code_and_replay_keeps_native_arithmetic) :-
    clause('native-calc'(_,_,_),_,Ref),
    load_cache:stored_clause(Ref,compiled(Head,Body),_,[]),
    strip_module(Head,_,PlainHead),
    compound_name_arguments(PlainHead,_,Args),
    compound_name_arguments(Copy,'native-cache-copy',Args),
    setup_call_cleanup(
        load_cache:assert_replayed_clause(user,compiled(Copy,Body)),
        (user:'native-cache-copy'(2,3,9),native_mul('native-cache-copy'/3),
         current_prolog_flag(optimise,false)),
        abolish(user:'native-cache-copy'/3)).

test(raw_prolog_replay_keeps_its_compilation_mode) :-
    setup_call_cleanup(
        load_cache:assert_replayed_clause(user,
            (native_raw(X,Y,Z)-(Z is X*Y))),
        (user:native_raw(2,3,6),\+ native_mul(native_raw/3),
         current_prolog_flag(optimise,false)),
        abolish(user:native_raw/3)).

test(unused_invalid_arithmetic_keeps_its_deferred_error) :-
    setup_call_cleanup(
        user:assert_compiled_clause(assertz,
            (native_deferred(B,R) :- (B==true -> R=ok ; R is nevercalled+42)),Ref),
        (user:native_deferred(true,ok),current_prolog_flag(optimise,false),
         catch(user:native_deferred(false,_),error(type_error(evaluable,_),_),Caught=true),
         Caught==true),
        erase(Ref)).

test(deferred_error_fallback_restores_enabled_host_mode) :-
    setup_call_cleanup(set_prolog_flag(optimise,true),
        setup_call_cleanup(
            user:assert_compiled_clause(assertz,(native_bad(R):-R is nevercalled),Ref),
            current_prolog_flag(optimise,true),
            erase(Ref)),
        set_prolog_flag(optimise,false)).

:- end_tests(native_arithmetic).
