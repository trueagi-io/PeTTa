:- ensure_loaded('../src/metta').
:- set_prolog_flag(optimise_debug,false).
:- use_module(library(plunit)).
:- set_test_options([load(always)]).

definition_order_test_barrier(Events,Release) :-
    thread_send_message(Events,entered),
    (thread_get_message(Release,resume,[timeout(5)])->true;throw(definition_order_barrier_timeout)).

:- begin_tests(definition_order).

program(Source,Results) :- user:process_metta_string(Source,Results).
fixture(Name,Path) :-
    source_file(plunit_definition_order:program(_,_),Self),file_directory_name(Self,Dir),
    directory_file_path(Dir,'fixtures/definition_order',Fixtures),directory_file_path(Fixtures,Name,Path).
lambda_names(Names) :- findall(F,(user:fun(F),atom(F),sub_atom(F,0,7,_,lambda_)),Raw),sort(Raw,Names).

chain_source(Prefix, N, Source) :-
    findall(Line,
        ( between(1,N,I),
          ( I<N -> J is I+1, format(string(Line),'(= (~w_~d $x) (~w_~d $x))',[Prefix,I,Prefix,J])
          ; format(string(Line),'(= (~w_~d $x) $x)',[Prefix,I]) ) ), Lines),
    atomics_to_string(Lines,"\n",Source).

memo_reader(Kind) :-
    atomic_list_concat([do_probe_leaf_,Kind],Leaf),
    atomic_list_concat([do_probe_top_,Kind],Top),
    setup_call_cleanup(message_queue_create(Events),
      setup_call_cleanup(message_queue_create(Release),
        memo_reader_live(Kind,Leaf,Top,Events,Release),message_queue_destroy(Release)),
      message_queue_destroy(Events)).
memo_reader_live(Kind,Leaf,Top,Events,Release) :-
    program("!(import! &self (library lib_memo))",_),
    OldLeaf=[=,[Leaf,X],1], NewLeaf=[=,[Leaf,X],2],
    Term=[=,[Top,X],[let,V,[Leaf,X],
                     [let,_,[translatePredicate,[definition_order_test_barrier,Events,Release]],V]]],
    user:'add-atom'('&self',OldLeaf,_),user:'add-atom'('&self',Term,_),user:enable_memoization(Top),
    (Kind==ground->Input=5;true),
    setup_call_cleanup(
      thread_create((catch(findall(R,user:cache_call(Top,[Input],R),Old),E,Old=error(E)),
                     thread_send_message(Events,result(Old))),Id,[]),
      (thread_get_message(Events,entered,[timeout(5)]),
       assertion(user:metta_memo_active(Top,2,_)),
       assertion(\+user:metta_memo_entry(Top,_,_,_,_)),
       user:'remove-atom'('&self',OldLeaf,_),user:memo_current_generation(Top,2,Cancelled),
       user:'add-atom'('&self',NewLeaf,_),
       (Kind==ground->user:cache_clear;true),
       assertion(user:memo_current_generation(Top,2,Cancelled)),
       thread_send_message(Release,resume),
       thread_get_message(Events,result(Observed),[timeout(5)]),assertion(Observed==[1])),
      (thread_send_message(Release,resume),thread_join(Id,_))),
    assertion(\+user:metta_memo_entry(Top,_,_,_,_)),
    assertion(\+user:metta_memo_active(Top,_,_)),
    thread_send_message(Release,resume),
    findall(R,user:cache_call(Top,[Input],R),Fresh),assertion(Fresh==[2]).
blocked_reader(Reader,Target,Known,OldExpected) :-
    setup_call_cleanup(message_queue_create(Events),
      setup_call_cleanup(message_queue_create(Release),
                         blocked_reader_live(Reader,Target,Known,OldExpected,Events,Release),
                         message_queue_destroy(Release)),message_queue_destroy(Events)).
blocked_reader_live(Reader,Target,Known,OldExpected,Events,Release) :-
    (Known==true->user:'add-atom'('&self',[=,[Target,X],[+,X,1]],_);true),
    Term=[=,[Reader],[let,_,[translatePredicate,[definition_order_test_barrier,Events,Release]],[Target,3]]],
    setup_call_cleanup(user:'add-atom'('&self',Term,_),
      (Call=..[Reader,R],
       setup_call_cleanup(
         thread_create((catch(findall(R,user:call(Call),Old),Error,Old=error(Error)),
                        thread_send_message(Events,result(Old))),Id,[]),
         (thread_get_message(Events,entered,[timeout(5)]),
          (Known==true->user:'remove-atom'('&self',[=,[Target,Y],[+,Y,1]],_);true),
          user:'add-atom'('&self',[=,[Target,Z],[*,Z,2]],_),
          thread_send_message(Release,resume),
          thread_get_message(Events,result(Observed),[timeout(5)]),assertion(Observed==OldExpected)),
         (thread_send_message(Release,resume),thread_join(Id,_))),
       thread_send_message(Release,resume),FreshCall=..[Reader,V],
       findall(V,user:call(FreshCall),Fresh),assertion(Fresh==[6])),
      user:'remove-atom'('&self',Term,_)).

test(hoisted_calls_have_data_fallback_until_the_equation_arrives) :-
    program("!(do_forward 5) (= (do_forward $x) (+ $x 1)) !(do_forward 5)",Results),
    assertion(Results==[[do_forward,5],6]).

test(eta_expansion_retires_the_provisional_source_arity) :-
    program("(= (do_eta_pair $x $y) (pair $x $y))
             (= (do_eta_make $x) (do_eta_pair $x))
             !((do_eta_make 1) 2)",Results),
    assertion(Results==[[pair,1,2]]),
    assertion(user:arity(do_eta_make,3)),assertion(\+user:arity(do_eta_make,2)).

test(eta_partial_application_through_a_variable_ignores_obsolete_predicates) :-
    program("(= (do_eta_apply $f $x) ($f $x))
             (= (do_eta_indirect_pair $x $y) (pair $x $y))
             (= (do_eta_indirect_make $x) (do_eta_indirect_pair $x))
             !((do_eta_apply do_eta_indirect_make 1) 2)",Results),
    assertion(Results==[[pair,1,2]]).

test(late_multi_arity_import_registers_the_whole_interface_before_refresh) :-
    program("(= (do_import_reader) (do_import_target 1 2)) !(do_import_reader)",Before),
    assertion(Before==[[do_import_target,1,2]]),
    fixture('multi_arity.metta',Path),user:load_metta_file(Path,_),
    program("!(do_import_reader) !(do_import_target 1 2)",After),assertion(After==[3,3]).

test(late_runtime_multi_arity_recovers_a_previously_overapplied_caller) :-
    program("(= (do_runtime_reader) (do_runtime_target 1 2))",_),
    program("!(add-atom &self (= (do_runtime_target $x) $x))",_),
    catch(user:do_runtime_reader(_),_,true),
    program("!(add-atom &self (= (do_runtime_target $x $y) (+ $x $y)))",_),
    program("!(do_runtime_reader)",Results),assertion(Results==[3]).

test(eta_arity_changes_propagate_through_existing_callers) :-
    program("(= (do_transitive_f $x $y) (+ $x $y))
             (= (do_transitive_g $x) (do_transitive_f $x))
             (= (do_transitive_r) (do_transitive_g 1))
             !(do_transitive_r 2)",Before),assertion(Before==[3]),
    program("!(add-atom &self (= (do_transitive_f $x) (+ $x 100)))
             !(do_transitive_g 1) !(do_transitive_r)",After),assertion(After==[true,101,101]).

test(removing_and_restoring_one_arity_changes_complete_and_partial_callers) :-
    program("(= (do_resize_f $x) (+ $x 100)) (= (do_resize_f $x $y) (+ $x $y))
             (= (do_resize_g $x) (do_resize_f $x))
             (= (do_resize_r) (do_resize_g 1)) !(do_resize_r)",Before),assertion(Before==[101]),
    program("!(remove-atom &self (= (do_resize_f $x) (+ $x 100))) !(do_resize_r 2)",Middle),
    assertion(Middle==[true,3]),
    program("!(add-atom &self (= (do_resize_f $x) (+ $x 100))) !(do_resize_r)",After),
    assertion(After==[true,101]).

test(removing_then_readding_a_different_arity_does_not_leave_a_false_complete_call) :-
    program("(= (do_readd_f $x) (+ $x 1)) (= (do_readd_reader $x) (do_readd_f $x))",_),
    program("!(remove-atom &self (= (do_readd_f $x) (+ $x 1)))
             !(add-atom &self (= (do_readd_f $x $y) (+ $x $y)))
             !((do_readd_f 10) 20) !((do_readd_reader 10) 20)",Results),
    assertion(Results==[true,true,30,30]).

test(removed_overapplication_becomes_data_in_existing_and_fresh_calls) :-
    program("(= (do_retired $x) (* $x 2)) (= (do_retired_reader) (do_retired 4 5))",_),
    program("!(remove-atom &self (= (do_retired $x) (* $x 2)))
             !(do_retired_reader) !(do_retired 4 5)",Results),
    assertion(Results==[true,[do_retired,4,5],[do_retired,4,5]]).

test(refresh_preserves_equation_order_and_duplicate_answers) :-
    program("(= (do_order_reader $x) (do_order_target $x))
             (= (do_order_reader $x) (do_order_target $x))",_),
    program("!(add-atom &self (= (do_order_target $x) 2))
             !(add-atom &self (= (do_order_target $x) 1))
             !(add-atom &self (= (do_order_target $x) 2))",_),
    program("!(do_order_reader 7)",Results),assertion(Results==[2,1,2,2,1,2]).

test(refresh_keeps_source_order_when_compiled_arities_merge) :-
    program("(= (do_merge_target $x $y) (pair $x $y))
             (= (do_merge_choice 0) first)
             (= (do_merge_choice 1) (do_merge_target 1))
             (= (do_merge_choice 2) last)
             !(add-atom &self (= (do_merge_target $x) (added $x)))
             !(collapse (do_merge_choice $x))",First),
    assertion(First==[true,[first,[added,1],last]]),
    program("!(remove-atom &self (= (do_merge_target $x) (added $x)))
             !(add-atom &self (= (do_merge_target $x) (added $x)))
             !(collapse (do_merge_choice $x))",Again),
    assertion(Again==[true,true,[first,[added,1],last]]).

test(no_matching_equation_removal_does_not_inspect_a_foreign_procedure) :-
    program("!(import_prolog_function atom_length)",_),
    user:'remove-atom'('&self',[=,[atom_length,_],0],Removed),assertion(Removed==false),
    program("!(atom_length hello)",Results),assertion(Results==[5]).

test(removing_a_user_arity_keeps_the_native_arity_of_the_same_name) :-
    program("(= (length $x $y) (do_native_pair $x $y))",_),
    user:'remove-atom'('&self',[=,[length,X,Y],[do_native_pair,X,Y]],Removed),
    assertion(Removed==true),program("!(length (quote (a b)))",Results),assertion(Results==[2]),
    assertion(user:arity(length,2)),assertion(\+user:arity(length,3)).

test(space_only_reimport_does_not_resurrect_a_removed_function) :-
    fixture('space_only.metta',Path),user:load_metta_file(Path,_),
    user:'remove-atom'('&self',[=,[do_space_only_target,X],[+,X,1]],_),
    user:load_metta_file(Path,_,'&do_data_import'),
    program("!(do_space_only_target 5) !(do_space_only_target 5 6)",Results),
    assertion(Results==[[do_space_only_target,5],[do_space_only_target,5,6]]),
    assertion(\+user:fun(do_space_only_target)),
    assertion(user:match('&do_data_import',[=,[do_space_only_target,4],[+,4,1]],yes,yes)).

test(lambda_dependencies_do_not_remint_the_creator_or_capture_shadowed_parameters) :-
    program("(= (do_lambda_maker $n $x) (|-> ($x) (do_lambda_late $n $x)))",_),
    lambda_names(Before),
    program("!(add-atom &self (= (do_lambda_late $a $b) (+ $a $b)))
             !((do_lambda_maker 5 100) 7)",Results),
    assertion(Results==[true,12]),lambda_names(After),assertion(After==Before).

test(a_zero_capture_lambda_arity_change_keeps_its_creator_and_handle) :-
    program("(= (do_zero_maker) (|-> ($x) (do_zero_target $x)))",_),
    once(user:do_zero_maker(Handle)),lambda_names(Before),
    findall(R,user:translated_from(R,[=,[do_zero_maker],_]),Refs),
    program("!(add-atom &self (= (do_zero_target $x $y) (pair $x $y)))",_),
    once(user:do_zero_maker(Now)),assertion(Now==Handle),
    lambda_names(After),assertion(After==Before),
    findall(R,user:translated_from(R,[=,[do_zero_maker],_]),SameRefs),assertion(SameRefs==Refs),
    findall(R,user:reduce([Handle,1,2],R),Answers),assertion(Answers==[[pair,1,2]]).

test(an_existing_lambda_survives_removal_and_a_new_callee_arity) :-
    program("(= (do_closure_target $x) (+ $x 1))",_),
    user:eval(['|->',[X],[do_closure_target,X]],Closure),
    user:'remove-atom'('&self',[=,[do_closure_target,Y],[+,Y,1]],_),
    findall(R,user:reduce([Closure,4],R),Removed),assertion(Removed==[[do_closure_target,4]]),
    user:'add-atom'('&self',[=,[do_closure_target,A,B],[+,A,B]],_),
    once(user:reduce([Closure,4],Partial)),findall(R,user:reduce([Partial,2],R),After),
    assertion(After==[6]).

test(existing_callers_start_using_new_memo_dispatch) :-
    program("!(import! &self (library lib_memo)) !(bind! do_ticks (new-state 0))
             (= (do_slow $x) (let $ignored (change-state! do_ticks (+ (get-state do_ticks) 1)) (+ $x 1)))
             (= (do_memo_reader $x) (do_slow $x))
             !(memoize do_slow) !(do_memo_reader 5) !(do_memo_reader 5) !(get-state do_ticks)",Results),
    assertion(append(_,[6,6,1],Results)).

test(refresh_invalidates_an_existing_memoized_data_answer) :-
    program("!(import! &self (library lib_memo))
             (= (do_cached_reader $x) (do_cached_late $x))
             !(memoize do_cached_reader) !(do_cached_reader 5)",Before),
    assertion(last(Before,[do_cached_late,5])),
    program("!(add-atom &self (= (do_cached_late $x) (+ $x 1))) !(do_cached_reader 5)",After),
    findall(R,user:do_cached_reader(5,R),Direct),assertion(Direct==[6]),
    assertion(last(After,6)).

test(polymorphic_lambda_call_snapshots_are_compared_up_to_variable_renaming) :-
    program("!(import! &self (library lib_memo))
             (: do_poly_identity (-> $a $a)) (= (do_poly_identity $x) $x)
             (= (do_poly_beta $x) ((|-> ($y) (do_poly_identity $y)) $x))
             !(do_poly_beta 3)",Before),assertion(last(Before,3)),
    lambda_names(Names),program("!(memoize do_poly_identity) !(do_poly_beta 3)",After),
    assertion(last(After,3)),lambda_names(Now),assertion(Now==Names),
    forall(user:translation_use(do_poly_identity,_,Use,Key),
           assertion(user:translation_use_valid(do_poly_identity,Use,Key))).

test(failed_file_drops_only_unprocessed_provisional_arities) :-
    fixture('failed_load.metta',Path),catch(user:load_metta_file(Path,_),Error,true),
    assertion(nonvar(Error)),assertion(Error=error(evaluation_error(zero_divisor),_)),
    program("!(do_failed_kept 1)",Kept),assertion(Kept==[11]),
    program("!(add-atom &self (= (do_failed_unreached $x $y) (+ $x $y)))
             !((do_failed_unreached 10) 20)",After),assertion(After==[true,30]).

test(transaction_rollback_does_not_keep_provisional_arities_or_refreshed_callers) :-
    program("(= (do_rollback_apply $f $x) ($f $x))
             (= (do_rollback_reader $x) (do_rollback_target $x))",_),
    assertion(\+ user:eval([transaction,[let,_,['add-atom','&self',
                  [=,[do_rollback_target,_],99]],[empty]]],_)),
    program("!(add-atom &self (= (do_rollback_target $x $y) (+ $x $y)))
             !((do_rollback_reader 10) 20) !((do_rollback_apply do_rollback_target 10) 20)",Results),
    assertion(Results==[true,30,30]).

test(hoisted_callee_updates_are_visible_at_its_next_entry_in_a_running_clause) :-
    program("!(let $ignored (add-atom &self (= (do_luv_hoisted $x) (* $x 2))) (do_luv_hoisted 3))
             (= (do_luv_hoisted $x) (* $x 3)) !(do_luv_hoisted 3)",Results),
    assertion(Results==[6,6,9]).

test(removing_a_callee_inside_an_active_caller_uses_the_empty_equation_fallback) :-
    program("(= (do_luv_removed $x) (+ $x 1))
             (= (do_luv_remove_and_use)
                (let $ignored (remove-atom &self (= (do_luv_removed $x) (+ $x 1))) (do_luv_removed 5)))
             !(do_luv_remove_and_use)",Results),assertion(Results==[[do_luv_removed,5]]).

test(a_new_name_remains_data_in_the_already_entered_translation) :-
    program("(= (do_luv_define_and_use $x)
                (let $ignored (add-atom &self (= (do_luv_new $v) (+ $v 1))) (do_luv_new $x)))
             !(do_luv_define_and_use 1) !(do_luv_new 1)",Results),
    assertion(Results==[[do_luv_new,1],2]).

test(an_entered_nondeterministic_equation_keeps_its_remaining_answers) :-
    program("(= (do_luv_choice) (superpose (1 2)))",_),
    findall(R,(user:do_luv_choice(R),
        (R==1->user:'remove-atom'('&self',[=,[do_luv_choice],[superpose,[1,2]]],_),
               user:'add-atom'('&self',[=,[do_luv_choice],9],_);true)),Results),
    assertion(Results==[1,2]),program("!(do_luv_choice)",Now),assertion(Now==[9]).

test(quoted_occurrences_do_not_refresh_or_mint_lambdas) :-
    program("(= (do_quoted_value) (quote ((|-> ($x) (do_quoted_late $x)))))",_),
    findall(Ref,user:translated_from(Ref,[=,[do_quoted_value],_]),Before),lambda_names(Names),
    program("!(add-atom &self (= (do_quoted_late $x) (+ $x 1)))",_),
    findall(Ref,user:translated_from(Ref,[=,[do_quoted_value],_]),After),lambda_names(Now),
    assertion(After==Before),assertion(Now==Names).

test(same_arity_remove_readd_does_not_recompile_an_untyped_complete_caller) :-
    program("(= (do_reuse_target $x) (* $x 2)) (= (do_reuse_reader) (do_reuse_target 3))",_),
    findall(Ref,user:translated_from(Ref,[=,[do_reuse_reader],_]),Before),
    program("!(remove-atom &self (= (do_reuse_target $x) (* $x 2)))
             !(add-atom &self (= (do_reuse_target $x) (* $x 3))) !(do_reuse_reader)",Results),
    assertion(Results==[true,true,9]),
    findall(Ref,user:translated_from(Ref,[=,[do_reuse_reader],_]),After),assertion(After==Before).

test(eta_choice_is_independent_of_multi_arity_declaration_order) :-
    program("(= (do_eta_reverse_make $x) (do_eta_reverse_target $x))
             (= (do_eta_reverse_target $x $y $z) (triple $x $y $z))
             (= (do_eta_reverse_target $x $y) (pair $x $y))
             !((do_eta_reverse_make 1) 2)",Reverse),
    assertion(Reverse==[[pair,1,2]]),
    program("(= (do_eta_forward_make $x) (do_eta_forward_target $x))
             (= (do_eta_forward_target $x $y) (pair $x $y))
             (= (do_eta_forward_target $x $y $z) (triple $x $y $z))
             !((do_eta_forward_make 1) 2)",Forward),assertion(Forward==Reverse).

test(a_partial_result_keeps_all_larger_overloads) :-
    program("(= (do_eta_all $x $y) (pair $x $y))
             (= (do_eta_all $x $y $z) (triple $x $y $z))
             (= (do_eta_all_reader $x) (do_eta_all $x))
             !((do_eta_all_reader 1) 2)
             !((do_eta_all_reader 1) 2 3)",Results),
    assertion(Results==[[pair,1,2],[triple,1,2,3]]).

test(adding_source_at_an_existing_native_arity_refreshes_partial_callers) :-
    assertz(user:do_native_eta(X,[one,X])),
    assertz(user:do_native_eta(X,Y,[two,X,Y])),
    program("!(import_prolog_function do_native_eta)
             (= (do_native_eta_reader) (do_native_eta))
             !(do_native_eta_reader 7)",Before),
    assertion(last(Before,[one,7])),
    program("!(add-atom &self (= (do_native_eta $x) (source $x)))
             !((do_native_eta_reader) 7 8)",After),
    assertion(last(After,[two,7,8])).

test(refresh_invalidates_memo_answers_through_an_unmemoized_caller) :-
    program("!(import! &self (library lib_memo))
             (= (do_cache_top $x) (do_cache_mid $x))
             (= (do_cache_mid $x) (do_cache_leaf $x))
             !(memoize do_cache_top) !(do_cache_top 5)",Before),
    assertion(last(Before,[do_cache_leaf,5])),
    program("!(add-atom &self (= (do_cache_leaf $x) (* $x 10)))
             !(do_cache_top 5)",After),
    assertion(last(After,50)).

test(memo_answers_follow_equation_arrival_and_retirement_without_recompilation) :-
    program("!(import! &self (library lib_memo))
             (= (do_cache_wait_top $x) (do_cache_wait_mid $x))
             (= (do_cache_wait_mid $x) (do_cache_wait_leaf $x))
             !(memoize do_cache_wait_top) !(do_cache_wait_top 5)
             (= (do_cache_wait_leaf $x) (* $x 10))
             !(do_cache_wait_top 5)
             !(remove-atom &self (= (do_cache_wait_leaf $x) (* $x 10)))
             !(do_cache_wait_top 5)",Results),
    assertion(append(_,[[do_cache_wait_leaf,5],50,true,[do_cache_wait_leaf,5]],Results)).

test(ground_memo_computation_is_invalidated_before_its_first_answer) :- memo_reader(ground).
test(variant_memo_computation_is_invalidated_before_its_first_answer) :- memo_reader(variant).

test(memo_probe_errors_leave_no_active_observer) :-
    program("!(import! &self (library lib_memo))
             (= (do_probe_error $x) (/ 1 0))",_),
    user:enable_memoization(do_probe_error),
    catch(user:cache_call(do_probe_error,[5],_),Error,true),
    assertion(nonvar(Error)),assertion(\+user:metta_memo_active(do_probe_error,_,_)),
    assertion(\+user:metta_memo_generation(do_probe_error,_,_)).

test(memo_import_and_a_live_unrelated_cache_keep_chain_loading_linear) :-
    program("!(import! &self (library lib_memo))
             !(bind! do_scale_ticks (new-state 0))
             (= (do_scale_cached $x)
                (let $u (change-state! do_scale_ticks (+ (get-state do_scale_ticks) 1)) (+ $x 1)))
             !(memoize do_scale_cached) !(do_scale_cached 5)",_),
    chain_source(do_scale_100,100,S100),chain_source(do_scale_200,200,S200),
    statistics(inferences,I0),program(S100,_),statistics(inferences,I1),
    program(S200,_),statistics(inferences,I2),
    Small is I1-I0, Large is I2-I1,assertion(Large<3*Small),
    assertion(\+ (user:metta_memo_generation(F,_,_),sub_atom(F,0,9,_,do_scale_),F\==do_scale_cached)),
    program("!(do_scale_cached 5) !(get-state do_scale_ticks)",Results),assertion(Results==[6,1]).

test(importing_an_existing_source_function_does_not_make_it_native) :-
    program("(= (do_source_import $x) (+ $x 1)) !(import_prolog_function do_source_import)
             !(remove-atom &self (= (do_source_import $x) (+ $x 1)))
             !(do_source_import 4) !(do_source_import 4 5)",Results),
    assertion(Results==[true,true,[do_source_import,4],[do_source_import,4,5]]),
    assertion(\+user:fun(do_source_import)),assertion(\+user:arity(do_source_import,_)).

test(refresh_does_not_change_an_already_entered_data_decision) :-
    blocked_reader(do_blocked_unknown_reader,do_blocked_new_target,false,[[do_blocked_new_target,3]]).

test(an_already_entered_known_call_sees_the_updated_callee_at_entry) :-
    blocked_reader(do_blocked_known_reader,do_blocked_known_target,true,[6]).

test(a_native_procedure_can_arrive_after_its_name_is_registered) :-
    user:register_fun(do_native_exact),assertz(user:do_native_exact(1,42)),
    findall(R,user:reduce([do_native_exact,1],R),Results),assertion(Results==[42]).

test(native_predicate_frontend_additions_keep_runtime_dispatch_working) :-
    program("!(import_prolog_function do_native_frontend)
             !(assertzPredicate (Predicate (quote (do_native_frontend 1 42))))
             !(reduce (do_native_frontend 1))",Results),assertion(Results==[true,true,42]).

test(late_foreign_import_refreshes_a_preexisting_data_user) :-
    program("(= (do_foreign_reader) (atom_codes word)) !(do_foreign_reader)",Before),
    assertion(Before==[[atom_codes,word]]),
    program("!(import_prolog_function atom_codes) !(do_foreign_reader)",After),
    assertion(After==[true,[119,111,114,100]]).

test(native_implementation_replaces_a_retired_source_arity_without_data_answers) :-
    program("(= (do_native_reopened $x) (+ $x 1))",_),
    user:'remove-atom'('&self',[=,[do_native_reopened,X],[+,X,1]],_),
    assertz(user:do_native_reopened(1,42)),
    user:import_prolog_function(do_native_reopened,true),
    findall(R,user:reduce([do_native_reopened,1],R),Results),assertion(Results==[42]).

test(same_file_caller_above_an_eta_expanding_function_refreshes) :-
    program("(= (do_eta_early_caller) ((do_eta_early_mk 1) 2))
             (= (do_eta_early_mk $x) (do_eta_early_pair $x))
             (= (do_eta_early_pair $x $y) (pair $x $y))
             !(do_eta_early_caller)",Results),
    assertion(Results==[[pair,1,2]]).


test(cyclic_eta_interface_change_is_rejected_and_rolled_back) :-
    program("(= (do_cycle_f $x) (do_cycle_g $x)) !(do_cycle_f 7)",Before),
    assertion(Before==[[do_cycle_g,7]]),
    catch(program("!(add-atom &self (= (do_cycle_g $x $y) (do_cycle_f $x)))",_),Error,true),
    assertion(nonvar(Error)),
    assertion(Error=error(domain_error(finite_application_interface,_),_)),
    assertion(\+user:fun(do_cycle_g)),
    program("!(do_cycle_f 7)",After),
    assertion(After==[[do_cycle_g,7]]).


test(arityless_registered_function_returns_partial_until_arity_is_defined) :-
    user:register_fun(do_arityless_fn),
    program("(= (do_arityless_caller $x) (do_arityless_fn $x))
             (= (do_arityless_eta) (do_arityless_fn 10))
             !(add-atom &self (= (do_arityless_fn $a $b) (+ $a $b)))
             !((do_arityless_caller 10) 20)
             !(do_arityless_eta 20)",Results),
    assertion(Results==[true,30,30]).


test(non_silent_file_load_prints_compiled_clauses_when_caller_refresh_occurs) :-
    setup_call_cleanup(
        ( retractall(user:silent(_)), assertz(user:silent(false)) ),
        with_output_to(string(Printed),
            program("(= (do_loud_caller) ((do_loud_mk 1) 2))
                     (= (do_loud_mk $x) (do_loud_pair $x))
                     (= (do_loud_pair $x $y) (pair $x $y))
                     !(do_loud_caller)",Results)),
        ( retractall(user:silent(_)), assertz(user:silent(true)) )),
    assertion(Results==[[pair,1,2]]),
    assertion(sub_string(Printed,_,_,_,"do_loud_mk")).


test(captured_lambda_arity_change_refreshes_its_eta_expanded_creator) :-
    program("(= (do_capture_make $n) (|-> ($x) (do_capture_late $n $x)))
             !((do_capture_make 1) 2)",Before),
    assertion(Before==[[do_capture_late,1,2]]),
    program("!(add-atom &self (= (do_capture_late $n $x $y) (triple $n $x $y)))
             !(((do_capture_make 1) 2) 3)",After),
    assertion(After==[true,[triple,1,2,3]]).

test(an_added_declaration_reaches_a_caller_compiled_before_it) :-
    program("(= (do_decl_shape $x) (shape $x))
             (= (do_decl_caller) (do_decl_shape (+ 1 2)))
             !(do_decl_caller)
             !(add-atom &self (: do_decl_shape (-> Atom %Undefined%)))
             !(do_decl_caller)
             !(remove-atom &self (: do_decl_shape (-> Atom %Undefined%)))
             !(do_decl_caller)",Results),
    assertion(Results==[[shape,3],true,[shape,[+,1,2]],true,[shape,3]]).

test(an_atom_output_edit_refreshes_new_calls_and_keeps_an_entered_call) :-
    program("(= (do_decl_body $x) (+ $x 1))
             (= (do_decl_body $x) (* $x 2))
             !(collapse (let $result (do_decl_body 2)
                 (let $_ (add-atom &self (: do_decl_body (-> Number Atom))) $result)))
             !(collapse (do_decl_body 2))
             !(remove-atom &self (: do_decl_body (-> Number Atom)))
             !(collapse (do_decl_body 2))", Results),
    assertion(Results == [[3,4], [[+,2,1],[*,2,2]], true, [3,4]]).

test(open_declaration_edits_leave_the_tail_and_space_alone) :-
    program("(: do_decl_existing (-> Number Number))
             !(let $row (cons : (cons do_decl_open $tail))
                 (let $result (catch (add-atom &self $row)) (pair $result (is-var $tail))))
             !(let $row (cons : (cons do_decl_existing $tail))
                 (let $result (catch (remove-atom &self $row)) (pair $result (is-var $tail))))
             !(collapse (match &self (: do_decl_open $t) $t))
             !(collapse (match &self (: do_decl_existing $t) $t))", Results),
    assertion(Results = [[pair, ['Error', instantiation_error, _], true],
                         [pair, ['Error', instantiation_error, _], true],
                         [], [[->, 'Number', 'Number']]]).

test(an_added_translator_rule_reaches_a_caller_compiled_before_it) :-
    program("(= (do_rule_expand $x) (quote (+ $x 1)))
             (= (do_rule_caller) (do_rule_expand 7))
             !(do_rule_caller)
             !(add-translator-rule! do_rule_expand)
             !(do_rule_caller)
             !(remove-translator-rule! do_rule_expand)
             !(do_rule_caller)",Results),
    assertion(Results==[[+,7,1],true,8,true,[+,7,1]]).

%Only a row (: F Type) with the colon and the symbol F declares F. A row with a
%variable in either place is data, and stored and fresh translations agree.
test(rows_with_a_variable_head_or_name_are_not_declarations) :-
    program("(= (do_vtag_id $x) $x)
             (= (do_vtag_caller) (do_vtag_id (+ 1 2)))
             !(add-atom &self ($tag do_vtag_id (-> Atom Atom)))
             !(add-atom &self (: $any (-> Atom Atom)))
             !(do_vtag_caller)
             !(eval (do_vtag_id (+ 1 2)))
             !(remove-atom &self (other_tag do_vtag_id (-> Atom Atom)))
             !(do_vtag_caller)", Results),
    assertion(Results == [true, true, 3, 3, true, 3]).

test(pattern_declaration_removal_refreshes_matched_callers) :-
    program("(: do_patrm_id (-> Atom Atom))
             (= (do_patrm_id $x) $x)
             (= (do_patrm_caller) (do_patrm_id (+ 1 2)))
             !(do_patrm_caller)
             !(remove-atom &self (: $f (-> Atom Atom)))
             !(do_patrm_caller)", Results),
    assertion(Results == [[+, 1, 2], true, 3]).

test(generic_three_variable_removal_does_not_strip_equations) :-
    program("(= (do_genrm_fn $x) (+ $x 1))
             !(let $r (catch (remove-atom &self ($a $b $c)))
                 (pair $r (and (is-var $a) (is-var $b))))
             !(do_genrm_fn 4)", Results),
    assertion(Results = [[pair, ['Error', instantiation_error, _], true], 5]).

test(active_translator_rule_declaration_and_equation_edits_refresh_callers) :-
    program("(= (do_rdecl_r $x) (quote (quote $x)))
             !(add-translator-rule! do_rdecl_r)
             (= (do_rdecl_c) (do_rdecl_r (+ 1 2)))
             !(do_rdecl_c)
             !(add-atom &self (: do_rdecl_r (-> Atom %Undefined%)))
             !(do_rdecl_c)
             !(remove-atom &self (: do_rdecl_r (-> Atom %Undefined%)))
             !(do_rdecl_c)
             !(add-atom &self (= (do_rdecl_r $x) (quote (+ $x 100))))
             !(remove-atom &self (= (do_rdecl_r $x) (quote (quote $x))))
             !(do_rdecl_c)", Results),
    assertion(Results == [true, 3, true, [+, 1, 2], true, 3, true, true, 103]).

test(removing_a_translator_rule_arity_falls_back_cleanly_without_looping) :-
    program("(= (do_rdrop $x) (quote (+ $x 1)))
             (= (do_rdrop $x $y) (quote (+ $x $y)))
             !(add-translator-rule! do_rdrop)
             (= (do_rdrop_c1) (do_rdrop 7))
             (= (do_rdrop_c2) (do_rdrop 3 4))
             !(do_rdrop_c1)
             !(do_rdrop_c2)
             !(remove-atom &self (= (do_rdrop $x) (quote (+ $x 1))))
             !((do_rdrop_c1) 5)
             !(do_rdrop_c2)
             !(remove-atom &self (= (do_rdrop $x $y) (quote (+ $x $y))))
             !(do_rdrop_c1)
             !(do_rdrop 7)
             !(add-atom &self (= (do_rdrop $x) (quote (+ $x 10))))
             !(do_rdrop_c1)", Results),
    assertion(Results == [true, 8, 7, true, [+, 7, 5], 7, true, [do_rdrop, 7], [do_rdrop, 7], true, 17]).

test(translator_rule_api_requires_atom_and_preserves_existing_rules) :-
    program("(= (do_rval $x) (quote (+ $x 1)))
             !(add-translator-rule! do_rval)
             !(let $r (catch (add-translator-rule! $x)) (pair $r (is-var $x)))
             !(let $r (catch (remove-translator-rule! $y)) (pair $r (is-var $y)))
             !(catch (add-translator-rule! 123))
             !(catch (remove-translator-rule! 123))
             !(do_rval 7)", Results),
    assertion(Results = [true,
                         [pair, ['Error', instantiation_error, _], true],
                         [pair, ['Error', instantiation_error, _], true],
                         ['Error', type_error(atom, 123), _],
                         ['Error', type_error(atom, 123), _],
                         8]).

test(eta_expanded_caller_uses_late_memoize_dispatch) :-
    program("!(import! &self (library lib_memo))
             !(bind! do_ememo_ticks (new-state 0))
             (= (do_ememo_slow $x $y)
                (let $_ (change-state! do_ememo_ticks (+ (get-state do_ememo_ticks) 1)) (+ $x $y)))
             (= (do_ememo_wrap $x) (do_ememo_slow $x))
             !(memoize do_ememo_slow)
             !(do_ememo_wrap 3 4)
             !(do_ememo_wrap 3 4)
             !(get-state do_ememo_ticks)", Results),
    assertion(append(_, [7, 7, 1], Results)).

test(declaration_variable_sharing_change_refreshes_callers) :-
    program("(: do_vshare (-> $a $a %Undefined%))
             (= (do_vshare $x $y) (ok $x $y))
             (= (do_vshare_caller) (do_vshare 1 \"hi\"))
             !(collapse (do_vshare_caller))
             !(remove-atom &self (: do_vshare (-> $a $a %Undefined%)))
             !(add-atom &self (: do_vshare (-> $a $b %Undefined%)))
             !(collapse (do_vshare_caller))", Results),
    assertion(Results == [[], true, true, [[ok, 1, "hi"]]]).

test(translator_rule_samples_space_data_at_expansion_and_refreshes_on_program_edit) :-
    program("(do_rdata_flag old)
             (= (do_rdata_rule) (quote (match &self (do_rdata_flag $v) $v)))
             (= (do_rdata_macro) (let $v (match &self (do_rdata_flag $x) $x) (quote $v)))
             !(add-translator-rule! do_rdata_macro)
             (= (do_rdata_caller) (do_rdata_macro))
             !(do_rdata_caller)
             !(remove-atom &self (do_rdata_flag old))
             !(add-atom &self (do_rdata_flag new))
             !(do_rdata_caller)
             !(add-atom &self (= (do_rdata_bump) 1))
             !(do_rdata_caller)", Results),
    assertion(Results == [true, old, true, true, old, true, new]).

test(transaction_rollback_restores_program_generation_and_keeps_reads_valid) :-
    program("(= (do_txgen_r $x) (quote (+ $x 1)))
             !(add-translator-rule! do_txgen_r)
             (= (do_txgen_c) (do_txgen_r 5))
             !(do_txgen_c)
             !(collapse (transaction (let $_ (add-atom &self (= (do_txgen_r $x) (quote (+ $x 99)))) (empty))))
             !(do_txgen_c)", Results),
    assertion(Results == [true, 6, [], 6]),
    forall(user:translation_use(Owner, _, Key, Saved),
           assertion(user:translation_use_valid(Owner, Key, Saved))).

test(variable_tagged_equation_removal_refreshes_callers) :-
    program("(= (do_veq_fn $x) (+ $x 1))
             (= (do_veq_caller) (do_veq_fn 4))
             (= (do_veq_scaller) (do_veq_sfn 4))
             !(do_veq_caller)
             !(let $r (remove-atom &self ($eq (do_veq_fn $x) $b)) (pair $r (is-var $eq)))
             !(do_veq_caller)
             !(let $_ (add-atom &do_veq_space (= (do_veq_sfn $x) (+ $x 2)))
                (let $before (do_veq_scaller)
                  (let $rm (remove-atom &do_veq_space ($eq2 (do_veq_sfn $x) $b))
                    (pair $before (pair $rm (do_veq_scaller))))))", Results),
    assertion(Results == [5, [pair, true, true], [do_veq_fn, 4], [pair, 6, [pair, true, [do_veq_sfn, 4]]]]).

test(refreshed_stored_callers_match_fresh_translations_across_edit_kinds) :-
    program("(= (do_eqv_target $x) (val $x))
             (= (do_eqv_stored) (do_eqv_target (+ 1 2)))", _),
    forall(member(Edit,
                  [ "!(add-atom &self (: do_eqv_target (-> Atom Atom)))",
                    "!(remove-atom &self (: do_eqv_target (-> Atom Atom)))",
                    "!(add-atom &self ($tag do_eqv_target (-> Atom Atom)))",
                    "!(remove-atom &self (other do_eqv_target (-> Atom Atom)))",
                    "!(add-atom &self (: $any (-> Atom Atom)))",
                    "!(remove-atom &self (: do_eqv_target (-> Atom Atom)))",
                    "!(add-atom &self (= (do_eqv_target $x) (quote (+ $x 10))))",
                    "!(remove-atom &self (= (do_eqv_target $x) (val $x)))",
                    "!(add-translator-rule! do_eqv_target)",
                    "!(add-atom &self (: do_eqv_target (-> Atom %Undefined%)))",
                    "!(remove-atom &self (: do_eqv_target (-> Atom %Undefined%)))",
                    "!(remove-atom &self (= (do_eqv_target $x) (quote (+ $x 10))))",
                    "!(add-atom &self (= (do_eqv_target $x) (quote (+ $x 20))))",
                    "!(remove-translator-rule! do_eqv_target)" ]),
           ( program(Edit, _),
             program("!(collapse (do_eqv_stored))", [StoredAns]),
             program("!(collapse (do_eqv_target (+ 1 2)))", [FreshAns]),
             assertion(StoredAns == FreshAns),
             forall(user:translation_use(Owner, _, Key, Saved),
                    assertion(user:translation_use_valid(Owner, Key, Saved))) )).


test(open_tags_are_data_in_the_loader_and_in_add_atom) :-
    program("($tag (do_open_tag_loaded) 7) !(do_open_tag_loaded)", Loaded),
    assertion(Loaded == [[do_open_tag_loaded]]),
    user:'add-atom'('&self', [Tag, [do_open_tag_added], 7], true),
    assertion(var(Tag)),
    assertion(\+ user:fun(do_open_tag_added)),
    user:'add-atom'('&self', [=, [do_open_tag_added], 9], true),
    user:'remove-atom'('&self', [RemoveTag, [do_open_tag_added], 9], true),
    assertion(var(RemoveTag)),
    program("!(do_open_tag_added)", Removed),
    assertion(Removed == [[do_open_tag_added]]).

test(a_rule_for_a_generated_head_refreshes_stored_code) :-
    program("(= (do_generated_caller) (unique (superpose (a b a))))", _),
    findall(X, user:do_generated_caller(X), Before),
    assertion(Before == [a,b]),
    setup_call_cleanup(
        assertz(user:collapse(_, [quote,[rewritten]]), Ref),
        setup_call_cleanup(
            user:'add-translator-rule!'(collapse, true),
            ( program("(= (do_generated_fresh) (unique (superpose (a b a))))", _),
              findall(X, user:do_generated_caller(X), Stored),
              findall(X, user:do_generated_fresh(X), Fresh),
              assertion(Fresh \== []),
              assertion(Fresh \== Before),
              assertion(Stored == Fresh) ),
            user:'remove-translator-rule!'(collapse, true)),
        erase(Ref)).

test(a_removal_that_finds_no_equation_keeps_the_function) :-
    program("(= (do_no_match_kept) 10)", _),
    user:'remove-atom'('&do_no_match_space', [=, [do_no_match_kept], 10], false),
    program("!(do_no_match_kept)", Results),
    assertion(Results == [10]).

test(memoization_keeps_equations_in_their_source_space) :-
    program("!(import! &self (library lib_memo))", _),
    Source = '&do_located_memo_space',
    Equation = [=, [do_located_memo, X], X],
    user:'add-atom'(Source, Equation, true),
    program("!(memoize do_located_memo) !(do_located_memo 4)", Results),
    assertion(Results == [true,4]),
    assertion(user:match(Source, Equation, yes, yes)),
    assertion(\+ user:match('&self', Equation, yes, yes)).

test(a_value_declaration_leaves_a_rules_arguments_evaluated) :-
    program("(: do_rule_value_decl Symbol)
             (= (do_rule_value_decl $x) (quote (+ $x 1)))
             !(add-translator-rule! do_rule_value_decl)
             !(do_rule_value_decl 5)", Results),
    assertion(Results == [true,6]).

test(a_rule_uses_only_declarations_applicable_at_its_arity) :-
    program("(: do_rule_two_arities (-> Atom Expression))
             (= (do_rule_two_arities $x) (quote $x))
             (= (do_rule_two_arities $x $y) (quote (+ $x $y)))
             !(add-translator-rule! do_rule_two_arities)
             !(do_rule_two_arities 3 4)", Results),
    assertion(Results == [true,7]).

:- end_tests(definition_order).
