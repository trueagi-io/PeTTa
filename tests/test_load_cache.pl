:- use_module('../src/load_cache').
:- begin_tests(load_cache).

test(execution_identity_is_not_state) :-
    load_cache:runtime_state(state(_, _, _, Flags)),
    assertion(\+ memberchk(pid-_, Flags)),
    assertion(\+ memberchk(system_thread_id-_, Flags)).

test(configuration_is_state) :-
    load_cache:runtime_state(state(_, _, _, Flags)),
    forall(member(Name, [version, threads, double_quotes, unknown, stack_limit]),
           ( current_prolog_flag(Name, Value),
             assertion(memberchk(Name-Value, Flags)) )).

test(changed_configuration_changes_fingerprint) :-
    setup_call_cleanup(
        current_prolog_flag(double_quotes, Original),
        ( set_prolog_flag(double_quotes, atom),
          load_cache:runtime_state(S0), load_cache:state_fingerprint(S0, F0),
          set_prolog_flag(double_quotes, string),
          load_cache:runtime_state(S1), load_cache:state_fingerprint(S1, F1),
          assertion(F0 \== F1) ),
        set_prolog_flag(double_quotes, Original)).

:- end_tests(load_cache).
