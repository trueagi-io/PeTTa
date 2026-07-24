:- use_module(library(thread)).

:- dynamic coordinated_load_state/3.

% Coordinate a keyed load without holding a mutex while user code runs.
% Recursive entry by the owner skips for cycle handling.  Other threads get a
% private queue and receive the owner's exact success, failure, or exception.
claim_coordinated_load(Key, Value, Action) :-
    thread_self(Self),
    with_mutex(coordinated_load_state,
               claim_coordinated_load_locked(Key, Value, Self, Action)).

claim_coordinated_load_locked(Key, Value, _, conflict(Existing)) :-
    coordinated_load_state(Key, Existing, _),
    Existing \== Value, !.
claim_coordinated_load_locked(Key, Value, _, skip) :-
    coordinated_load_state(Key, Value, loaded), !.
claim_coordinated_load_locked(Key, Value, Self, skip) :-
    coordinated_load_state(Key, Value, loading(Self, _)), !.
claim_coordinated_load_locked(Key, Value, Self, wait(Queue)) :-
    retract(coordinated_load_state(Key, Value, loading(Owner, Waiters))),
    Owner \== Self, !,
    message_queue_create(Queue),
    assertz(coordinated_load_state(Key, Value,
                                   loading(Owner, [Queue|Waiters]))).
claim_coordinated_load_locked(Key, Value, Self, load) :-
    assertz(coordinated_load_state(Key, Value, loading(Self, []))).

run_coordinated_load(skip, _, _, _).
run_coordinated_load(wait(Queue), _, _, _) :-
    await_coordinated_load(Queue).
run_coordinated_load(load, Key, Value, Goal) :-
    catch(( once(Goal)
            -> complete_coordinated_load(Key, Value, success)
             ; complete_coordinated_load(Key, Value, failure), fail ),
          Error,
          ( complete_coordinated_load(Key, Value, error(Error)),
            throw(Error) )).

complete_coordinated_load(Key, Value, Outcome) :-
    with_mutex(coordinated_load_state,
               ( retract(coordinated_load_state(Key, Value,
                                                loading(_, Waiters))),
                 ( Outcome == success
                   -> assertz(coordinated_load_state(Key, Value, loaded))
                    ; true ) )),
    forall(member(Queue, Waiters),
           catch(thread_send_message(Queue, Outcome), _, true)).

await_coordinated_load(Queue) :-
    setup_call_cleanup(true,
                       thread_get_message(Queue, Outcome),
                       message_queue_destroy(Queue)),
    coordinated_load_outcome(Outcome).

coordinated_load_outcome(success).
coordinated_load_outcome(failure) :- fail.
coordinated_load_outcome(error(Error)) :- throw(Error).
