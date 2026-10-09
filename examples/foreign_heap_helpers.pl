:- use_module(library(heaps)).

%PeTTa functions use the final Prolog argument as their result. Hide the heap
%key output so the native minimum lookup has one input and one result.
heap_peek_native(Heap, Value) :- min_of_heap(Heap, _, Value).

%Return the key, payload and residual heap as one positional product so a
%parametric Foreign declaration can expose all three through MeTTa's ordinary
%let/let* destructuring.
heap_pop_native(Heap0, [Priority, Value, Heap]) :-
    get_from_heap(Heap0, Priority, Value, Heap).
