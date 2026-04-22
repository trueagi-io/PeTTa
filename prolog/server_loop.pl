% server_loop.pl - PeTTa Prolog server loop
% This file contains the server loop and protocol handling code.
% It is loaded by the Rust engine after all other Prolog files.

server_main :-
    set_stream(user_input, type(binary)),
    set_stream(user_output, type(binary)),
    put_byte(user_output, 255),
    flush_output(user_output),
    server_loop.

server_loop :-
    get_byte(user_input, Type),
    ( Type =:= -1 -> halt
    ; Type =:= 81 -> halt
    ; Type =:= 67 -> handle_cancel
    ; read_u32(Len),
      read_bytes_fast(Len, Bytes),
      string_codes(Query, Bytes),
      execute_query(Type, Query, Results),
      write_response(Results),
      flush_output(user_output),
      server_loop
    ).

handle_cancel :-
    abort.

execute_query(70, Query, Results) :-  % 'F' = load_metta_file
    with_output_to(user_error, catch(load_metta_file(Query, Results), _, Results=[])).
execute_query(83, Query, Results) :-  % 'S' = process_metta_string
    with_output_to(user_error, catch(process_metta_string(Query, Results), _, Results=[])).
execute_query(_, _, Results) :- Results = [].

read_u32(V) :-
    get_byte(user_input,B0), get_byte(user_input,B1),
    get_byte(user_input,B2), get_byte(user_input,B3),
    V is (B0<<24) \/ (B1<<16) \/ (B2<<8) \/ B3.

write_u32(V) :-
    B0 is (V>>24) /\ 255, B1 is (V>>16) /\ 255,
    B2 is (V>>8)  /\ 255, B3 is V /\ 255,
    put_byte(user_output,B0), put_byte(user_output,B1),
    put_byte(user_output,B2), put_byte(user_output,B3).

%Bulk-read payload using efficient tail-recursive loop:
read_bytes_fast(0, []) :- !.
read_bytes_fast(N, Bytes) :-
    read_bytes_tail(N, Bytes).

read_bytes_tail(N, Bytes) :-
    read_bytes_loop(N, [], Rev),
    reverse(Rev, Bytes).

read_bytes_loop(0, Acc, Acc) :- !.
read_bytes_loop(N, Acc, Result) :-
    get_code(user_input, Code),
    ( Code =:= -1 -> !, reverse(Acc, Result)
    ; N1 is N - 1,
      read_bytes_loop(N1, [Code|Acc], Result) ).

%Tail-recursive byte writing:
write_bytes(Codes) :- write_bytes_loop(Codes).

write_bytes_loop([]) :- !.
write_bytes_loop([C|Cs]) :-
    put_byte(user_output, C),
    write_bytes_loop(Cs).

write_response(Results) :-
    length(Results, Count),
    put_byte(user_output, 0),
    write_u32(Count),
    maplist(write_result_str, Results).

write_error_response(Error) :-
    put_byte(user_output, 1),
    ( compound(Error) -> format(atom(Msg), '~q', [Error])
    ; atom(Error)     -> Msg = Error
    ; format(atom(Msg), '~w', [Error]) ),
    atom_codes(Msg, Codes),
    length(Codes, Len),
    write_u32(Len),
    write_bytes(Codes).

%Single-pass result serialization: DCG directly produces codes, skip atom intermediate:
write_result_str(Term) :-
    phrase(swrite_exp(Term), Codes),
    length(Codes, Len),
    write_u32(Len),
    write_bytes(Codes).

%Cached type lookup: avoids repeated match/4 calls for type declarations.
%Types are cached in a list of Fun-TypeChains pairs, invalidated when atoms are added/removed.
get_fun_types(Fun, TypeChains) :-
    catch(nb_getval(fun_types, Cache), _, fail),
    member(Fun-TypeChains, Cache), !.
get_fun_types(Fun, TypeChains) :-
    findall(TypeChain, catch(match('&self', [':', Fun, TypeChain], TypeChain, TypeChain), _, fail), TypeChains),
    ( catch(nb_getval(fun_types, Cache), _, fail)
      -> nb_setval(fun_types, [Fun-TypeChains|Cache])
      ; nb_setval(fun_types, [Fun-TypeChains]) ).

invalidate_type_cache :- nb_setval(fun_types, []).
