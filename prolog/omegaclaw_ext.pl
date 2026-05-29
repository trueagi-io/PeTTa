:- catch(use_module(library(http/websocket)), _, true).
:- catch(use_module(library(readutil)), _, true).

% Lazy initialization - just marks ready, actual connection on first ws_call
init_ws :- true.

ws_connected :-
    nb_current(ws_connection, _), !.

% Lazy WS connection with retry on first call
maybe_init_ws :-
    ws_connected, !.
maybe_init_ws :-
    \+ getenv('WS_PORT', _), !.
maybe_init_ws :-
    getenv('WS_PORT', PortStr), !,
    atom_number(PortStr, Port),
    atomic_list_concat(['ws://127.0.0.1:', Port], URL),
    (   catch(ws_open(URL, WS, []), _, fail)
    ->  nb_setval(ws_connection, WS),
        nb_setval(ws_msg_id, 0),
        !
    ;   sleep(0.5),
        fail
    ),
    !.
maybe_init_ws :-
    sleep(0.5),
    maybe_init_ws.

ws_call(Method, Params, Result) :-
    maybe_init_ws, !,
    ws_connected, !,
    nb_getval(ws_connection, WS),
    nb_getval(ws_msg_id, Id0),
    Id is Id0 + 1,
    nb_setval(ws_msg_id, Id),
    ws_send(WS, _{id:Id, method:Method, params:Params}),
    catch(ws_recv(WS, _{id:Id, result:Result}), _, fail).
ws_call(_, _, []).

'call-llm'(Provider, Prompt, MaxTokens, Response) :-
    ws_call("llm_call", _{provider:Provider, prompt:Prompt, max_tokens:MaxTokens}, Response).

'call-embedding'(Text, Provider, Vector) :-
    ws_call("embed", _{text:Text, provider:Provider}, Vector).

'vector-remember'(Text, Vector, Timestamp, _) :-
    ws_call("vector_remember", _{text:Text, vector:Vector, ts:Timestamp}, _).

'vector-query'(Vector, N, Results) :-
    ws_call("vector_query", _{vector:Vector, n:N}, Results).

'web-search'(Query, Results) :-
    ws_call("web_search", _{query:Query}, Results).

'channel-recv'(Msg) :-
    ws_call("channel_recv", _{}, Msg).

'channel-send'(Msg, _) :-
    ws_call("channel_send", _{msg:Msg}, _).

'balance-parens'(Str, Result) :-
    ws_call("helper_balance_parens", _{str:Str}, Result).

'normalize-string'(Str, Result) :-
    ws_call("helper_normalize_string", _{str:Str}, Result).

'around-time'(Time, K, Result) :-
    ws_call("helper_around_time", _{time:Time, k:K}, Result).

'irc-connect'(Server, Port, Nick, Channel, AuthSecret, _) :-
    ( nonvar(AuthSecret), AuthSecret \== empty -> Secret = AuthSecret
    ; getenv('OMEGACLAW_AUTH_SECRET', Secret) -> true
    ; Secret = '' ),
    ws_call("irc_connect", _{server:Server, port:Port, nick:Nick, channel:Channel, auth_secret:Secret}, _).

'irc-recv'(Msg) :-
    ws_call("irc_recv", _{}, Msg).

'irc-send'(Msg, _) :-
    ws_call("irc_send", _{msg:Msg}, _).

'irc-stop'(_) :-
    ws_call("irc_stop", _{}, _).