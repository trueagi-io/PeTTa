:- module(omegaclaw_ext, []).
:- catch(use_module(library(http/websocket)), _, true).
:- catch(use_module(library(readutil)), _, true).

:- initialization(init_ws).

init_ws :-
    catch(do_init_ws, _, true).

do_init_ws :-
    getenv('WS_PORT', PortStr),
    atom_number(PortStr, Port),
    atomic_list_concat(['ws://127.0.0.1:', Port], URL),
    ws_open(URL, WS, []),
    nb_setval(ws_connection, WS),
    nb_setval(ws_msg_id, 0).

ws_connected :-
    nb_current(ws_connection, _).

ws_call(Method, Params, Result) :-
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

'around-time'(Time, MaxLines, Result) :-
    catch((
        read_file_to_string('repos/OmegaClaw-Core/memory/history.metta', Content, []),
        split_string(Content, "\n", "\n", Lines),
        reverse(Lines, RevLines),
        length(RevLines, Total),
        MinTake is min(Total, MaxLines),
        length(TakeList, MinTake),
        append(TakeList, _, RevLines),
        reverse(TakeList, Selected),
        atomic_list_concat(Selected, "\n", Result)
    ), _, Result = "").

'balance-parens'(Str, Str).

'irc-connect'(Server, Port, Nick, Channel, AuthSecret, _) :-
    ( nonvar(AuthSecret), AuthSecret \== (empty) -> Secret = AuthSecret
    ; getenv('OMEGACLAW_AUTH_SECRET', Secret) -> true
    ; Secret = '' ),
    ws_call("irc_connect", _{server:Server, port:Port, nick:Nick, channel:Channel, auth_secret:Secret}, _).

'irc-recv'(Msg) :-
    ws_call("irc_recv", _{}, Msg).

'irc-send'(Msg, _) :-
    ws_call("irc_send", _{msg:Msg}, _).

'irc-stop'(_) :-
    ws_call("irc_stop", _{}, _).
