% Import log4p
use_module(library(log4p)).

% PeTTa log handler, append timestamped messages to a petta.log file
% under the current folder
petta_log_handler(Level, Msg) :-
    % Get current time
    get_time(T),
    format_time(atom(FT), '%F %T:%3f', T),
    % Format full message as "[DATETIME] [LOGLEVEL] MESSAGE"
    string_upper(Level, ULvl),
    swritef(FullMsg, "[%w] [%w] %w", [FT, ULvl, Msg]),
    % Append full message to petta.log (TODO: should be optimized)
    open('petta.log', append, Stream),
    write(Stream, FullMsg),
    nl(Stream),
    close(Stream).

% Set PeTTa log handler
remove_log_handler(default_log_handler).
add_log_handler(petta_log_handler).

'logger-info'(Msg, []) :-
    log4p:info(Msg).
