% Logger for MeTTa based on
%
% [log4p](https://www.swi-prolog.org/pack/list?p=log4p)
%
% a SWI-Prolog logger.
%
% By default it logs a message MESSAGE by appending the following line
% to the file petta.log in the current directory
%
% [DATETIME] [LOGLEVEL] MESSAGE
%
% where DATETIME is the current date and time and LOGLEVEL is one the
% following: TRACE, DEBUG, INFO, WARN, ERROR, FATAL.
%
% The logger has a global (across all threads) log level, as well as
% local (per thread) log levels.  The default log level (global and
% local) is INFO, meaning that by default all messages logged at the
% INFO, WARN, ERROR or FATAL level will be logged while the messages
% logged at the TRACE or DEBUG level will be ignored.

% Import log4p
:- use_module(library(log4p)).

% NEXT: support resetting stdout, filepath and timestamp.

% NEXT: test overhead and see if it possible to minimize it.

% NEXT: add lots of comments in lib_logger.metta.

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
