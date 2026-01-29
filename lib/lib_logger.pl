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
:- remove_log_handler(default_log_handler).
:- add_log_handler(petta_log_handler).

% Get current log level, one of trace, debug, info, warn, error and
% fatal.
'logger-level'(LogLvl) :-
    log4p:get_log_level(LogLvl).

% Get all possible log levels, which are trace, debug, info, warn,
% error and fatal.
'logger-levels'(LogLvls) :-
    log4p:log_levels(LogLvls).

% Change log level globally (across all threads).  Valid log levels
% are trace, debug, info, warn, error and fatal.
'logger-set-global-level'(NewLvl, true) :-
    log4p:set_global_log_level(NewLvl).

% Change log level locally (only the current threads).  Valid log
% levels are trace, debug, info, warn, error and fatal.
'logger-set-local-level'(NewLvl, true) :-
    log4p:set_local_log_level(NewLvl, true).
'logger-set-level'(NewLvl, true) :-
    log4p:set_local_log_level(NewLvl).

% Log a message at the trace level
'logger-trace'(Msg, true) :-
    log4p:trace(Msg).

% Log a message at the debug level
'logger-debug'(Msg, true) :-
    log4p:debug(Msg).

% Log a message at the info level
'logger-info'(Msg, true) :-
    log4p:info(Msg).

% Log a message at the warn level
'logger-warn'(Msg, true) :-
    log4p:warn(Msg).

% Log a message at the error level
'logger-error'(Msg, true) :-
    log4p:error(Msg).

% Log a message at the fatal level.  This will NOT halt the process.
% This responsability is left to the programmer.
'logger-fatal'(Msg, true) :-
    log4p:fatal(Msg).

% Log a message at the given level.
'logger-log'(LogLvl, Msg, true) :-
    log4p:log(LogLvl, Msg).

% Log a formatted message at the given level.  The format
% specification can be found
% [here](https://www.swi-prolog.org/pldoc/man?predicate=format/3)
'logger-logf'(LogLvl, Msg, Args, true) :-
    maplist(repr, Args, Strs),
    log4p:logf(LogLvl, Msg, Strs).

% Log a formatted message at the trace level
'logger-tracef'(Msg, Args, true) :-
    maplist(repr, Args, Strs),
    log4p:logf(trace, Msg, Strs).

% Log a message at the debug level
'logger-debugf'(Msg, Args, true) :-
    maplist(repr, Args, Strs),
    log4p:logf(debug, Msg, Strs).

% Log a message at the info level
'logger-infof'(Msg, Args, true) :-
    maplist(repr, Args, Strs),
    log4p:logf(info, Msg, Strs).

% Log a message at the warn level
'logger-warnf'(Msg, Args, true) :-
    maplist(repr, Args, Strs),
    log4p:logf(warn, Msg, Strs).

% Log a message at the error level
'logger-errorf'(Msg, Args, true) :-
    maplist(repr, Args, Strs),
    log4p:logf(error, Msg, Strs).

% Log a message at the fatal level.  This will NOT halt the process.
% This responsability is left to the programmer.
'logger-fatalf'(Msg, Args, true) :-
    maplist(repr, Args, Strs),
    log4p:logf(fatal, Msg, Strs).
