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
%
% The logger always appends messsages to log file and never deletes
% it.  It is up to the user to delete the log file.
%
% To install log4p you may need to run the following command
%
% ```
% swipl pack install log4p
% ```
%
% See https://www.swi-prolog.org/pldoc/man?section=pack-install for
% more information.

% Import log4p
:- use_module(library(log4p)).

% NEXT: test overhead and see if it possible to minimize it.

% Declare global variables
:- dynamic filepath/1.		% Path where to write
:- dynamic stdout_flag/1.	% Toggle writing to stdout
:- dynamic timestamp_flag/1.	% Toggle prefixing message with timestamp

% Set global variable defaults
filepath("petta.log").
stdout_flag(false).
timestamp_flag(true).

% Set global variables
set_filepath(NewFP) :-
    retractall(filepath(_)),
    asserta(filepath(NewFP)).
set_stdout(NewSF) :-
    retractall(stdout_flag(_)),
    asserta(stdout_flag(NewSF)).
set_timestamp(NewTF) :-
    retractall(timestamp_flag(_)),
    asserta(timestamp_flag(NewTF)).

% Format message as
%
% "[DATETIME] [LOGLEVEL] MESSAGE"
%
% or
%
% "[LOGLEVEL] MESSAGE"
format_message(Level, Msg, FmtMsg) :-
    % Retrieve timestamp flag
    timestamp_flag(TF),
    % Format message
    (TF
     -> (% Get current time
         get_time(T),
         format_time(atom(FmtT), '%F %T:%3f', T),
         % Format full message as "[DATETIME] [LOGLEVEL] MESSAGE"
         string_upper(Level, ULvl),
         swritef(FmtMsg, "[%w] [%w] %w", [FmtT, ULvl, Msg]))
     ; (% Format full message as "[LOGLEVEL] MESSAGE"
        string_upper(Level, ULvl),
        swritef(FmtMsg, "[%w] %w", [ULvl, Msg]))).

write_to_logfile(FmtMsg) :-
    % Retrieve filepath
    filepath(FP),
    % Append formatted message to log file.  TODO: opening then
    % closing is not optimal, should be optimized.
    (string_length(FP, 0)
     -> true
     ; (open(FP, append, Stream),
        writeln(Stream, FmtMsg),
        close(Stream))).

write_to_stdout(FmtMsg) :-
    % Retrieve stdout_flag
    stdout_flag(SF),
    % Write formatted message to stdout.
    (SF
     -> writeln(FmtMsg)
     ; true).

% PeTTa log handler, append formatted messages to log file.
petta_log_handler(Level, Msg) :-
    % Format message for logging
    format_message(Level, Msg, FmtMsg),
    % Write formatted message to log file
    write_to_logfile(FmtMsg),
    % Write formatted message to stdout
    write_to_stdout(FmtMsg).

% Set PeTTa log handler
:- remove_log_handler(default_log_handler).
:- add_log_handler(petta_log_handler).

%%%%%%%%%%%%%%%
%% MeTTa API %%
%%%%%%%%%%%%%%%

% Get global variables
'logger-filepath'(FP) :-
    filepath(FP).
'logger-stdout'(SF) :-
    stdout_flag(SF).
'logger-timestamp'(TF) :-
    timestamp_flag(TF).

% Set global variables
'logger-set-filepath'(NewFP, true) :-
    set_filepath(NewFP).
'logger-set-stdout'(NewSF, true) :-
    set_stdout(NewSF).
'logger-set-timestamp'(NewTF, true) :-
    set_timestamp(NewTF).

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
