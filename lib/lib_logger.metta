;; Logger library to log timestamped messages into a log file.  New
;; messages never replace old messages, instead they are appended to
;; the end of the log file.  The responsability of deleting a log file
;; is left the user.
;;
;; This logger library is based on
;; [log4p](https://www.swi-prolog.org/pack/list?p=log4p).  To install
;; log4p you may need to run the following command:
;;
;; ```
;; swipl pack install log4p
;; ```
;;
;; The logger library contains the following methods:
;;
;; - (logger-filepath) returns a string of the filepath where log
;;   messages are written.  Default: "petta.log".
;;
;; - (logger-stdout) returns True iff messages are written to stdout.
;;   Default: False.
;;
;; - (logger-timestamp) returns True iff messages are prefixed with
;;   date and time.  Default: True.
;;
;; - (logger-set-filepath FILEPATH) sets the destination of the
;;   filepath to FILEPATH, where log messages are written.
;;
;; - (logger-set-stdout FLAG) sets the flag to write log messages to
;;   stdout iff FLAG is True.
;;
;; - (logger-set-timestamp FLAG) sets the flag to prefix log messages
;;   with date and time iff FLAG is True.
;;
;; - (logger-level) returns the current log level.  Only messages are
;;   at this level or higher are logged.  For instance, if the current
;;   log level is info, then messages at levels info, warn, error and
;;   fatal will be logged.  Default: info.
;;
;; - (logger-levels) returns a tuple with all possible log levels,
;;   which are trace, debug, info, warn, error and fatal.
;;
;; - (logger-trace MESSAGE) log MESSAGE at the trace level.
;;
;; - (logger-debug MESSAGE) log MESSAGE at the debug level.
;;
;; - (logger-info MESSAGE) log MESSAGE at the info level.
;;
;; - (logger-warn MESSAGE) log MESSAGE at the warn level.
;;
;; - (logger-error MESSAGE) log MESSAGE at the error level.
;;
;; - (logger-fatal MESSAGE) log MESSAGE at the fatal level.  Note that
;;   this will not halt the process. This responsability is left to
;;   the programmer.
;;
;; - (logger-log LEVEL MESSAGE) log MESSAGE at level LEVEL.
;;
;; - (logger-logf LEVEL MESSAGE ARGUMENTS) log MESSAGE at level LEVEL,
;;   formatting the message with ARGUMENTS according
;;   https://www.swi-prolog.org/pldoc/man?predicate=format/3.
;;
;; - (logger-tracef MESSAGE ARGUMENTS) like logger-trace but expects a
;;   formatted message.
;;
;; - (logger-debugf MESSAGE ARGUMENTS) like logger-debug but expects a
;;   formatted message.
;;
;; - (logger-infof MESSAGE ARGUMENTS) like logger-info but expects a
;;   formatted message.
;;
;; - (logger-warnf MESSAGE ARGUMENTS) like logger-warn but expects a
;;   formatted message.
;;
;; - (logger-errorf MESSAGE ARGUMENTS) like logger-error but expects a
;;   formatted message.
;;
;; - (logger-fatalf MESSAGE ARGUMENTS) like logger-fatal but expects a
;;   formatted message.

;; Import prolog logger library
!(import! &self (library lib_import))
!(import_prolog_functions_from_file (library lib_logger.pl)
                                    (logger-filepath
                                     logger-stdout
                                     logger-timestamp
                                     logger-set-filepath
                                     logger-set-stdout
                                     logger-set-timestamp
                                     logger-level
                                     logger-levels
                                     logger-set-global-level
                                     logger-set-local-level
                                     logger-set-level
                                     logger-trace
                                     logger-debug
                                     logger-info
                                     logger-warn
                                     logger-error
                                     logger-fatal
                                     logger-log
                                     logger-logf
                                     logger-tracef
                                     logger-debugf
                                     logger-infof
                                     logger-warnf
                                     logger-errorf
                                     logger-fatalf))
