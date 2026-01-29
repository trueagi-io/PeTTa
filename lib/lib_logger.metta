!(import! &self (library lib_import))
!(import_prolog_functions_from_file (library lib_logger.pl)
                                    (logger-level
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
