;; Test logger library

;; Import logger library
!(import! &self (library lib_logger))

;; Get current filepath
!(test
  (logger-filepath)
  "petta.log")

;; Get current stdout flag
!(test
  (logger-stdout)
  False)

;; Get current timestamp flag
!(test
  (logger-timestamp)
  True)

;; Get current log-level
!(test
  (logger-level)
  info)

;; Get all possible log levels
!(test
  (logger-levels)
  (trace debug info warn error fatal))

;; Set log level to debug
!(test
  (logger-set-level debug)
  True)

;; Get current log-level
!(test
  (logger-level)
  debug)

;; Log message at the trace level (should be ignored)
!(test
  (logger-trace "Log message at the trace level")
  True)

;; Log message at the debug level
!(test
  (logger-debug "Log message at the debug level")
  True)

;; Log message at the info level
!(test
  (logger-info "Log message at the info level")
  True)

;; Log message at the warn level
!(test
  (logger-warn "Log message at the warn level")
  True)

;; Log message at the error level
!(test
  (logger-error "Log message at the error level")
  True)

;; Log message at the fatal level
!(test
  (logger-fatal "Log message at the fatal level")
  True)

;; Log message at the given info level
!(test
  (logger-log info "Log message at the given info level")
  True)

;; Log message containing MeTTa terms at the given info level
!(test
  (logger-logf info
               "Log terms %w, %w and %w at the given info level"
               ((+ 21 21) "Hello World!" (R A B)))
  True)

;; Log message containing MeTTa terms at the info level
!(test
  (logger-infof "Log terms %w, %w and %w at the info level"
                ((+ 21 21) "Hello World!" (R A B)))
  True)

;; Reset log filepath
!(test
  (logger-set-filepath "my-petta.log")
  True)
!(test
  (logger-info "Log message to \"my-petta.log\" file")
  True)

;; Enable logging to stdout
!(test
  (logger-set-stdout True)
  True)
!(test
  (logger-info "Message to stdout as well")
  True)

;; Disable prefixing the message with timestampe
!(test
  (logger-set-timestamp False)
  True)
!(test
  (logger-info "Message without timestamp")
  True)
