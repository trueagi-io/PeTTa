!(import! &self (library lib_builtin_types))

; The arrow caches BadRow before its alias is available.
(: bad-late-row (-> BadRow))
(: BadRow (Alias (Number String)))

; Late expansion must retain the positional field check.
(= (bad-late-row) (data 1 2))
