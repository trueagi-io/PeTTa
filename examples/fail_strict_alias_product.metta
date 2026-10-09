!(import! &self (library lib_builtin_types))

(: Row (Alias (Number String)))

; The expanded positional expectation rejects the second field.
(: wrong-row (-> Row))
(= (wrong-row) (data 1 2))
