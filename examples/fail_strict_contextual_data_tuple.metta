!(import! &self (library lib_builtin_types))

; Contextual field checking is an obligation, not an optimistic annotation.
(: wrong-data-pair (-> (Number Number)))
(= (wrong-data-pair)
   (data 1 "not a number"))
