!(import! &self (library lib_memo))

; Aggregate all answers for ground calls into one cached value.
!(config-memoize (aggregate sum))
!(memoize choices)

(= (choices $x) $x)
(= (choices $x) (+ $x 1))
(= (choices $x) (+ $x 2))

; sum(5,6,7) => 18
!(test (choices 5) 18)

; Restore default mode for follow-up runs in the same session.
!(config-memoize (aggregate none))
