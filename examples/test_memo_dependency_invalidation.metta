!(import! &self (library lib_memo))

!(memoize double)
(= (double $x) (+ $x $x))

; First call caches, second uses cache.
!(test (double 5) 10)
!(test (double 5) 10)
