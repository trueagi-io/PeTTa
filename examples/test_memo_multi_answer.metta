!(import! &self (library lib_memo))

; Enable memoization for a function that returns multiple answers.
!(memoize choose)

; Two valid answers for the same input.
(= (choose $x)
   $x)
(= (choose $x)
   (Pair $x $x))

; First call misses, second hits from cache.
!(test (choose 7) (7 (Pair 7 7)))
!(test (choose 7) (7 (Pair 7 7)))
