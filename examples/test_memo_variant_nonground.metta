!(import! &self (library lib_memo))

; Enable memoization for a function called with non-ground inputs.
!(memoize shape-kind)

; The output depends only on structure, not concrete variable identity.
(= (shape-kind (Pair $x $y))
   pair)

; Both non-ground calls with different variable names should return pair.
!(test (shape-kind (Pair $a 2)) pair)
!(test (shape-kind (Pair $b 2)) pair)
