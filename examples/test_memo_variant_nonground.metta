!(import! &self (library lib_memo))

; Enable memoization for a function called with non-ground inputs.
!(memoize shape-kind)

; The output depends only on structure, not concrete variable identity.
(= (shape-kind (Pair $x $y))
   pair)

; Two non-ground calls with different variable names should share a variant key.
!(println! (shape-kind (Pair $a 2)))
!(println! (shape-kind (Pair $b 2)))
