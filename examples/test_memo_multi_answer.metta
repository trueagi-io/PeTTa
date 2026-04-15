!(import! &self (library lib_memo))

; Enable memoization for a function that returns multiple answers.
!(memoize choose)

; Two valid answers for the same input.
(= (choose $x)
   $x)
(= (choose $x)
   (Pair $x $x))

; Run twice: second call should come from cached answer-set.
!(println! (choose 7))
!(println! (choose 7))
