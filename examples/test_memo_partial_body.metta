!(import! &self (library lib_memo))
!(memoize slow-add)

; Counts how often slow-add's body really runs.
!(bind! &runs (new-state 0))
(= (slow-add $x $y)
   (progn (change-state! &runs (+ 1 (get-state &runs)))
          (+ $x $y)))

; add5's body is the partial (slow-add 5): its clause completes the call with
; the extra argument, and that call must go through the memo table too.
(= (add5) (slow-add 5))

!(test (add5 1) 6)
!(test (add5 1) 6)
!(test (slow-add 5 1) 6)
!(test (get-state &runs) 1)
