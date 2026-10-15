; Under --strict-det a -[det]-> function that PROVABLY cannot match some input
; is a compile error: every clause pins argument 1 to a literal, no clause has
; a variable there, and Number has infinitely many other values - so
; (lookup 3) yields zero results while the arrow promises exactly one.
; The fix is one word: -[semidet]-> (see strictdet_semidet_arrow.metta), which
; keeps the commit and the last-call optimization.
(: lookup (-[det]-> Number Number))
(= (lookup 1) 10)
(= (lookup 2) 20)

!(test (lookup 1) 10)
