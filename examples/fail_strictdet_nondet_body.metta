; A superpose body contradicts an explicit -[det]-> commitment.
(: sneaky (-[det]-> Number Number))
(= (sneaky $n) (superpose (1 $n)))
!(test (collapse (sneaky 5)) (1 5))
