; Head elaboration calls head-choice before entering the clause. Until head
; goals have their own effect proof, an explicit commitment cannot certify it.
(= (head-choice) (superpose (1 2)))

(: guarded (-[det]-> Number Number))
(= (guarded (head-choice)) 0)
