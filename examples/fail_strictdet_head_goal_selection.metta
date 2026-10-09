(= (head-choice) (superpose (1 2)))
(= (guarded-head (head-choice)) 0)

(: consumes-head-proof (-[det]-> Number))
(= (consumes-head-proof) (guarded-head $unknown))
