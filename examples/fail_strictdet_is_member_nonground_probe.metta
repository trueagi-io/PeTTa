; A nonvar probe is not enough for deterministic membership.  The structured
; probe can bind $x twice against these distinct ground elements.
(: structured-member? (-[det]-> Bool))
(= (structured-member?)
   (is-member (f $x) ((f a) (f b))))
