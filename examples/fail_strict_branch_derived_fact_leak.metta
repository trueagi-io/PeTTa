; The true arm derives $derived from a field that only exists when the match
; succeeds.  That branch-local type must not leak into the fallback and
; discharge accepts-statement?'s argument check there.
(: LeakProof Type)
(: LeakStatement Type)
(: LeakTV Type)
(: LeakAnnotated Type)
(: LeakTuple (-[det]-> LeakProof LeakStatement LeakTV LeakAnnotated))

(: accepts-leak-statement? (-[det]-> LeakStatement Bool))
(= (accepts-leak-statement? $_statement) true)

(: derived-branch-leak? (-[det]-> LeakAnnotated Bool))
(= (derived-branch-leak? $annotated)
   (if (= $annotated (LeakTuple $_proof $statement $_tv))
      (let $derived $statement true)
      (accepts-leak-statement? $derived)))
