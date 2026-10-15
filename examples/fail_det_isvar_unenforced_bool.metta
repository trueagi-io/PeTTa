; The is-var exemption cuts BOTH ways: a parameter the body tests with
; is-var gets no boundary boundness check, so it may arrive unbound - and
; therefore it no longer counts as enforced-bound for the call-site
; strengthenings. (and $b True) on such a parameter is the bool/1 generator
; again, so this body cannot prove -[det]-> and is rejected. Cover the
; unbound case before the boolean, or drop the is-var test.
(: tricky (-[det]-> Bool Bool))
(= (tricky $b) (if (is-var $b) True (and $b True)))

!(tricky True)
