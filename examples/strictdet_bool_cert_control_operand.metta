; A bound-Bool certificate reads an operand of and/or/not like a clause body:
; an if whose branches are both bound Bools, or a let whose body is, is a
; bound Bool. pick and zero? are certified that way, so (and (pick $c) True)
; and (not (zero? $n)) are tests, not generators. The bodies are added at
; runtime, outside the file batch, where the certificate is the only proof.
(: pick (-[det]-> Bool Bool))
(= (pick $c) (not (if $c False True)))

(: zero? (-[det]-> Number Bool))
(= (zero? $n) (or (let $m $n (== $m 0)) False))

(: both (-[det]-> Bool Bool))
!(add-atom &self (= (both $c) (and (pick $c) True)))

(: nonzero? (-[det]-> Number Bool))
!(add-atom &self (= (nonzero? $n) (not (zero? $n))))

!(test (both True) True)
!(test (both False) False)
!(test (nonzero? 0) False)
!(test (nonzero? 3) True)
