; A recursive Bool result declaration proposes `proper_bool` but does not
; prove it. The base branch passes through a parameter which may be unbound,
; so the greatest-fixed-point pass must remove the candidate from the SCC.
(: recursive-pass-bool (-[det]-> Bool Atom Bool))
(= (recursive-pass-bool $value $flag)
   (if (== $flag Base)
      $value
      (recursive-pass-bool $value $flag)))

(: recursive-pass-gate (-[det]-> Bool Atom Bool))
(= (recursive-pass-gate $value $flag)
   (and (recursive-pass-bool $value $flag) true))

!(recursive-pass-gate true Base)
