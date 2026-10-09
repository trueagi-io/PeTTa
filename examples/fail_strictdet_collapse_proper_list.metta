; Sound boundary of the output-properness certificate (checker feature 2).
;
; The certificate holds only when EVERY clause of a function provably yields a
; bound proper list. maybe-collect's True clause qualifies (a collapse form),
; but its False clause returns a (List Number) PARAMETER - well-typed, yet a
; declared list type never proves properness (it may arrive unbound). One
; non-certifying clause withdraws the whole certificate, so maybe-collect is
; NOT certified, union-atom is not provably det on (maybe-collect ...), and
; extend-maybe's -[det]-> promise is rejected. The two heads are non-overlapping
; (True vs False), so the rejection is the certificate boundary, not overlap.

(: maybe-collect (-[det]-> Bool (List Number) (List Number)))
(= (maybe-collect True $fallback) (collapse 1))
(= (maybe-collect False $fallback) $fallback)

(: extend-maybe (-[det]-> Bool Number (List Number) (List Number)))
(= (extend-maybe $cond $right $fallback)
   (union-atom
      (maybe-collect $cond $fallback)
      ($right)))

!(test (extend-maybe True 2 (9)) (1 2))
