; Explicitness is recursive: the outer function is det, but its closure
; parameter still uses an uncommitted plain arrow.
(: apply-unannotated (-[det]-> (-> Number Number) Number Number))
(= (apply-unannotated $f $x) ($f $x))
