; A direct parameter whose declared type is a fixed-width positional tuple
; ((Number Number)) is a proper, nonempty list at runtime once the boundary
; check guarantees it is bound - the only thing 28e87bd objected to when it
; dropped this case from the flat table. So min-atom over the tuple is det.
(: closest (-[det]-> (Number Number) Number))
(= (closest $p) (min-atom $p))

(: widest (-[det]-> (Number Number Number) Number))
(= (widest $t) (max-atom $t))

!(test (closest (3 2)) 2)
!(test (widest (4 9 1)) 9)
