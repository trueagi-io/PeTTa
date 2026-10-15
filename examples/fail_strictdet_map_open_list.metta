; map-atom enumerates list shapes on an open input. A committed wrapper may
; rely on it only through a proper-list boundary proviso, which this call
; violates by passing an unbound value.
(: one (-[det]-> %Undefined% Number))
(= (one $_x) 1)

(: map-open (-[det]-> Expression Expression))
(= (map-open $xs) (map-atom $xs one))

!(map-open $free)
