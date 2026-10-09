; A local carrying only (List Number) type metadata is not thereby bound or a
; proper list. Selection needs a producer output-shape certificate.
(= (list-code ()) 0)
(= (list-code (cons $head $tail)) 1)

(: pass-through (-[det]-> (List Number) (List Number)))
(= (pass-through $xs) $xs)

(: bad-list (-[det]-> (List Number) Number))
(= (bad-list $raw)
   (let $values (pass-through $raw) (list-code $values)))
