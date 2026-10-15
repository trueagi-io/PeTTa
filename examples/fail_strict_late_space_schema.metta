(: put-late-row (-> %Undefined% Bool))
(= (put-late-row $row) (add-atom &late-schema-jobs $row))

; The negative SpaceOf dependency recompiles put-late-row and exposes the
; row schema when this declaration arrives. Open payloads stay guard-free;
; the later concrete contradiction is rejected at the space boundary.
(: &late-schema-jobs (SpaceOf (job Number)))

!(put-late-row (job "oops"))
