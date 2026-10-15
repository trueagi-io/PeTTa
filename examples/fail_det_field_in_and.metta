; The boundary check is SPINE-LEVEL: a destructured parameter (P $u) is bound
; as a whole by head unification, but its FIELD $u is NOT checked and may arrive
; unbound out of ordinary well-typed code ((P $u) built with an unfilled field).
; So $u is not enforced-bound, (and $u true) can still enumerate, and the
; -[det]-> promise is REJECTED - the field-leak asymmetry, pinned. Runs in the
; default mode: an explicit -[det]-> is checked in every mode.
(: P (-> Bool Pair))
(: use-field (-[det]-> Pair Bool))
(= (use-field (P $u)) (and $u true))

!(use-field (P true))
