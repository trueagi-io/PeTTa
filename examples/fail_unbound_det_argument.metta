; The runtime side of the NEED-BASED boundary check. A -[det]-> function is
; checked nonvar only on the parameters its determinism proof CONSUMED. use-it's
; body (and $b true) proves det precisely BY treating $b as an enforced-bound
; Bool operand of and/2 (manifest_bool -> enforced_bound_param), so the proof
; consumes $b's boundness and the boundary check is emitted for it. leak hands
; use-it an UNBOUND argument; enforcement throws a clear unbound_det_argument
; error where before the code enumerated a finite type through bool/1.
; Default mode: enforcement is mode-independent, keyed on the explicit arrow.
(: use-it (-[det]-> Bool Bool))
(= (use-it $b) (and $b true))

(: leak (-> Bool))
(= (leak) (use-it $u))

!(leak)
