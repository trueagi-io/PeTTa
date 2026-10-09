; The file batch initially proves that producer always constructs a proper
; Bool.  A runtime clause mutation invalidates that batch summary before the
; later consumer is compiled; otherwise the stale fact would let and accept a
; producer call that can now return the unbound atom below.
(: scope-bogus Bool)

(: scope-producer (-[semidet]-> Bool Bool))
(= (scope-producer true) true)

!(add-atom &self
   (= (scope-producer scope-bogus) scope-bogus))

(: consume-mutated-producer (-[semidet]-> Bool Bool))
(= (consume-mutated-producer $value)
   (and (scope-producer $value) true))
