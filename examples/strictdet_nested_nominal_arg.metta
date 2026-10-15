; A brand of Expression does NOT inherit the unevaluated-argument
; convention: (Newtype Expression) says the payload shape is unconstrained,
; not that callers must quote, so a nominal-typed position receives the
; VALUE of a nested call. From PeTTaChainer's
; typechecker_repros/nested_nominal_call_argument_not_reduced.metta.
;; A nested function call returning a nominal type must be evaluated before
;; its value is passed to another function accepting that nominal type.

(: Role (Newtype Expression))
(: role (-[det]-> Number Role))

(: Holder (Newtype Expression))
(: holder (-[det]-> Role Holder))

(: holder-role (-[det]-> Holder Role))
(= (holder-role (holder $role)) $role)

(: expected-role? (-[det]-> Role Bool))
(= (expected-role? $role)
   (== $role (role 1)))

(: holder-has-expected-role? (-[det]-> Holder Bool))
(= (holder-has-expected-role? $holder)
   (expected-role? (holder-role $holder)))

!(test
   (holder-has-expected-role?
      (holder (role 1)))
   true)
