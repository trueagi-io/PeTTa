; Whether (f ...) is a call or data must not depend on load order. A caller
; compiled before f exists builds data; when f is defined by a later import,
; or registered later as a Prolog native, the caller is recompiled into a
; call. It used to stay data unless f had been declared first.
!(import! &self imports/late_definition/early)
!(test (early-call (1 2)) (later-defined 1))
!(bind! &effects (new-space))
!(import! &self imports/late_definition/late)
!(test (early-call (1 2)) 101)
!(test (early-statement 7) done)
!(test (collapse (match &effects (effect $x) $x)) (7))

; A native imported after its caller compiled:
!(test (early-native (5 6)) (late-native 5))
!(assertaPredicate (Predicate (late-native $x $x)))
!(import_prolog_function late-native)
!(test (early-native (5 6)) 5)
