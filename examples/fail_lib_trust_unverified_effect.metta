!(import! &self (library lib_trust_fixture))

(mystery-number 7)

; This undeclared intermediary may suppress the library guard, but that makes
; its call effect semidet.  A det caller cannot launder it into exact-one.
(= (untyped-trusted-bridge)
   (trusted-number-id
      (match &self (mystery-number $value) $value)))

(: trusted-effect-launder (-[det]-> Number))
(= (trusted-effect-launder) (untyped-trusted-bridge))
