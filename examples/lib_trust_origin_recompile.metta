!(import! &self (library lib_trust_fixture))

(origin-mystery "not-a-number")

(= (origin-sensitive-caller)
   (trusted-number-id
      (match &self (origin-mystery $value) $value)))

; Flipping the callee origin must invalidate and recompile the already-built
; guard-free caller.
!(add-atom &self
   (: trusted-number-id (-[det]-> Number Number)))

!(test
   (let $caught
      (catch (eval (quote (origin-sensitive-caller))))
      (car-atom $caught))
   Error)
