!(import! &self (library lib_builtin_types))

; Erased nominal newtypes: distinct compile-time roles over one runtime
; representation. Nothing is wrapped and no guard is emitted - passing this
; file under --strict is itself the proof.
(: KB (Newtype Expression))
(: Proof (Newtype Expression))
(: Statement (Newtype Expression))

(: Evidence Type)
(: rule-ev (-> KB Proof Evidence))

; Parameters propagate brands; raw literals at call sites acquire them
; contextually from the expected position.
(: remember-rule (-> KB Proof Evidence))
(= (remember-rule $kb $proof) (rule-ev $kb $proof))

!(test (remember-rule mykb r1) (rule-ev mykb r1))

; A dynamic boundary is branded explicitly - erased trust, no runtime goal.
(: from-dynamic (-> %Undefined% Evidence))
(= (from-dynamic $x) (rule-ev (brand KB $x) (brand Proof r0)))

!(test (from-dynamic somekb) (rule-ev somekb r0))

; brand erases inside Expression-typed argument positions too: the callee
; receives the bare value, never a literal (brand ...) form.
(: Box (Newtype Expression))
(: identity-box (-> Box Box))
(= (identity-box $value) $value)

(: brand-and-call (-> Expression Box))
(= (brand-and-call $value) (identity-box (brand Box $value)))

!(test (brand-and-call (payload value)) (payload value))

; Brands do not survive an untyped space, but a declared relation schema
; restores them on match - still guard-free.
(: rule-row (-> KB Proof Atom))
(rule-row kb1 p1)

(: fetch (-> Evidence))
(= (fetch) (the Evidence (match &self (rule-row $kb $p) (rule-ev $kb $p))))

!(test (fetch) (rule-ev kb1 p1))
