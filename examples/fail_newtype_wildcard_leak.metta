; (Newtype T) over a WILDCARD representation used to be a universal type.
;
; type_unify/2 rewrote a brand to its representation, and Expression is a
; wildcard that unifies with everything - so (: Proof (Newtype Expression))
; made Proof and Number mutually compatible. leak's body handed a Proof to a
; function requiring a Number and the obligation was discharged statically,
; with no guard emitted; the program compiled clean, in --strict too, and died
; inside is/2.
;
; The rule now is that a wildcard is "nothing is stated here", not "every type
; at once". A brand fits itself and any wildcard requirement (that is what
; erasure means), and otherwise fits whatever its representation fits - but a
; WILDCARD representation constrains nothing, so it grants nothing: Proof no
; longer fits Number. Nothing implicitly fits a brand either, wildcards
; included; a value acquires a brand only by being written (brand T V).
;
; The obligation on $p therefore survives compilation as a residual runtime
; check, and the call below contradicts it with a literal.
;
; The payload is a LIST on purpose. This is the variant the argument oracle
; could not catch either: --oracle re-checks a discharged obligation with the
; checker's own value relation, and check_value/3 answers "unknown" for a list
; against Number rather than "mismatch". The oracle is structurally blind to a
; wrong type MODEL - it can only re-ask the same question - so this half had to
; be fixed in the model, not instrumented.
(: Proof (Newtype Expression))

(: need-number (-> Number Number))
(= (need-number $n) (+ $n 1))

(: leak (-> Proof Number))
(= (leak $p) (need-number $p))

!(test (leak (brand Proof (a b))) 0)
