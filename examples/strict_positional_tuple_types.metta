; Positional tuple types. (Statement KBContext TV) is a three-FIELD record -
; field i has the i-th listed type - because every head atom here is a
; declared type name. Compare strict_tuple_types.metta, where the head of
; (Stats Number Number Number) is an undeclared atom and is therefore a
; literal tag the value must carry.
;
; The rule: a head atom that is a declared constructor of matching arity is a
; TAG; a head atom declared as anything else (typically (: Statement Type)) is
; the first FIELD's type; an undeclared head atom stays a tag.

(: Statement Type)
(: Implication Statement)
(: Conjunction Statement)
(: KBContext Type)
(: mkctx (-> Symbol Symbol KBContext))
(: TV Type)
(: STV (-> Number Number TV))

(: tv-conf (-> TV Number))
(= (tv-conf (STV $_strength $confidence)) $confidence)

; Destructuring in a clause head: the pattern has no tag, so its variables
; take the field types positionally and $tv is TV with no ascription.
(: fact-conf (-> (Statement KBContext TV) Number))
(= (fact-conf ($_statement $_context $tv)) (tv-conf $tv))

; A call site with a real value: the three fields check one by one, so
; Implication : Statement, (mkctx a b) : KBContext, (STV 1.0 0.9) : TV.
!(test (fact-conf (Implication (mkctx a b) (STV 1.0 0.9))) 0.9)

; The same shape as an output type - the constructed record is checked
; against the declaration, and flows into fact-conf without a residual guard.
(: mk-fact (-> Number (Statement KBContext TV)))
(= (mk-fact $confidence) (Conjunction (mkctx a b) (STV 1.0 $confidence)))

!(test (fact-conf (mk-fact 0.25)) 0.25)

; And destructured in a case branch:
(: fact-conf-cased (-> (Statement KBContext TV) Number))
(= (fact-conf-cased $fact)
   (case $fact
      ((($_statement $_context $tv) (tv-conf $tv)))))

!(test (fact-conf-cased (mk-fact 0.5)) 0.5)
