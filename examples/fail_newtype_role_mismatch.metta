; Different newtypes never unify merely because they share a representation:
; swapping role-branded arguments is a compile error, not a runtime guard
; (there is nothing left to guard at runtime - brands are erased).
(: KB (Newtype Expression))
(: Proof (Newtype Expression))
(: Evidence Type)
(: rule-ev (-> KB Proof Evidence))
(: swapped (-> KB Proof Evidence))
(= (swapped $kb $proof) (rule-ev $proof $kb))
!(test (swapped mykb r1) never)
