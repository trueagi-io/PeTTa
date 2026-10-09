!(import! &self (library lib_builtin_types))

; Narrowing a union with an UNTAGGED (variable-headed) case pattern.
;
; The scrutinee is (| Goal (Statement KBContext Proof TV)). The second branch
; matches any four-element expression, which by shape alone could still be a
; Goal - CPU/3 also builds four elements. What makes it safe is that case is
; first-match/committed: branch one already consumed every CPU value, and CPU
; is Goal's only constructor, so a four-element value reaching branch two must
; be the positional member. $prf : Proof and $tv : TV follow positionally, and
; compiling under --strict is the proof that no ascription and no residual
; runtime check is needed.
;
; The $prf : Proof half of that used to be vacuous: Proof is a newtype over
; Expression, Expression is a wildcard, and a brand over a wildcard fitted
; every type in both directions - so proof-score's first parameter would have
; accepted anything the narrowing produced. It does not any more (brand_unify/2
; in typecheck.pl): the narrowing has to deliver Proof specifically, and this
; example now pins that it does.
;
; Reverse the two branches and the narrowing is refused - see
; fail_strict_union_var_branch_first.metta.

(: Goal Type)
(: CPU (-> Symbol Expression Expression Goal))

(: Statement Type)
(: Implication Statement)
(: KBContext Type)
(: mkctx (-> Symbol Symbol KBContext))
(: Proof (Newtype Expression))
(: TV Type)
(: STV (-> Number Number TV))

(: proof-score (-> Proof TV Number))
(= (proof-score $_proof $_tv) 0.5)

(: cpu-expected-confidence (-> Symbol Expression Number))
(= (cpu-expected-confidence $_fun $_args) 0.1)
(: cpu-result-confidence (-> Expression Number))
(= (cpu-result-confidence $_res) 0.2)

(: term-confidence
   (-> (| Goal
          (Statement KBContext Proof TV))
       Number))
(= (term-confidence $proof-term)
   (case $proof-term
      (((CPU $fun $args $res)
          (if (is-var $res)
             (cpu-expected-confidence $fun $args)
             (cpu-result-confidence $res)))
       (($_type ($_kbid $_ctx $_vars) $prf $tv)
          (proof-score $prf $tv)))))

; branch one, result already computed:
!(test (term-confidence (CPU AndMarginalProjection (a b) (r))) 0.2)
; branch one, result still a variable:
!(test (term-confidence (CPU AndMarginalProjection (a b) $result)) 0.1)
; branch two: the untagged four-element record:
!(test (term-confidence (Implication (mkctx a b) (brand Proof (p)) (STV 1.0 1.0))) 0.5)
