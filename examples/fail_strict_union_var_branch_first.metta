!(import! &self (library lib_builtin_types))

; The unsound ordering of strict_union_prior_branch_narrowing.metta: the
; variable-headed four-element pattern comes FIRST, so nothing has been
; excluded yet. A (CPU $fun $args $res) value - four elements, a Goal - reaches
; this branch at runtime, so narrowing the union to its positional member
; would be a lie: $prf is not known to be a Proof.
;
; The checker must refuse to narrow, leaving $prf untyped, so the Proof
; argument of proof-score needs a residual runtime check that --strict rejects.

(: Goal Type)
(: CPU (-> Symbol Expression Expression Goal))

(: Statement Type)
(: KBContext Type)
(: Proof (Newtype Expression))
(: TV Type)

(: proof-score (-> Proof TV Number))
(= (proof-score $_proof $_tv) 0.5)

(: cpu-result-confidence (-> Expression Number))
(= (cpu-result-confidence $_res) 0.2)

(: term-confidence
   (-> (| Goal
          (Statement KBContext Proof TV))
       Number))
(= (term-confidence $proof-term)
   (case $proof-term
      ((($_type ($_kbid $_ctx $_vars) $prf $tv)
          (proof-score $prf $tv))
       ((CPU $_fun $_args $res)
          (cpu-result-confidence $res)))))

!(term-confidence (CPU f (a b) (r)))
