!(import! &self (library lib_builtin_types))

; Union narrowing by prior branch, with a REDUCIBLE helper in the way.
;
; As in strict_union_prior_branch_narrowing.metta, branch one consumes every
; CPU/3 value, so the variable-headed four-element pattern in branch two can
; only be the positional (Statement KBContext Proof TV) member, and $tv : TV.
;
; What is pinned here is that make-goal/3 does not disturb that. It is
; declared to return Goal and it takes three arguments, so its applications
; are four elements too - but it HAS an equation, so PeTTa always rewrites
; (make-goal ...) at the call site and no such expression ever survives as a
; value. Only a declared symbol WITHOUT equations stays literal data, and only
; those count as constructors when deciding whether branch one was exhaustive.
;
; Remove make-goal's equation, keeping the declaration, and it becomes a real
; second Goal constructor that branch one does not consume - narrowing is then
; correctly refused, see fail_strict_union_undefined_second_ctor.metta.
;
; The definition is placed BELOW its use on purpose: definedness is recorded
; in the parse prepass over the whole file, so order does not matter.

(: Goal Type)
(: CPU (-> $formula $arguments $result Goal))

(: Statement Type)
(: Implication Statement)
(: KBContext Type)
(: mkctx (-> Symbol Symbol KBContext))
(: Proof (Newtype Expression))
(: TV Type)
(: STV (-> Number Number TV))

(: make-goal (-> $formula $arguments $result Goal))

(: proof-score (-> $proof TV Number))
(= (proof-score $_proof $_tv) 0.5)

(: term-confidence
   (-> (| Goal
          (Statement KBContext Proof TV))
       Number))
(= (term-confidence $proof-term)
   (case $proof-term
      (((CPU $_fun $_args $_res) 0.1)
       (($_statement $_context $proof $tv)
          (proof-score $proof $tv)))))

(= (make-goal $fun $arguments $result)
   (CPU $fun $arguments $result))

!(test (term-confidence (CPU formula arguments result)) 0.1)
!(test (term-confidence (make-goal formula (a b) (r))) 0.1)
!(test (term-confidence (Implication (mkctx a b) (brand Proof (p)) (STV 1.0 1.0))) 0.5)
