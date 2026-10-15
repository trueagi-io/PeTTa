!(import! &self (library lib_builtin_types))

; The other side of strict_union_reducible_helper_not_ctor.metta: make-goal is
; declared exactly the same way but has NO equation, so it never reduces and
; (make-goal f (a b) (r)) IS a Goal value - a second three-argument Goal
; constructor.
;
; Branch one consumes only CPU/3, so a four-element make-goal value reaches
; branch two. Narrowing the union to its positional member there would be a
; lie: $tv is not known to be a TV. The checker must refuse, leaving the TV
; argument of proof-score to a residual runtime check that --strict rejects.
;
; This is the guard against the "defined functions are not constructors" rule
; being widened into "declared functions are not constructors".

(: Goal Type)
(: CPU (-> $formula $arguments $result Goal))

(: Statement Type)
(: Implication Statement)
(: KBContext Type)
(: mkctx (-> Symbol Symbol KBContext))
(: Proof (Newtype Expression))
(: TV Type)
(: STV (-> Number Number TV))

(: make-goal (-> Symbol Expression Expression Goal))

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

!(term-confidence (make-goal f (a b) (r)))
