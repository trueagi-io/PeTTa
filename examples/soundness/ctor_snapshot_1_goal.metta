; Multi-file soundness case, part 1 of 2 - load this file FIRST.
;
; This is the constructor-set SNAPSHOT problem on union_member_excluded/3,
; isolated so it can be regression-tested. The second branch of the case below
; narrows the union by ELEMENT COUNT only: its pattern has a variable head, so
; it carries no tag evidence. That narrowing is admitted because branch one
; already consumed CPU, and CPU is - right now, in this file - Goal's only
; four-element constructor.
;
; "Right now" was the whole problem. The exclusion is a snapshot of the
; constructor set as it stands while this file compiles, and part 2 declares a
; SECOND four-element Goal constructor. Nothing used to revisit this clause.
;
; The snapshot is now recorded with the key set it saw (note_ctor_snapshot/1),
; and a later declaration that changes that set recompiles the clauses which
; read it - sharing recompile_function_clauses/1 with the late-declaration
; fix, because it is the same problem wearing a different hat. Recompiled with
; GPU visible, the narrowing is correctly refused and the obligation on $w
; survives as a real check instead of a false certification.
;
; See examples/soundness/ctor_snapshot_2_gpu.metta.

(: Goal Type)
(: CPU (-> Symbol Expression Expression Goal))

(: Statement Type)
(: Implication Statement)
(: KBContext Type)
(: mkctx (-> Symbol Symbol KBContext))
(: Proof (Newtype Expression))

(: proof-weight (-> Proof Number Number))
(= (proof-weight $_p $n) (+ $n 1))

(: cpu-conf (-> Symbol Expression Expression Number))
(= (cpu-conf $_f $_a $_r) 0.2)

(: term-conf
   (-> (| Goal
          (Statement KBContext Proof Number))
       Number))
(= (term-conf $t)
   (case $t
     (((CPU $fun $args $res) (cpu-conf $fun $args $res))
      (($_type ($_kbid $_ctx $_vars) $prf $w) (proof-weight $prf $w)))))

; The honest call: a real positional member of the second union arm.
!(test (term-conf (Implication (mkctx a b) (brand Proof (p)) 41)) 42)
