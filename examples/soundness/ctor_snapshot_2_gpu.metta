; Multi-file soundness case, part 2 of 2 - load this file AFTER
; ctor_snapshot_1_goal.metta.
;
; A second four-element Goal constructor, arriving after the exclusion that
; term-conf's compilation already made. A GPU value now reaches the count-only
; branch, where the checker had certified $w : Number - and GPU's third field
; is a String.
;
; This declaration is the hook. It changes Goal's constructor set, which
; invalidates the recorded snapshot, which recompiles term-conf; the narrowing
; is refused this time and $w is no longer certified. The call below then
; contradicts the surviving obligation with a literal and is rejected at
; compile time, naming the value and the type.
;
; Before the fix nothing here was rejected: without --oracle the program
; reached (+ "oops" 1) and died inside is/2 with a Prolog arithmetic error that
; named neither the certification nor the type.

(: GPU (-> Expression Expression String Goal))

!(test (term-conf (GPU (a b c) (p) "oops")) 0)
