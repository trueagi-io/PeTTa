; Multi-file soundness case, part 1 of 2 - load this file FIRST.
;
; The OTHER user of a constructor-set snapshot: det exhaustiveness. Colour has
; exactly two values right now, rank matches both, so the -[det]-> promise of
; exactly one result holds - as far as this file can see.
;
; "As far as this file can see" is the whole problem, and it is the same
; problem ctor_snapshot_1_goal.metta poses for union narrowing. There the
; snapshot decides a per-clause narrowing, which can be recompiled; here it
; decides a property of the whole clause set, judged in a per-file prepass
; with no single clause to redo, so the verdict itself is recorded and re-run.
;
; See examples/soundness/late_ctor_exhaustive_2_blue.metta.

(: Colour Type)
(: Red Colour)
(: Green Colour)

(: rank (-[det]-> Colour Number))
(= (rank Red) 1)
(= (rank Green) 2)

!(test (rank Red) 1)
