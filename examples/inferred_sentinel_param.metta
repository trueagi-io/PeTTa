; An undeclared function's parameter types are INFERRED from how its body uses
; them, and "uses" over-approximates "requires": $current is a Number only on
; the branch that the sentinel test rules out. Inference records that knowledge
; but does not demand it of callers, so (score none 0.42) - a correct program
; that handles the sentinel explicitly - compiles and runs, in every mode.
; See README, "Inference", and strict_union_types.metta for the declared way to
; say the same thing.

(= (score $current $cand) (if (== $current none) $cand (max $cand $current)))

!(test (score none 0.42) 0.42)
!(test (score 0.1 0.42) 0.42)
!(test (score 0.9 0.42) 0.9)

; ...also when the caller cannot see the argument value at all, and when the
; caller is itself undeclared (its own parameters stay unconstrained rather
; than inheriting the callee's inferred Number):
(= (best $a $b) (score $a $b))

!(test (best none 0.5) 0.5)
!(test (best 0.7 0.5) 0.7)

; The knowledge is still there: the inferred output type of score is Number, so
; this arithmetic fuses instead of guarding.
!(test (+ (score none 1.5) 1) 2.5)
