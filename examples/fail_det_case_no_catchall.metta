; A -[det]-> function whose case cannot match every value of the scrutinee.
;
; det means EXACTLY one solution. (f 3) matches neither branch, so it has
; zero. The clause-head exhaustiveness check cannot see this: f has a single
; clause with a variable head, which is exhaustive at the clause level - the
; incompleteness is inside the body.
;
; A case compiles to a nested if-then-else with no final else, so a value no
; branch matches makes the whole case FAIL. case_coverage_determinism/3
; reports that as may_fail, which -[semidet]-> accepts and -[det]-> rejects.
;
; The verdict is asymmetric, exactly like the clause-head check: only a
; PROVABLY uncovered value counts. Here the scrutinee is a Number and every
; branch pattern is a numeric literal, so no finite set of them can cover it.
; See semidet_partial_constructs.metta for the shapes that stay silent.
(: f (-[det]-> Number Number))
(= (f $x) (case $x ((1 10) (2 20))))
!(test (collapse (f 3)) ())
