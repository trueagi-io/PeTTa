; A -[det]-> function whose body is (once <semidet call>).
;
; once/1 removes a callee's EXTRA solutions, so nothing nondeterministic
; escapes - but it does not manufacture a solution where there was none, and
; the analysis used to discard its argument outright. A -[semidet]-> callee
; with no matching clause leaves (h 3) with zero solutions while h promises
; exactly one.
;
; (once E) is now: ok when E is ok, may_fail otherwise - including when E is
; nondeterministic or unanalysable, which once really does cap at one solution
; but cannot make productive. -[semidet]-> accepts every one of those.
(: lookup (-[semidet]-> Number Number))
(= (lookup 1) 10)

(: h (-[det]-> Number Number))
(= (h $x) (once (lookup $x)))

!(test (h 1) 10)
!(test (collapse (h 3)) ())
