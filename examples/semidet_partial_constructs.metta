; The other side of the may_fail rules: what stays ACCEPTED.
;
; once, a two-argument if and a case with no catch-all all fail to deliver a
; result for some input, so none of them can be -[det]->. All three are
; perfectly good -[semidet]-> bodies: semidet promises zero OR one, commits to
; the first matching clause exactly like det, and costs the same.
;
; The counterexamples are fail_det_once_semidet.metta,
; fail_det_two_arg_if.metta and fail_det_case_no_catchall.metta.

(: lookup (-[semidet]-> Number Number))
(= (lookup 1) 10)

(: maybe-lookup (-[semidet]-> Number Number))
(= (maybe-lookup $x) (once (lookup $x)))

(: when-true (-[semidet]-> Bool Number))
(= (when-true $b) (if $b 1))

(: small-name (-[semidet]-> Number Number))
(= (small-name $x) (case $x ((1 10) (2 20))))

; A case the checker can prove TOTAL is still det: Bool has exactly two
; values and both are matched.
(: negate (-[det]-> Bool Bool))
(= (negate $b) (case $b ((true false) (false true))))

; And a case whose scrutinee type has no enumerable value set stays SILENT -
; the coverage verdict is asymmetric, so "cannot prove it incomplete" is
; accepted rather than punished. (List Number) is not a nominal type with a
; constructor set, so nothing here is provable either way.
(: head-or-zero (-[det]-> (List Number) Number))
(= (head-or-zero $l) (case $l ((() 0) (($h) $h))))

!(test (maybe-lookup 1) 10)
!(test (collapse (maybe-lookup 3)) ())
!(test (when-true true) 1)
!(test (collapse (when-true false)) ())
!(test (small-name 2) 20)
!(test (negate true) false)
!(test (negate false) true)
!(test (head-or-zero ()) 0)
!(test (head-or-zero (7)) 7)
