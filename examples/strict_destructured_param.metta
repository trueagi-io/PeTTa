; A variable bound by a DESTRUCTURING head pattern is a parameter like any
; other, so it gets the same fresh assumption type that a plain parameter gets:
; both clauses below infer (-> Number Number Number) / (-> (stv Number Number)
; Number) and compile to the same guard-free multiplication. --strict rejects
; any residual runtime type check, so compiling this file at all is the
; assertion - before, tvf2 emitted two.

(= (tvf  $s $c)       (* $s $c))
(= (tvf2 (stv $s $c)) (* $s $c))

!(test (tvf 2 3) 6)
!(test (tvf2 (stv 2 3)) 6)

; The pattern's own type is rebuilt from those field assumptions, which is what
; lets the call site keep checking the argument - see
; fail_destructured_param_mismatch.metta.

; A pattern headed by a uniquely declared constructor takes its field types
; from that declaration instead, and needs no type for the scrutinee to do it:
; in a head pattern, in a let/let* binder, and in a case branch.
(: Pair (-> Number Number PairT))

(= (pair-head (Pair $a $b)) (* $a $b))
(= (pair-let  $p) (let (Pair $a $b) $p (* $a $b)))
(= (pair-let* $p) (let* (((Pair $a $b) $p)) (* $a $b)))
(= (pair-case $p) (case $p (((Pair $a $b) (* $a $b)))))

!(test (pair-head (Pair 2 3)) 6)
!(test (pair-let  (Pair 2 3)) 6)
!(test (pair-let* (Pair 2 3)) 6)
!(test (pair-case (Pair 2 3)) 6)
