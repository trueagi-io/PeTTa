; `data` is an erased constructor: (data $a $b) builds the bare tuple ($a $b).
; A pattern spells that same tuple, so a (data ...) pattern matches what a
; (data ...) expression built. It used to stay the literal three-element list
; (data $a $b), which no constructed value could match, so the case silently
; took its fallback branch.
(= (pair-of $a $b) (data $a $b))
(= (tally $row) (case $row (((data $key $count) (counted $key $count)) ($_ no-row))))
!(test (tally (pair-of k 3)) (counted k 3))

; Inside a cons pattern, the shape of a collapsed match result:
(= (first-count $rows)
   (case $rows (((cons (data $key $count) $_rest) (counted $key $count)) (() empty))))
!(test (first-count (collapse (pair-of k 4))) (counted k 4))

; In a clause head, as in a let pattern:
(= (sum-pair (data $a $b)) (+ $a $b))
!(test (sum-pair (pair-of 3 4)) 7)
!(test (let (data $a $b) (pair-of 5 6) (+ $a $b)) 11)

; The first field is never called, in a pattern as in an expression:
(= (inc $x) (+ $x 1))
!(test (case (data inc 5) (((data inc $v) (literal inc $v)) ($_ called))) (literal inc 5))

; Raw positions keep the literal term, as before: a space row, a match
; pattern and an Atom argument are source, not constructed values.
!(bind! &rows (new-space))
!(add-atom &rows (data 1 2))
!(test (match &rows $r $r) (quote (data 1 2)))
!(test (collapse (match &rows (data $a $b) (found $a $b))) ((found 1 2)))

; A quoted payload is literal too, in a pattern as in a value:
!(test (case (quote (data 1 2)) (((data $a $b) erased) ($_ quoted))) quoted)
!(test (case (quote (quote (data 1 2))) (((quote (data $a $b)) (matched $a $b)) ($_ fallback)))
       (matched 1 2))

; Migration: a literal data head is matched by spelling it explicitly.
!(test (case (quote (data 1 2)) (((data data $a $b) (lit $a $b)) ($_ no))) (lit 1 2))
