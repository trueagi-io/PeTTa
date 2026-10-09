; Multi-file soundness case, part 1 of 2 - load this file FIRST.
;
; Two clauses, no type declaration: an untyped, genuinely nondeterministic
; nullary function. Nothing is wrong here yet.
(= (pick) 1)
(= (pick) 2)
