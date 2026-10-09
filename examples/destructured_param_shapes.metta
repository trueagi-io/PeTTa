; A rebuilt parameter shape is only knowledge if it is READ BACK as the shape
; it was built from. The two readings of a tuple type do not compose freely
; (see tagged_tuple_type/3): with (: Statement Type) declared, the pattern
; (Statement $s $p) infers a type whose head is a declared type name, which
; reads positionally - "field 1 is a Statement" - and no value matching the
; pattern is that. The shape is dropped rather than believed, so these calls
; are not rejected.
(: Statement Type)

(= (subj (Statement $s $p)) (* $s $p))

!(test (subj (Statement 2 3)) 6)

; Nested patterns work the same way, one assumption per bound variable:
(= (inner (box (pt $x $y))) (+ $x $y))

!(test (inner (box (pt 2 3))) 5)

; A field the body never constrains leaves the whole shape unknown rather than
; half-claimed, so passing anything in that position is still fine:
(= (fst (pair $a $b)) $a)

!(test (fst (pair 1 "two")) 1)
!(test (fst (pair "one" 2)) "one")
