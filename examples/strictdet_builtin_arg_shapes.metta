; builtin_call_determinism/3 is keyed on (name, arity) only, so one worst-case
; verdict has to serve every call site. Most of the weak entries are weak for a
; SHAPE reason - length/2, append/3 and select/3 INVERT over an open list,
; min_list/2 has no answer for (), and bool/1 invents a boolean it was not
; given - and where the shape is manifest in the source that reason does not
; apply. The stronger verdict is provable-only: see
; fail_strictdet_min_atom_unknown_shape.metta.
;
; What counts as manifest is the SPINE, and only the spine the compiler itself
; builds: a list literal, or a cons onto a manifestly proper tail. A declared
; type never counts, not even a fixed-width one - a residual guard succeeds on
; an unbound variable, so no type implies a bound value.

; A two-element list literal cannot be the () that min_list/2 has no answer
; for, so min-atom is det here even though the table calls it semidet.
(: min-known-pair (-[det]-> Number Number Number))
(= (min-known-pair $a $b) (min-atom ($a $b)))
!(test (min-known-pair 3 5) 3)

(: max-of-three (-[det]-> Number Number Number Number))
(= (max-of-three $a $b $c) (max-atom ($a $b $c)))
!(test (max-of-three 3 9 5) 9)

; size-atom, length and reverse enumerate the shapes an OPEN list can take; a
; literal spine is not open.
(: literal-size (-[det]-> Number))
(= (literal-size) (size-atom (1 2 3)))
!(test (literal-size) 3)

(: literal-reverse (-[det]-> Number Number %Undefined%))
(= (literal-reverse $a $b) (reverse ($a $b)))
!(test (literal-reverse 1 2) (2 1))

; A cons onto a manifestly proper tail is manifestly proper too.
(: consed-size (-[det]-> Number Number))
(= (consed-size $x) (size-atom (cons $x (2 3))))
!(test (consed-size 1) 3)

; last/2 needs the list non-empty as well as proper.
(: literal-last (-[det]-> Number Number Number))
(= (literal-last $a $b) (last ($a $b)))
!(test (literal-last 1 2) 2)

; append/3 recurses on its first list; only that one has to be proper.
(: front (-[det]-> Number Number (List Number)))
(= (front $a $b) (append ($a $b) ()))
!(test (front 1 2) (1 2))

; and/or/not/xor/implies are nondet because bool/1 generates rather than
; tests. An operand that is a literal, or the result of a det builtin whose
; declared output is Bool, is already a bound boolean.
(: in-range (-[det]-> Number Bool))
(= (in-range $x) (and (> $x 0) (< $x 10)))
!(test (in-range 5) True)
!(test (in-range 50) False)

(: either (-[det]-> Bool))
(= (either) (or True False))
!(test (either) True)
