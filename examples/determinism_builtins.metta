; The determinism of a builtin is whatever builtin_call_determinism/3 records
; and nothing else, so the table has to carry every builtin that genuinely is
; deterministic or legitimate proofs disappear. It is also the ONLY thing the
; checker knows about them, so every entry has to be true under the calling
; convention that actually holds: any argument may be UNBOUND or of the wrong
; type at run time. Nothing the compiler emits rules that out - the residual
; type guard succeeds on an unbound variable, so a declared Bool never implies
; a bound one and a declared (List T) never implies a proper list.
;
; This pins what a det body may rely on. Arithmetic, comparison and reflection
; are det because their single clause RAISES on an argument it cannot use, and
; an exception is neither a second solution nor a missing one.
(: arith (-[det]-> Number Number Number))
(= (arith $a $b) (+ (* $a 2) (min $b (max $a 3))))

(: classify (-[det]-> %Undefined% Bool))
(= (classify $x) (is-ground $x))

; sort-atom and unique-atom are the two list wrappers that are det, and for a
; reason the rest of the family does not share: msort/2 and list_to_set/2
; raise on a partial or unbound list rather than enumerating.
(: canonical (-[det]-> Expression Expression))
(= (canonical $xs) (unique-atom (sort-atom $xs)))

; The rest of the list operations INVERT. size-atom measures an open list by
; enumerating every length it could have - (size-atom $u) on a fully unbound
; argument does not terminate - and union-atom is append, which enumerates
; every split. Those modes are used on purpose (examples/logicprogset.metta,
; lib_roman's mylast/init/rcons), so the honest arrow for a body that calls
; them says nondet. See examples/fail_det_size_atom_open_list.metta and
; examples/fail_det_unwrappable_list_builtin.metta.
(: shape (-[nondet]-> Expression Number))
(= (shape $x) (size-atom (cdr-atom $x)))

(: extend (-[nondet]-> (List Number) Number (List Number)))
(= (extend $xs $x) (union-atom $xs ($x)))

; and/or/not/xor/implies type-check their arguments with bool/1, which is two
; facts - so an unbound argument is enumerated rather than reported, and the
; operator answers twice (examples/booleansolver.metta relies on exactly that,
; examples/fail_det_bool_generator.metta shows what it costs).
(: compare-them (-[nondet]-> Number Number Bool))
(= (compare-them $a $b) (and (< $a $b) (not (== $a $b))))

; get-metatype's eight clauses do not cover every value - a partial
; application is neither a list nor an atom - so it is semidet, which is what
; -[semidet]-> is for. repr is det.
(: describe (-[semidet]-> Expression String))
(= (describe $x) (repr (get-metatype $x)))

!(test (arith 4 5) 12)
!(test (classify (a b)) true)
!(test (canonical (b a b)) (a b))
!(test (shape (a b c)) 2)
!(test (extend (1 2) 3) (1 2 3))
!(test (compare-them 1 2) true)
!(test (describe (a b)) "Expression")
