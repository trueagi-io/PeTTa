; A typed-space update whose row the compiler proves against the schema
; skips the runtime row check; every other update keeps it.
(: &rows (SpaceOf (Number Number)))
!(bind! &rows (new-space))

(= (some-string) (car-atom ("a")))

; Proven: both fields are the declared Number parameter.
(: put (-> Number Bool))
(= (put $n) (add-atom &rows ($n $n)))

(: drop (-> Number Bool))
(= (drop $n) (remove-atom &rows ($n $n)))

!(put 1)
!(put 2)
!(drop 1)
!(test (collapse (match &rows ($a $b) $a)) (2))

; An undeclared caller still cannot store a wrong row through put: the
; declared parameter is guarded at the call.
(= (put-via $x) (put $x))
!(test (let (Error $e $kind) (catch (put-via (some-string))) $kind) typecheck)

; Not proven: an undeclared function's parameter type is only an inference
; assumption, and a parametric parameter is the caller's choice.
(= (put-untyped $x) (add-atom &rows ($x $x)))
!(test (let (Error $e $kind) (catch (put-untyped (some-string))) $kind) typecheck)

(: put-any (-> $t Bool))
(= (put-any $x) (add-atom &rows ($x $x)))
!(test (let (Error $e $kind) (catch (put-any (some-string))) $kind) typecheck)

!(test (collapse (match &rows ($a $b) $a)) (2))
