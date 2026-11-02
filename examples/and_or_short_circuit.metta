; and/or evaluate left to right and stop at the operand that decides the
; result: (and False X) and (or True X) never evaluate X. They used to be
; eager calls, so X always ran, and a failing X removed the result.
!(bind! &calls (new-space))
(= (probe $v) (progn (add-atom &calls (probed $v)) $v))
(= (calls) (collapse (match &calls (probed $v) $v)))

!(test (and False (probe True)) False)
!(test (or True (probe False)) True)
!(test (calls) ())
!(test (and True (probe False)) False)
!(test (or False (probe True)) True)
!(test (calls) (False True))

; A right operand that fails no longer removes a decided result:
!(test (and False (empty)) False)
!(test (or True (empty)) True)
!(test (collapse (and True (empty))) ())

; Per left result, in order: the right operand runs only for the True one.
!(test (collapse (and (superpose (True False)) (superpose (True False))))
       (True False False))

; An unbound operand still enumerates both Bools:
!(test (collapse (let $r (and $x True) ($x $r))) ((True True) (False False)))

; So does an operand expression whose value is unbound:
!(test (collapse (and True (progn $u))) (True False))
!(test (collapse (or False (progn $v))) (True False))
!(test (collapse (or (progn $w) False)) (True False))
