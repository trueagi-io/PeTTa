; get-metatype has eight clauses - variable, number, string, the two booleans,
; a registered fun, a list, an atom - and a partial application is none of
; them: (add3 1) compiles to partial(add3, [1]), a compound that is not a
; list, so the call fails. get-metatype is therefore semidet, not det, and a
; -[det]-> body may not call it.
(: add3 (-> Number Number Number Number))
(= (add3 $a $b $c) (+ $a (+ $b $c)))

(: mt (-[det]-> Atom))
(= (mt) (get-metatype (add3 1)))

!(mt)
