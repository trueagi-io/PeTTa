; Sound boundary of flow-sensitive nonemptiness (checker feature 1).
;
; The nonempty narrowing upgrades a -[semidet]-> call to det ONLY when BOTH
; legs are proven: the callee's heads cover the nonempty domain AND every
; clause body is may-not-fail. Here the coverage leg holds - (cons $h $t)
; matches every nonempty list - but the body calls a -[semidet]-> function
; that can itself fail (lookup has no clause for 2). So risky-first can still
; produce zero results on a nonempty input, the upgrade is refused, and
; risky-or-zero's -[det]-> promise is rejected. This pins leg (b): a callee
; whose body can fail keeps the semidet verdict at the narrowed site.

(: lookup (-[semidet]-> Number Number))
(= (lookup 1) 10)

(: risky-first (-[semidet]-> (List Number) Number))
(= (risky-first (cons $head $_tail)) (lookup $head))

(: risky-or-zero (-[det]-> (List Number) Number))
(= (risky-or-zero $values)
   (if (== $values ())
      0
      (risky-first $values)))

!(test (risky-or-zero (1 2)) 10)
