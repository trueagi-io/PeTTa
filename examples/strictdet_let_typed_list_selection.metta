; A let value with a proper-list OUTPUT CERTIFICATE carries selection shape
; into the continuation. A declared (List ...) result alone is intentionally
; insufficient for a local because its runtime value could still be unbound.
(: first-list (-[semidet]-> (List (List Number)) (List Number)))
(= (first-list (cons $head $_tail)) $head)

(: pick-first (-[det]-> (List (List Number)) (List Number)))
(= (pick-first $raw)
   (let $values
      (collapse (superpose $raw))
      (if (== $values ())
         ()
         (first-list $values))))

!(test (pick-first ((1) (2))) (1))
