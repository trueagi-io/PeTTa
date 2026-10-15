; Representative outer control-flow shape used by PeTTaChainer's
; query-needs-temp-rule?.
; The nonempty guard makes decons exactly-one, its result is an exact
; two-field list, and the recursive helper coinductively proves proper Bool
; results, so the right-associated binary and stays deterministic.
(: statement-children (-[det]-> Expression Expression))
(= (statement-children $items) $items)

(: query-child-needs-bundle? (-[det]-> Expression Bool))
(= (query-child-needs-bundle? $item)
   (is-alpha-member $item (a Compute Evaluation)))

(: compound-direct-query-bundle? (-[det]-> Expression Bool))
(= (compound-direct-query-bundle? $items)
   (if (== $items ()) false
      (case $items (
         ((cons $item $rest)
            (if (query-child-needs-bundle? $item)
               true
               (compound-direct-query-bundle? $rest)))))))

(: query-shape? (-[det]-> Expression Bool))
(= (query-shape? $term)
   (if (and
          (is-expr $term)
          (not (== $term ())))
      (let ($head $tail) (decons $term)
         (and
            (not (is-var $head))
            (and
               (is-alpha-member $head (And Or LikelierThan))
               (compound-direct-query-bundle?
                  (statement-children $tail)))))
      false))

!(test (query-shape? (And a b)) true)
!(test (query-shape? (Other a b)) false)
!(test (query-shape? ()) false)
