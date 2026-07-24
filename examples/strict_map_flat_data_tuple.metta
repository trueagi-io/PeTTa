;; Reduced from PeTTaChainer goal-results, the "map-flat + --strict" report.
;;
;; A -[det]-> function map-flats a helper over a list. The helper
;; (normalize-entry) is a plain -> function whose body returns a VAR-HEADED
;; tuple ($k ...): its head is a data parameter, so this is deterministic
;; data construction. A -[det]-> function's determinism is verified by
;; TRANSITIVELY analysing its callees' clauses. That analysis reads a callee's
;; stored clauses, which - unlike the callee's own direct check - had no
;; parameter type information, so a var-headed tuple built from a data
;; parameter was misread as a dynamic call of unknown determinism. The pure
;; helper then analysed as unspecified and poisoned this -[det]-> caller; the
;; error printed the whole map-flat body, which is why it looked like a
;; map-flat bug. Passes in normal mode, --strict, and --strict-det.
!(import! &self (library lib_roman))

(: normalize-entry (-> (Number (List Number)) (Number (List Number))))
(= (normalize-entry ($k $v)) ($k (list_to_set $v)))

(: normalize-all
   (-[det]-> (List (Number (List Number))) (List (Number (List Number)))))
(= (normalize-all $entries) (map-flat normalize-entry $entries))

(: entry (-> Number (List Number) (Number (List Number))))
(= (entry $k $v) ($k $v))

!(test (normalize-all (cons (entry 1 (2 2 3)) (cons (entry 5 (5 5)) ())))
       (cons (entry 1 (2 3)) (cons (entry 5 (5)) ())))
!(test (normalize-all ()) ())
