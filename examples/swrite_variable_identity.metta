;; Serialized variable identity, asserted behaviorally -- no naming scheme
;; is assumed anywhere in this file.
;; (Compact form of the sharing shape from Nil's OBC S13 enumeration.)

(= (countdown $n)
   (if (< 0 $n)
       (cons $n (countdown (- $n 1)))
       ()))

(= (padded-ax1 $p $q)
   (: ax1 (-> $p (-> $q $p))))

(= (two-vars $p $q)
   (: pair (-> $p $q)))

;; 1. Distinctness survives the round trip: two distinct variables must
;;    re-parse as distinct, i.e. bindable to different constants.
!(test (case (parse (repr (two-vars $a $b)))
             (((: pair (-> 7 8)) distinct-preserved)
              ($other fabricated-sharing)))
       distinct-preserved)

;; 2. Sharing survives the round trip: if the two occurrences of one
;;    variable print under different names, the re-parsed term has two
;;    independent variables, which CAN bind to the different constants
;;    1 and 2 -- the first case branch fires and reports the split.
!(test (case (parse (repr (padded-ax1 $shared (countdown 2000))))
             (((: ax1 (-> 1 (-> $pad 2))) split-detected)
              ($other sharing-preserved)))
       sharing-preserved)
