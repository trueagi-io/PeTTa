(: guarded-list-id (-> (List Number) (List Number)))
(= (guarded-list-id $xs) $xs)

; Strict mode rejects the implicit residual guard rather than allowing the
; nested variable to acquire an incompatible value later.
!(let $result (guarded-list-id ($x))
      (let "not-a-number" $x $result))
