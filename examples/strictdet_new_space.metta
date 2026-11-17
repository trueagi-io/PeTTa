(: allocate (-[det]-> Symbol))
(= (allocate) (new-space))
!(test (get-metatype (allocate)) Symbol)
!(test (let* (($first (allocate)) ($second (allocate)))
         (== $first $second)) False)

(: apply-maker (-[det]-> (-[det]-> Symbol) Symbol))
(= (apply-maker $f) ($f))
!(test (is-space (apply-maker allocate)) True)
