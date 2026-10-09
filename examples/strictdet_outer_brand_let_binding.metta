!(import! &self (library lib_builtin_types))

; The nominal output knowledge recorded by brand survives a let binding and
; discharges the declared Proof input of its consumer.
(: Proof (Newtype Expression))

(: accepts-proof (-[det]-> Proof Bool))
(= (accepts-proof $_proof) true)

(: construct-and-use-proof (-[det]-> Number Bool))
(= (construct-and-use-proof $index)
   (let $proof
      (brand Proof
         (if (== $index 0)
            (data ctx proof)
            (data ctx proof $index)))
      (accepts-proof $proof)))

!(test (construct-and-use-proof 0) true)
!(test (construct-and-use-proof 1) true)
