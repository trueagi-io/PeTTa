; Recursive and nullary constructors use the existing checker.
(datatype DatatypeTree
   datatype-empty
   (datatype-leaf Number)
   (datatype-node DatatypeTree DatatypeTree))

(: datatype-size (-[det]-> DatatypeTree Number))
(= (datatype-size datatype-empty) 0)
(= (datatype-size (datatype-leaf $_value)) 1)
(= (datatype-size (datatype-node $left $right))
   (+ (datatype-size $left) (datatype-size $right)))

; Signatures remain beside mutually recursive functions.
(: datatype-even? (-[det]-> Number Bool))
(= (datatype-even? $n) (if (== $n 0) true (datatype-odd? (- $n 1))))
(: datatype-odd? (-[det]-> Number Bool))
(= (datatype-odd? $n) (if (== $n 0) false (datatype-even? (- $n 1))))

!(test (datatype-size (datatype-node (datatype-leaf 1) datatype-empty)) 1)
!(test (datatype-even? 4) true)
!(test (datatype-odd? 3) true)
!(test (get-type datatype-leaf) (-[det]-> Number DatatypeTree))
!(test (get-type datatype-empty) DatatypeTree)
!(test (quote (datatype Quoted quoted-constructor))
       (quote (datatype Quoted quoted-constructor)))
