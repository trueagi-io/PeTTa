(datatype DatatypeChoice datatype-first datatype-second)
(: datatype-first-only (-[det]-> DatatypeChoice Number))
(= (datatype-first-only datatype-first) 1)
!(datatype-first-only datatype-first)
