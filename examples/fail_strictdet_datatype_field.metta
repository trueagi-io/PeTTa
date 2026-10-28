(datatype DatatypeField (datatype-number Number))
(: datatype-wrong-field (-[det]-> DatatypeField))
(= (datatype-wrong-field) (datatype-number "wrong"))
!(datatype-wrong-field)
