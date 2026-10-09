; decons can fail on (), so it cannot implement an exactly-one-result arrow.
(: split-expression-det (-[det]-> Expression Expression))
(= (split-expression-det $value) (decons $value))
