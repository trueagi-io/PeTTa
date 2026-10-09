; Every exhaustive case arm constructs an actual Bool.  The call therefore
; carries the same proper-Bool guarantee as the if producer.
(: bool-from-case (-[det]-> Bool Bool))
(= (bool-from-case $choice)
   (case $choice
      ((true true)
       (false false))))

(: consume-case-bool (-[det]-> Bool Bool))
(= (consume-case-bool $choice)
   (and (bool-from-case $choice) true))

!(test (consume-case-bool true) true)
!(test (consume-case-bool false) false)
