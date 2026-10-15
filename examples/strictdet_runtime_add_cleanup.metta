; A failed runtime function addition leaves no raw atom, registration,
; metadata, executable clause, or dependency edge behind. Removing the
; declaration afterward makes the name ordinary data again.
(: ghost (-[det]-> Atom Number))

!(let $error
   (catch
      (eval
         (quote
            (add-atom &self
               (= (ghost $x) (superpose (1 2)))))))
   (let $removed
      (remove-atom &self (: ghost (-[det]-> Atom Number)))
      (test (ghost a) (ghost a))))
