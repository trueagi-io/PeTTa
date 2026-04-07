;Depends on is-even from import_order_helper.metta
(= (process-number $x)
   (if (is-even $x)
    (+ $x 15)
    (* $x 2)))

