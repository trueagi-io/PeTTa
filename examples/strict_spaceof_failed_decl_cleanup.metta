; Failed non-function declaration validation restores the declaration store.
; Once the contradictory row is removed, the same schema can be added cleanly.
!(add-atom &jobs (job "oops"))
!(let $error
   (catch
      (eval
         (quote
            (add-atom &self (: &jobs (SpaceOf (job Number)))))))
   True)
!(remove-atom &jobs (job "oops"))
!(add-atom &jobs (job 7))
(: &jobs (SpaceOf (job Number)))

!(test (collapse (match &jobs $x $x)) ((job 7)))
