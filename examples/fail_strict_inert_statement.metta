; A discarded statement can only matter through its effects. A head that is
; still not a function once loading is complete builds inert data: here a
; misspelled helper, which used to compile as a constructor and do nothing.
(: note-seen! (-> Number Bool))
(= (note-seen! $x) (println! (seen $x)))
(: visit (-> Number Bool))
(= (visit $x) (let $_ (note-sen! $x) True))
!(visit 1)
