; callPredicate stays nondet by default: an inner Prolog goal with NO
; declared determinism arrow claims nothing - member/2 genuinely
; enumerates - so a -[det]-> wrapper around it is rejected. Declare the
; predicate's arrow (a trusted foreign promise, as lib_builtin_types does
; for assertz and erase) to state otherwise.
(: pick-any (-[det]-> Expression Bool))
(= (pick-any $list)
   (let $_picked
      (callPredicate
         (Predicate (member $x $list)))
      true))

!(pick-any (a b))
