;Add
(: ＋ (-[semidet]-> Atom %Undefined%))
(= (＋ $x)
   (add-atom &mork $x))

;Remove
(: － (-[semidet]-> Atom %Undefined%))
(= (－ $x)
   (remove-atom &mork $x))

;Query
(: ? (-[nondet]-> Atom Atom %Undefined%))
(= (? $x $y)
   (match &mork $x $y))

;Transform
(: ~> (-[nondet]-> Atom Atom %Undefined%))
(= (~> $head $body)
   ((add-atom &mork (exec 0 $head $body))
    (mm2-exec &mork 1)))
