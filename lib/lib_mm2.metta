;Add
(: ＋ (-> Atom %Undefined%))
(= (＋ $x)
   (add-atom &mork $x))

;Remove
(: － (-> Atom %Undefined%))
(= (－ $x)
   (remove-atom &mork $x))

;Query
(: ? (-> Atom Atom %Undefined%))
(= (? $x $y)
   (match &mork $x $y))

;Transform
(: ~> (-> Atom Atom %Undefined%))
(= (~> $head $body)
   ((add-atom &mork (exec 0 $head $body))
    (mm2-exec &mork 1)))
