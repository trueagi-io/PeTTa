;Migrate atoms to another space:
(: migrateAtoms (-> %Undefined% %Undefined% Atom %Undefined%))
(= (migrateAtoms $FromSpace $ToSpace $Pattern)
   (match $FromSpace $Pattern ((add-atom $FromSpace $Pattern)
                               (remove-atom $FromSpace $Pattern))))

;Like unify-space case of Minimal MeTTa:
(: find (-> %Undefined% Atom %Undefined%))
(= (find $space $pattern)
   (case (match $space $pattern True)
         ((True True)
          (Empty False))))

;Check whether a predicate succeeds
(: succeedsPredicate (-> Atom Bool))
(= (succeedsPredicate $pattern)
   (quote (case (translatePredicate (catch (Predicate $pattern) $_ fail))
                (($item True)
                 (Empty False)))))
!(add-translator-rule! succeedsPredicate)

;Remove all atoms of a space:
(= (remove-all-atoms $space) 
   (collapse (match $space $x (remove-atom $space $x))))

;Efficiently count the amount of particular items in the space without listifying the items:
(: match-count (-> %Undefined% Atom %Undefined%))
(= (match-count $space $pattern)
   (foldall + (match $space $pattern 1) 0))
