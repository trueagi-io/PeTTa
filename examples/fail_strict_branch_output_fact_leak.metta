; The TV ascription belongs only to the is-var arm.  It must not leak through
; $tv into the fallback case arm and certify an arbitrary Atom as TV.
(: LeakSTV Type)
(: LeakCTV Type)
(: LeakNone Type)
(: LeakSTVValue (-[det]-> Number Number LeakSTV))
(: LeakCTVValue (-[det]-> LeakSTV LeakSTV LeakCTV))
(: leak-no-evidence LeakNone)
(: LeakTV (Alias (| LeakSTV LeakCTV LeakNone)))

(: checked-leak-stv (-[det]-> Atom LeakSTV))
(= (checked-leak-stv $tv)
   (case $tv
      (((LeakSTVValue $s $c) (LeakSTVValue $s $c))
       ($other (the LeakSTV $other)))))

(: branch-output-fact-must-not-leak (-[det]-> Atom LeakTV))
(= (branch-output-fact-must-not-leak $tv)
   (if (is-var $tv)
      (the LeakTV $tv)
      (case $tv
         (((LeakSTVValue $s $c) (LeakSTVValue $s $c))
          ((LeakCTVValue $pos $neg)
             (LeakCTVValue
                (checked-leak-stv $pos)
                (checked-leak-stv $neg)))
          (leak-no-evidence leak-no-evidence)
          ($other $other)))))
