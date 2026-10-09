; An untyped-storage catchall can state its runtime obligation explicitly.
; Unlike a leaked branch fact, this keeps the guard on the actual fallback.
(: StoredSTV Type)
(: StoredCTV Type)
(: StoredNone Type)
(: StoredSTVValue (-[det]-> Number Number StoredSTV))
(: StoredCTVValue (-[det]-> StoredSTV StoredSTV StoredCTV))
(: stored-no-evidence StoredNone)
(: StoredTV (Alias (| StoredSTV StoredCTV StoredNone)))

(: checked-stored-stv-example (-[det]-> Atom StoredSTV))
(= (checked-stored-stv-example $tv)
   (case $tv
      (((StoredSTVValue $s $c) (StoredSTVValue $s $c))
       ($other (the StoredSTV $other)))))

(: checked-stored-tv-example (-[det]-> Atom StoredTV))
(= (checked-stored-tv-example $tv)
   (if (is-var $tv)
      (the StoredTV $tv)
      (case $tv
         (((StoredSTVValue $s $c) (StoredSTVValue $s $c))
          ((StoredCTVValue $pos $neg)
             (StoredCTVValue
                (checked-stored-stv-example $pos)
                (checked-stored-stv-example $neg)))
          (stored-no-evidence stored-no-evidence)
          ($other (the StoredTV $other))))))

!(test
   (checked-stored-tv-example (StoredSTVValue 0.5 0.75))
   (StoredSTVValue 0.5 0.75))
