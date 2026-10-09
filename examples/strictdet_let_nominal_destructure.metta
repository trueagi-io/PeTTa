; A value of a nominal type with one constructor is guaranteed to match that
; constructor's fresh-variable pattern. The proof depends on the constructor
; set and is invalidated if another inhabitant is declared later.
(: Stats (-[det]-> Number Number Number DistributionStats))

(: zero-stats (-[det]-> DistributionStats))
(= (zero-stats) (Stats 0 0 0))

(: stats-count (-[det]-> Number))
(= (stats-count)
   (let (Stats $sum $sumsq $count)
      (zero-stats)
      $count))

!(test (stats-count) 0)
