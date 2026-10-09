(= (boom) (/ 1 0))
(= (invoke $f) ($f))

!(catch (collapse (let $f (superpose (boom)) (invoke $f))))
!(catch (collapse (invoke boom)))
