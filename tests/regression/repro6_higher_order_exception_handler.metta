(= (boom $x) (/ $x 0))
(= (invoke $f) ($f 1))
(callee boom)

!(catch (collapse (match &self (callee $f) (invoke $f))))
!(catch (collapse (invoke boom)))
