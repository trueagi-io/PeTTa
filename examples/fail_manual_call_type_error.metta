; Manual dispatch bypasses typed translation, not typing: the unique
; declaration's input checks still apply to call/reduce.
(: inc (-> Number Number))
(= (inc $x) (+ $x 1))
(: bad (-> Number))
(= (bad) (call (inc "oops")))
!(test (bad) 0)
