; A discarded statement whose fields evaluate is not inert even when its head
; is a constructor: the fields' effects and failure happen.
(: show (-> Bool))
(= (show) (progn (row (println! effect)) True))
!(test (show) True)
(: none (-> Bool))
(= (none) (progn (row (empty)) True))
!(test (collapse (none)) ())
!(test (progn (row (println! top)) done) done)
