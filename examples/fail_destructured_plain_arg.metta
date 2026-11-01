; Direct parameters still move their inferred requirement to the caller.
(: b B)
(: only-a (-> A %Undefined%))
(= (only-a $x) (ok $x))
(= (g $x) (only-a $x))
!(g b)
