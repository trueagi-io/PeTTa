; Functions added at runtime via add-atom go through the same translation
; pipeline as file loading, so --strict-det validates them too: a superpose
; body contradicts the plain -> declaration at the add-atom call site.
!(add-atom &self (: dyn-bad (-> Number Number)))
!(add-atom &self (= (dyn-bad $n) (superpose (1 $n))))
!(test (dyn-bad 4) never-reached)
