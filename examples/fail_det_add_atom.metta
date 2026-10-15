; add-atom's fallback clause is add_sexp(Space, [Rel|Args]) - it needs an
; expression. A bare symbol matches neither that nor the (= ...) function
; clause, so (add-atom &self foo) fails: zero solutions, from something
; declared det. add-atom and remove-atom are semidet.
(: remember (-[det]-> Atom Bool))
(= (remember $a) (add-atom &self $a))

!(remember (fact 1))
