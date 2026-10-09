; rows initially has a consumed proper-list output certificate, making the
; effect-polymorphic traversal deterministic. Its late non-list clause
; withdraws that proof; the dependency graph must recompile consume through
; the unified selection proof's output_cert edge and reject its stale verdict.
(: cert-id (-[det]-> Atom Atom))
(= (cert-id $x) $x)

(: cert-map (-[$e]-> (-[$e]-> Atom Atom) (List Atom) (List Atom)))
(= (cert-map $f ()) ())
(= (cert-map $f (cons $x $xs))
   (cons ($f $x) (cert-map $f $xs)))

(: rows (-> Atom Expression))
(= (rows $x) ())

(: consume (-[det]-> Atom Expression))
(= (consume $x) (cert-map cert-id (rows $x)))

!(add-atom &self (= (rows $x) $x))
