; conduit is det modulo the effect of its closure while helper is inferred
; det.  The late helper clause changes conduit's conditional verdict; that
; change must flow on to use, where nondeterminism violates -[det]->.
(: identity (-[det]-> Atom Atom))
(= (identity $x) $x)

(: helper (-> Atom Atom))
(= (helper $x) $x)

(: conduit (-[$e]-> (-[$e]-> Atom Atom) Atom Atom))
(= (conduit $f $x) (helper ($f $x)))

(: use (-[det]-> Atom Atom))
(= (use $x) (conduit identity $x))

!(add-atom &self (= (helper $x) (superpose ($x extra))))
