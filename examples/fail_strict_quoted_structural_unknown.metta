; An untyped field inside quoted structure is not positive evidence for Atom.
; Strict mode rejects the residual product check.
(: unknown-quoted (-[det]-> (Atom Number)))
(= (unknown-quoted)
   (quote ($unknown 1)))
