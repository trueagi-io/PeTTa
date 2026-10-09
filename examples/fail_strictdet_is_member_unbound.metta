; An unbound probe makes is-member enumerate members, so the result is nondet.
(: some-known-name? (-[det]-> Bool))
(= (some-known-name?)
   (is-member $name (alpha beta gamma)))
