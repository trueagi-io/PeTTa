; The constructor field type exists only after a successful match.  On the
; fallback edge $statement is unbound and must not inherit Statement.
(: Proof Type)
(: Statement Type)
(: TV Type)
(: AnnotatedStatement Type)
(: : (-[det]-> Proof Statement TV AnnotatedStatement))

(: accepts-statement? (-[det]-> Statement Bool))
(= (accepts-statement? $_statement) true)

(: match-fact-must-not-leak? (-[det]-> AnnotatedStatement Bool))
(= (match-fact-must-not-leak? $annotated)
   (if (= $annotated (: $_proof $statement $_tv))
       true
       (accepts-statement? $statement)))
