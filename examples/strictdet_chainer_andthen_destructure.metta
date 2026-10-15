; Chainer-shaped regression: the equality in and-then destructures an
; AnnotatedStatement.  The constructor declaration must type the bound
; $statement field as Statement before the right-hand operand is checked.
(: Proof Type)
(: Statement Type)
(: TV Type)
(: AnnotatedStatement Type)
(: KBContext Type)
(: Goal Type)

(: : (-[det]-> Proof Statement TV AnnotatedStatement))
; The production program also overloads ':' at arity four. Arity-qualified
; declared arguments and constructor lookup must still select :/3 above.
(: : (-[det]-> KBContext Proof Statement TV Goal))
(: proof-token Proof)
(: statement-token Statement)
(: tv-token TV)

(: statement-accepted? (-[det]-> Statement Bool))
(= (statement-accepted? $statement)
   (== $statement statement-token))

(: annotated-statement-accepted? (-[det]-> AnnotatedStatement Bool))
(= (annotated-statement-accepted? $annotated)
   (and-then
      (= $annotated (: $_proof $statement $_tv))
      (statement-accepted? $statement)))

!(test
   (annotated-statement-accepted?
      (: proof-token statement-token tv-token))
   true)
