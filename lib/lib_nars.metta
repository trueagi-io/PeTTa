;; Import shared truth value functions
(import! lib_truth)

;; Truth functions (Truth_c2w and Truth_w2c imported from lib_truth)

(= (Truth_Deduction (stv $f1 $c1)
                    (stv $f2 $c2))
   (stv (* $f1 $f2) (* (* $f1 $f2) (* $c1 $c2))))

(= (Truth_Abduction (stv $f1 $c1) 
                    (stv $f2 $c2))
   (stv $f2 (Truth_w2c (* (* $f1 $c1) $c2))))

(= (Truth_Induction $T1 $T2)
   (Truth_Abduction $T2 $T1))

(= (Truth_Exemplification (stv $f1 $c1)
                          (stv $f2 $c2))
   (stv 1.0 (Truth_w2c (* (* $f1 $f2) (* $c1 $c2)))))

(= (Truth_StructuralDeduction $T)
   (Truth_Deduction $T (stv 1.0 0.9)))
  
(= (Truth_Negation (stv $f $c))
   (stv (- 1 $f) $c))

(= (Truth_StructuralDeductionNegated $T)
   (Truth_Negation (Truth_StructuralDeduction $T)))

(= (Truth_Intersection (stv $f1 $c1) 
                       (stv $f2 $c2))
   (stv (* $f1 $f2) (* $c1 $c2)))

(= (Truth_StructuralIntersection $T)
   (Truth_Intersection $T (stv 1.0 0.9)))

(= (Truth_or $a $b)
   (- 1 (* (- 1 $a) (- 1 $b))))

(= (Truth_Comparison (stv $f1 $c1)
                     (stv $f2 $c2))
   (let $f0 (Truth_or $f1 $f2) 
        (stv (if (== $f0 0.0)
                 0.0 
                 (/ (* $f1 $f2) $f0))
             (Truth_w2c (* $f0 (* $c1 $c2))))))
  
(= (Truth_Analogy (stv $f1 $c1)
                  (stv $f2 $c2))
   (stv (* $f1 $f2) (* (* $c1 $c2) $f2)))

(= (Truth_Resemblance (stv $f1 $c1)
                      (stv $f2 $c2))
   (stv (* $f1 $f2) (* (* $c1 $c2) (Truth_or $f1 $f2))))

(= (Truth_Union (stv $f1 $c1)
                (stv $f2 $c2))
   ((Truth_or $f1 $f2) (* $c1 $c2)))

(= (Truth_Difference (stv $f1 $c1)
                     (stv $f2 $c2))
   (stv (* $f1 (- 1 $f2)) (* $c1 $c2)))

(= (Truth_DecomposePNN (stv $f1 $c1)
                       (stv $f2 $c2))
   (let $fn (* $f1 (- 1 $f2))
        (stv (- 1 $fn) (* $fn (* $c1 $c2)))))

(= (Truth_DecomposeNPP (stv $f1 $c1)
                       (stv $f2 $c2))
   (let $f (* (- 1 $f1) $f2)
        (stv $f (* $f (* $c1 $c2)))))

(= (Truth_DecomposePNP (stv $f1 $c1)
                       (stv $f2 $c2))
   (let $f (* $f1 (- 1 $f2))
        (stv $f (* $f (* $c1 $c2)))))

(= (Truth_DecomposePPP $v1 $v2)
   (Truth_DecomposeNPP (Truth_Negation $v1) $v2))

(= (Truth_DecomposeNNN (stv $f1 $c1)
                       (stv $f2 $c2))
   (let $fn (* (- 1 $f1) (- 1 $f2))
        ((- 1 $fn) (* $fn (* $c1 $c2)))))

(= (Truth_Eternalize (stv $f $c))
   (stv $f (Truth_w2c $c)))

(= (Truth_Revision (stv $f1 $c1)
                   (stv $f2 $c2))
   (let* (($w1 (Truth_c2w $c1))
          ($w2 (Truth_c2w $c2))
          ($w  (+ $w1 $w2))
          ($f (/ (+ (* $w1 $f1) (* $w2 $f2)) $w))
          ($c (Truth_w2c $w)))
          (stv (min 1.00 $f) (min 0.99 (max (max $c $c1) $c2)))))

(= (Truth_Expectation (stv $f $c))
   (+ (* $c (- $f 0.5)) 0.5))

;; INFERENCE RULES

;;NAL-1
;;!Revision
(= (|- ($T $T1) ($T $T2)) ($T (Truth_Revision $T1 $T2)))
;;!Syllogistic rules for -->:
(= (|- ((--> $a $b) $T1) ((--> $b $c) $T2)) ((--> $a $c) (Truth_Deduction $T1 $T2)))
(= (|- ((--> $a $b) $T1) ((--> $a $c) $T2)) ((--> $c $b) (Truth_Induction $T1 $T2)))
(= (|- ((--> $a $c) $T1) ((--> $b $c) $T2)) ((--> $b $a) (Truth_Abduction $T1 $T2)))
(= (|- ((--> $a $b) $T1) ((--> $b $c) $T2)) ((--> $c $a) (Truth_Exemplification $T1 $T2)))

;;NAL-3
;;!Set decomposition:
(= (|- ((--> ({} $A $B) $M) $T)) ((--> ({} $A) $M) (Truth_StructuralDeduction $T)))
(= (|- ((--> ({} $A $B) $M) $T)) ((--> ({} $B) $M) (Truth_StructuralDeduction $T)))
(= (|- ((--> $M ([] $A $B)) $T)) ((--> $M ([] $A)) (Truth_StructuralDeduction $T)))
(= (|- ((--> $M ([] $A $B)) $T)) ((--> $M ([] $B)) (Truth_StructuralDeduction $T)))
;;!Extensional and intensional intersection decomposition:
(= (|- ((--> (∪ $S $P) $M) $T)) ((--> $S $M) (Truth_StructuralDeduction $T)))
(= (|- ((--> $M (∩ $S $P)) $T)) ((--> $M $S) (Truth_StructuralDeduction $T)))
(= (|- ((--> (∪ $S $P) $M) $T)) ((--> $P $M) (Truth_StructuralDeduction $T)))
(= (|- ((--> $M (∩ $S $P)) $T)) ((--> $M $P) (Truth_StructuralDeduction $T)))
(= (|- ((--> (~ $A $S) $M) $T)) ((--> $A $M) (Truth_StructuralDeduction $T)))
(= (|- ((--> $M (− $B $S)) $T)) ((--> $M $B) (Truth_StructuralDeduction $T)))
(= (|- ((--> (~ $A $S) $M) $T)) ((--> $S $M) (Truth_StructuralDeductionNegated $T)))
(= (|- ((--> $M (− $B $S)) $T)) ((--> $M $S) (Truth_StructuralDeductionNegated $T)))
;;!Extensional and intensional intersection decomposition:
(= (|- ((--> $S $M) $T1) ((--> (∪ $S $P) $M) $T2)) ((--> $P $M) (Truth_DecomposePNN $T1 $T2)))
(= (|- ((--> $P $M) $T1) ((--> (∪ $S $P) $M) $T2)) ((--> $S $M) (Truth_DecomposePNN $T1 $T2)))
(= (|- ((--> $S $M) $T1) ((--> (∩ $S $P) $M) $T2)) ((--> $P $M) (Truth_DecomposeNPP $T1 $T2)))
(= (|- ((--> $P $M) $T1) ((--> (∩ $S $P) $M) $T2)) ((--> $S $M) (Truth_DecomposeNPP $T1 $T2)))
(= (|- ((--> $S $M) $T1) ((--> (~ $S $P) $M) $T2)) ((--> $P $M) (Truth_DecomposePNP $T1 $T2)))
(= (|- ((--> $S $M) $T1) ((--> (~ $P $S) $M) $T2)) ((--> $P $M) (Truth_DecomposeNNN $T1 $T2)))
(= (|- ((--> $M $S) $T1) ((--> $M (∩ $S $P)) $T2)) ((--> $M $P) (Truth_DecomposePNN $T1 $T2)))
(= (|- ((--> $M $P) $T1) ((--> $M (∩ $S $P)) $T2)) ((--> $M $S) (Truth_DecomposePNN $T1 $T2)))
(= (|- ((--> $M $S) $T1) ((--> $M (∪ $S $P)) $T2)) ((--> $M $P) (Truth_DecomposeNPP $T1 $T2)))
(= (|- ((--> $M $P) $T1) ((--> $M (∪ $S $P)) $T2)) ((--> $M $S) (Truth_DecomposeNPP $T1 $T2)))
(= (|- ((--> $M $S) $T1) ((--> $M (− $S $P)) $T2)) ((--> $M $P) (Truth_DecomposePNP $T1 $T2)))
(= (|- ((--> $M $S) $T1) ((--> $M (− $P $S)) $T2)) ((--> $M $P) (Truth_DecomposeNNN $T1 $T2)))

;; NAL-4
;;!Rules for more efficient reasoning about relation components:
(= (|- ((--> (× $A $B) $R) $T1) ((--> (× $C $B) $R) $T2)) ((--> $C $A) (Truth_Abduction $T1 $T2)))
(= (|- ((--> (× $A $B) $R) $T1) ((--> (× $A $C) $R) $T2)) ((--> $C $B) (Truth_Abduction $T1 $T2)))
(= (|- ((--> $R (× $A $B)) $T1) ((--> $R (× $C $B)) $T2)) ((--> $C $A) (Truth_Induction $T1 $T2)))
(= (|- ((--> $R (× $A $B)) $T1) ((--> $R (× $A $C)) $T2)) ((--> $C $B) (Truth_Induction $T1 $T2)))
(= (|- ((--> (× $A $B) $R) $T1) ((--> $C $A) $T2)) ((--> (× $C $B) $R) (Truth_Deduction $T1 $T2)))
(= (|- ((--> (× $A $B) $R) $T1) ((--> $A $C) $T2)) ((--> (× $C $B) $R) (Truth_Induction $T1 $T2)))
(= (|- ((--> (× $A $B) $R) $T1) ((--> $C $B) $T2)) ((--> (× $A $C) $R) (Truth_Deduction $T1 $T2)))
(= (|- ((--> (× $A $B) $R) $T1) ((--> $B $C) $T2)) ((--> (× $A $C) $R) (Truth_Induction $T1 $T2)))
(= (|- ((--> $R (× $A $B)) $T1) ((--> $A $C) $T2)) ((--> $R (× $C $B)) (Truth_Deduction $T1 $T2)))
(= (|- ((--> $R (× $A $B)) $T1) ((--> $C $A) $T2)) ((--> $R (× $C $B)) (Truth_Abduction $T1 $T2)))
(= (|- ((--> $R (× $A $B)) $T1) ((--> $B $C) $T2)) ((--> $R (× $A $C)) (Truth_Deduction $T1 $T2)))
(= (|- ((--> $R (× $A $B)) $T1) ((--> $C $B) $T2)) ((--> $R (× $A $C)) (Truth_Abduction $T1 $T2)))

;;NAL-5
;;!Negation ∧ and ∨ decomposition:
(= (|- ((¬ $A) $T)) ($A (Truth_Negation $T)))
(= (|- ((∧ $A $B) $T)) ($A (Truth_StructuralDeduction $T)))
(= (|- ((∧ $A $B) $T)) ($B (Truth_StructuralDeduction $T)))
(= (|- ($S $T1) ((∧ $S $A) $T2)) ($A (Truth_DecomposePNN $T1 $T2)))
(= (|- ($S $T1) ((∨ $S $A) $T2)) ($A (Truth_DecomposeNPP $T1 $T2)))
(= (|- ($S $T1) ((∧ (¬ $S) $A) $T2)) ($A (Truth_DecomposeNNN $T1 $T2)))
(= (|- ($S $T1) ((∨ (¬ $S) $A) $T2)) ($A (Truth_DecomposePPP $T1 $T2)))
;;!Higher-order decomposition
(= (|- ($A $T1) ((==> $A $B) $T2)) ($B (Truth_Deduction $T1 $T2)))
(= (|- ($A $T1) ((==> (∧ $A $B) $C) $T2)) ((==> $B $C) (Truth_Deduction $T1 $T2)))
(= (|- ($B $T1) ((==> $A $B) $T2)) ($A (Truth_Abduction $T1 $T2)))

;;How many derivation steps (task selections)
(= (NARS.Config.MaxSteps) 100)

;;Size of active tasks PQ
(= (NARS.Config.TaskQueueSize) 10)

;;Max beliefs buffer size
(= (NARS.Config.BeliefQueueSize) 100)

;;Whether evidence was just counted once
(= (StampDisjoint $Ev1 $Ev2)
   (== () (collapse (let* (($x (superpose $Ev1))
                           ($y (superpose $Ev2)))
                          (case (== $x $y) ((True True)))))))

;;Concat stamp with sorting
(= (StampConcat $stamp $addition)
   (if (== $addition ())
       $stamp
       (msort (append $stamp $addition))))

;;retrieve the best candidate (PQ functionality)
(= (BestCandidate $evaluateCandidateFunction $bestCandidate $tuple)
   (if (== $tuple ())
       $bestCandidate
       (let* (($head (car-atom $tuple))
              ($tail (cdr-atom $tuple)))
             (if (> ($evaluateCandidateFunction $head)
                    ($evaluateCandidateFunction $bestCandidate))
                 (BestCandidate $evaluateCandidateFunction $head $tail)
                 (BestCandidate $evaluateCandidateFunction $bestCandidate $tail)))))

;;candidate evaluation based on priority
(= (PriorityRank (Sentence ($x (stv $f $c)) $Ev1)) $c)
(= (PriorityRank ()) -99999.0)

;;candidate elimination based on negated priority
(= (PriorityRankNeg (Sentence ($x (stv $f $c)) $Ev1)) (- 0.0 $c))
(= (PriorityRankNeg ()) -99999.0)

;;Return limited-sized version of $L (bounded PQ functionality)
(= (LimitSize $L $size)
   (if (< (length $L) $size)
       $L
       (let $lowestPriorityItem (BestCandidate PriorityRankNeg () $L)
            (LimitSize (exclude-item $lowestPriorityItem $L) $size))))

;;Priority-queue based task ranking deriver with belief buffer
(= (NARS.Derive $Tasks $Beliefs $steps $maxsteps $taskqueuesize $beliefqueuesize)
   (if (or (> $steps $maxsteps) (== $Tasks ()))
       ($Tasks $Beliefs)
       (let (Sentence $x $Ev1) (BestCandidate PriorityRank () $Tasks)
            (let $derivations
                 (collapse (superpose ((let* (((Sentence $y $Ev2) (superpose $Beliefs))
                                              ($stamp (msort (append $Ev1 $Ev2))))
                                             (if (StampDisjoint $Ev1 $Ev2)
                                                 (case (superpose ((|- $x $y)
                                                                   (|- $y $x)))
                                                       ((($T $TV) (Sentence ($T $TV) $stamp))))
                                                 (empty)))
                                       (case (|- $x) ((($T3 $TV3) (Sentence ($T3 $TV3) $Ev1)))))))
                 (let $temp (trace! (SELECTED $steps (Sentence $x $Ev1)) 42)
                            (NARS.Derive (LimitSize (exclude-item (Sentence $x $Ev1) (list_to_set (append $Tasks $derivations))) $taskqueuesize)
                                         (LimitSize (list_to_set (append $Beliefs $derivations)) $beliefqueuesize)
                                         (+ $steps 1)
                                         $maxsteps
                                         $taskqueuesize
                                         $beliefqueuesize))))))

(= (NARS.Derive $Tasks $Beliefs $maxsteps $taskqueuesize $beliefqueuesize)
   (NARS.Derive $Tasks $Beliefs 1 $maxsteps $taskqueuesize $beliefqueuesize))

(= (NARS.Derive $Tasks $Beliefs $maxsteps)
   (NARS.Derive $Tasks $Beliefs $maxsteps (NARS.Config.TaskQueueSize) (NARS.Config.BeliefQueueSize)))

(= (NARS.Derive $Tasks $Beliefs)
   (NARS.Derive $Tasks $Beliefs (NARS.Config.MaxSteps)))

;;Candidate evaluation based on confidence
(= (ConfidenceRank ((stv $f $c) $Ev)) $c)
(= (ConfidenceRank ()) 0)

;;Pose a question of a certain term to the system on some knowledge base
(= (NARS.Query $Tasks $Beliefs $term $maxsteps $taskqueuesize $beliefqueuesize)
   (BestCandidate ConfidenceRank () (collapse (let ($TasksRet $BeliefsRet) (NARS.Derive $Tasks $Beliefs $maxsteps $taskqueuesize $beliefqueuesize)
                                                   (case (superpose $BeliefsRet)
                                                         (((Sentence ($Term $TV) $Ev) (case (== $Term $term)
                                                                                            ((True ($TV $Ev)))))))))))

(= (NARS.Query $kb $term $maxsteps $taskqueuesize $beliefqueuesize)
   (NARS.Query $kb $kb $term $maxsteps $taskqueuesize $beliefqueuesize))

(= (NARS.Query $kb $term $maxsteps)
   (NARS.Query $kb $term $maxsteps (NARS.Config.TaskQueueSize) (NARS.Config.BeliefQueueSize))) ;default space bound

(= (NARS.Query $kb $term)
   (NARS.Query $kb $term (NARS.Config.MaxSteps))) ;default steps bound
