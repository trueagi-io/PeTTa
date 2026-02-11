(= (clamp $v $min $max)
   (min $max (max $v $min)))

;;Concat tuples
(= (TupleConcat $Ev1 $Ev2)
   (append $Ev1 $Ev2))
;   (collapse (superpose ((superpose $Ev1) (superpose $Ev2)))))

;;Count size of tuple
(= (TupleCount $tuple)
   (length $tuple))
;   (if (== $tuple ()) 0 (+ 1 (TupleCount (cdr-atom $tuple)))))

;Shortcut for and with 5 args
(= (and5 $0 $1 $2 $3 $4)
   (and $0 (and $1 (and $2 (and $3 $4)))))

;Shortcut for min with 5 args
(= (min5 $0 $1 $2 $3 $4)
   (min $0 (min $1 (min $2 (min $3 $4)))))

;Exclude division by 0
(= (/safe $A $B)
   (if (> $B 0.0)
       (/ $A $B)
       (empty)))

;Negate (1 minus)
(= (negate $arg)
   (- 1.0 $arg))

;Invert (1 over)
(= (invert $arg)
   (/safe 1.0 $arg))

;;Test case
(= (Test2 $Is $Should)
   ((Is: $Is) (Should: $Should) (Passed: (== $Is $Should))))

;; InsertionSort helper: insert element x into sorted list L
(= (InsertSorted $x $L)
   (if (== $L ())
       ($x)
       (let* (($head (car-atom $L))
              ($tail (cdr-atom $L)))
             (if (< $x $head)
                 (TupleConcat ($x $head) $tail)
                 (TupleConcat ($head) (InsertSorted $x $tail))))))

;; InsertionSort main
(= (InsertionSort $L $Ret)
   (msort $L))
;   (if (== $L ())
;       $Ret
;       (let* (($x (car-atom $L))
;              ($rest (cdr-atom $L))
;              ($newRet (InsertSorted $x $Ret)))
;             (InsertionSort $rest $newRet))))

;;Exclude item from tuple
(= (Without $Tuple $a)
   (exclude-item $a $Tuple))
;   (collapse (let $x (superpose $Tuple)
;                  (if (== $x $a)
;                      (empty)
;                      $x))))

;;Whether a is an element of the tuple
(= (ElementOf $a $Tuple)
   (is-member $a $Tuple))
;   (not (== (collapse (let $x (superpose $Tuple)
;                           (if (== $x $a)
;                               True
;                               (empty)))) ())))

;;Fast but TODO use efficient native handling once available: https://github.com/trueagi-io/metta-morph/pull/27
(= (Unique $L $Ret)
   (list_to_set $L))
;   (if (== $L ())
;       $Ret
;       (let* (($x (car-atom $L))
;              ($rest (cdr-atom $L)))
;             (if (ElementOf $x $Ret)
;                 (Unique $rest $Ret)
;                 (Unique $rest (TupleConcat ($x) $Ret))))))

;; Consistency Conditions: PLN book "5.2.2.2 PLN Deduction and Second-Order Probability", page 74:

; borrowed from https://github.com/trueagi-io/hyperon-pln/blob/main/metta/pln/dependent-types/DeductionDTL.metta
(: smallest-intersection-probability (-> Number Number Number))
(= (smallest-intersection-probability $As $Bs)
   (clamp (/ (- (+ $As $Bs) 1) $As) 0 1))

(: largest-intersection-probability (-> Number Number Number))
(= (largest-intersection-probability $As $Bs)
   (clamp (/ $Bs $As) 0 1))

(: conditional-probability-consistency (-> Number Number Number Bool))
(= (conditional-probability-consistency $As $Bs $ABs)
   (and (< 0 $As)
        (and (<= (smallest-intersection-probability $As $Bs) $ABs)
             (<= $ABs (largest-intersection-probability $As $Bs)))))

(= (Consistency_ImplicationImplicantConjunction $As $Bs $Cs $ACs $BCs)
   ; Conditional probability consistency checks:
   ; P(C|A) <= P(C)/P(A)  =>  $sAC <= $sC / $sA
   ; P(C|B) <= P(C)/P(B)  =>  $sBC <= $sC / $sB
   ; Also ensure denominators are not zero for the checks.
   (and5 (> $As 0) (> $Bs 0) (> $Cs 0) ; Avoid division by zero and ensure meaningful probabilities
         (<= $ACs (/ $Cs $As))
         (<= $BCs (/ $Cs $Bs))))

;; Truth functions

;Forward declaration
(= (STV $stv) (empty))

(: Truth_c2w (-> Number Number))
(= (Truth_c2w $c)
   (/safe $c (- 1 $c)))

(: Truth_w2c (-> Number Number))
(= (Truth_w2c $w)
   (/safe $w (+ $w 1)))

; Deduction formula: PLN book "1.4 Truth-value Formulas", page 15:
; borrowed from https://github.com/trueagi-io/hyperon-pln/blob/main/metta/pln/dependent-types/DeductionDTL.metta
(= (Truth_Deduction (stv $Ps $Pc)
                    (stv $Qs $Qc)
                    (stv $Rs $Rc)
                    (stv $PQs $PQc)
                    (stv $QRs $QRc))
   (if (and (conditional-probability-consistency $Ps $Qs $PQs)
            (conditional-probability-consistency $Qs $Rs $QRs))
       ;; Preconditions are met
       (stv (if (< 0.9999 $Qs)                  ; avoid division by 0
                ;; Qs tends to 1
                $Rs
                ;; Otherwise
                (+ (* $PQs $QRs) (/safe (* (- 1 $PQs) (- $Rs (* $Qs $QRs))) (- 1 $Qs))))
            (min $Pc (min $Qc (min $Rc 

               (*   (- (+ $PQs $QRs)  (* $PQs $QRs))    (* $PQc $QRc) )
            ))))
       ;; Preconditions are not met
       (stv 1 0)))

; Induction formula: PLN book "Appendix A: Comparison of PLN Rules with NARS Rules", page 307
(= (Truth_Induction (stv $sA $cA)
                    (stv $sB $cB)
                    (stv $sC $cC)
                    (stv $sBA $cBA)
                    (stv $sBC $cBC))
   (stv (+ (/safe (* (* $sBA $sBC) $sB) $sA)
           (* (- 1 (/safe (* $sBA $sB) $sA))
              (/safe (- $sC (* $sB $sBC)) (- 1 $sB))))
             
        (min $cA (min $cB (min $cC
           (/  (* $sBC (* $cBA $cBC))    (+ (* $sBC (* $cBA $cBC)) 1.0)  )
        )))
        
        ))

; Abduction formula: PLN book "Appendix A: Comparison of PLN Rules with NARS Rules", page 307
(= (Truth_Abduction (stv $sA $cA)
                    (stv $sB $cB)
                    (stv $sC $cC)
                    (stv $sAB $cAB)
                    (stv $sCB $cCB))
   (stv (+ (/safe (* (* $sAB $sCB) $sC)
                  $sB)
           (/safe (* $sC (* (- 1 $sAB) (- 1 $sCB)))
                  (- 1 $sB)))
        
        (min $cA (min $cB (min $cC
           (/  (* $sAB (* $cAB $cCB))    (+ (* $sAB (* $cAB $cCB)) 1.0)  )
        )))
        
        ))

;Modus Ponens: PLN book "5.7.1 Modus Ponens", page 111:
(= (Truth_ModusPonens (stv $f1 $c1) (stv $f2 $c2)) 
   (stv (+ (* $f1 $f2) (* 0.02 (- 1 $f1)))
        (* $c1 $c2))) ;min overestimates here as we do not have access to node probabilities

; SymmetricModusPonens rule   see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/symmetric-modus-ponens.scm
(= (Truth_SymmetricModusPonens (stv $sA $cA) (stv $sAB $cAB))
   (let* (($snotAB 0.2)
          ($cnotAB 1.0))
         (stv (+ (* $sA $sAB) (* (* $snotAB (negate $sA)) (+ 1.0 $sAB)))
              (min (min $cAB $cnotAB) $cA))))

;Revision: PLN Book "5.10.2 A Heuristic Revision Rule for Simple Truth-values", page 116:
(= (Truth_Revision (stv $f1 $c1) (stv $f2 $c2))
   (let* (($w1 (Truth_c2w $c1))
          ($w2 (Truth_c2w $c2))
          ($w (+ $w1 $w2))
          ($f (/safe (+ (* $w1 $f1) (* $w2 $f2)) $w))
          ($c (Truth_w2c $w)))
         (stv (min 1.0 $f)
              (min 1.0 (max (max $c $c1) $c2)))))

; negation, see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/negation-introduction.scm#L41
; negation elimination, see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/not-elimination.scm#L34
(= (Truth_Negation (stv $s $c))
   (stv (- 1.0 $s) $c))

(= (Truth_inversion (stv $Bs $Bc) (stv $ABs $ABc))
   ; confidence depends on Truth of (target) B node, which is not according to OpenCOG classic.
   ; confidence penality not according to OpenCOG classic PLN. Is weaker in this implementation.
   (stv $ABs (* $Bc (* $ABc 0.6))))

; see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/equivalence-to-implication.scm
(= (Truth_equivalenceToImplication (stv $As $Ac) (stv $Bs $Bc) (stv $ABs $ABc))
    (let* (($ConclS (if (< 0.99 (* $ABs $ABc)) ; Hack to work around the lack of distributional
                                               ; TV. If ABs is high enough, we just set $ConclS as $ABs
                        $ABs
                        ;; Formula based on PLN book formula for sim2inh
                        (/safe (* (+ 1.0 (/safe $Bs $As)) $ABs) (+ 1.0 $ABs)))))
          (stv $ConclS $ABc)))

; see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/formulas.scm#L160-L173
(= (TransitiveSimilarityStrength $sA $sB $sC $sAB $sBC)
   (let* (($T1 (/ (* (+ 1.0 (/ $sB $sA)) $sAB) (+ 1.0 $sAB)))
          ($T2 (/ (* (+ 1.0 (/ $sC $sB)) $sBC) (+ 1.0 $sBC)))
          ($T3 (/ (* (+ 1.0 (/ $sB $sC)) $sBC) (+ 1.0 $sBC)))
          ($T4 (/ (* (+ 1.0 (/ $sA $sB)) $sAB) (+ 1.0 $sAB))))
         (invert (- (+ (invert (+ (* $T1 $T2) (* (negate $T1) (/safe (- $sC (* $sB $T2)) (negate $sB)))))
                       (invert (+ (* $T3 $T4) (* (negate $T3) (/safe (- $sC (* $sB $T4)) (negate $sB)))))) 1.0))))

; see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/transitive-similarity.scm
(= (Truth_transitiveSimilarity (stv $As $Ac)
                               (stv $Bs $Bc)
                               (stv $Cs $Cc)
                               (stv $ABs $ABc)
                               (stv $BCs $BCc))
   (let* (($ConclS (TransitiveSimilarityStrength $As $Bs $Cs $ABs $BCs))
          ($ConclC (min $ABc $BCc)))
         (stv $ConclS $ConclC)))

; see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/formulas.scm
(= (simpleDeductionStrength $sA $sB $sC $sAB $sBC)
   (if (and (conditional-probability-consistency $sA $sB $sAB)
            (conditional-probability-consistency $sB $sC $sBC))
       ;; Preconditions are met
       (if (< 0.99 $sB)
           ;; sB tends to 1
           $sC
           ;; otherwise
           (+ (* $sAB $sBC) (/safe (* (- 1.0 $sAB) (- $sC (* $sB $sBC))) (- 1.0 $sB))))
       ;; Preconditions are not met
       (empty)))

; see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/evaluation-implication.scm
(= (Truth_evaluationImplication (stv $As $Ac)
                                (stv $Bs $Bc)
                                (stv $Cs $Cc)
                                (stv $ABs $ABc)
                                (stv $ACs $ACc))
   (let* (($ConclS (simpleDeductionStrength $Bs $As $Cs $ABs $ACs))
          ($ConclC (* (* 0.9 0.9)
                      (min5 $Bc $Ac $Cc $ACc (* 0.9 $ABc)))))
         (stv $ConclS $ConclC)))

;; INFERENCE RULES

;Revision
(= (|- ($T $T1) 
       ($T $T2))
   ($T (Truth_Revision $T1 $T2)))

;Modus Ponens
(= (|- ($A $T1) 
       ((Implication $A $B) $T2))
   ($B (Truth_ModusPonens $T1 $T2)))

; guard to only allow inference for certain link types
(= (SymmetricModusPonensRuleGuard Similarity) True)
(= (SymmetricModusPonensRuleGuard IntentionalSimilarity) True)
(= (SymmetricModusPonensRuleGuard ExtensionalSimilarity) True)

; SymmetricModusPonens rule, see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/symmetric-modus-ponens.scm
(= (|- ($A $TruthA)
       (($LinkType $A $B) $TruthAB))
   (if (SymmetricModusPonensRuleGuard $LinkType)
       ($B (Truth_SymmetricModusPonens $TruthA $TruthAB)) (empty)))

; guard to only allow inference for certain link types
(= (SyllogisticRuleGuard Inheritance) True)
(= (SyllogisticRuleGuard Implication) True)

; Deduction rule, see https://github.com/opencog/pln/blob/master/opencog/pln/rules/term/deduction.scm
(= (|- (($LinkType $A $B) $T1)
       (($LinkType $B $C) $T2))
   (if (SyllogisticRuleGuard $LinkType)
       (($LinkType $A $C)
        (Truth_Deduction (STV $A)
                         (STV $B)
                         (STV $C) $T1 $T2)) (empty)))
      

; Induction rule, see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/induction.scm
(= (|- (($LinkType $C $A) $T1)
       (($LinkType $C $B) $T2))
   (if (SyllogisticRuleGuard $LinkType)
       (($LinkType $A $B) (Truth_Induction (STV $A)
                                           (STV $B)
                                           (STV $C) $T1 $T2)) (empty)))

; Abduction rule, see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/abduction.scm
(= (|- (($LinkType $A $C) $T1)
       (($LinkType $B $C) $T2))
   (if (SyllogisticRuleGuard $LinkType)
       (($LinkType $A $B) (Truth_Abduction (STV $A)
                                           (STV $B)
                                           (STV $C) $T1 $T2)) (empty)))

;Usage of inheritance for predicates
;unary arg
(= (|- ((Evaluation (Predicate $x)
                    (List (Concept $C))) $T1)
       ((Inheritance (Concept $S) (Concept $C)) $T2))
   ((Evaluation (Predicate $x)
                (List (Concept $S))) (Truth_ModusPonens $T1 $T2)))

;binary arg1
(= (|- ((Evaluation (Predicate $x)
                    (List (Concept $C1) (Concept $C2))) $T1)
       ((Inheritance (Concept $S) (Concept $C1)) $T2))
   ((Evaluation (Predicate $x)
                (List (Concept $S) (Concept $C2))) (Truth_ModusPonens $T1 $T2)))

;binary arg2
(= (|- ((Evaluation (Predicate $x)
                    (List (Concept $C1) (Concept $C2))) $T1)
       ((Inheritance (Concept $S) (Concept $C2)) $T2))
   ((Evaluation (Predicate $x)
                (List (Concept $C1) (Concept $S))) (Truth_ModusPonens $T1 $T2)))

; negation elimination rule (introduction now handled by Translator), see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/not-elimination.scm#L1-L8
(= (|- ((Not $A) $T)) ($A (Truth_Negation $T)))

; Inheritance Inversion Rule, see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/inversion.scm
(= (|- ((Inheritance $A $B) $Truth))
   ((Inheritance $B $A) (Truth_inversion (STV $B) $Truth)))

; Implication Inversion Rule (same)
(= (|- ((Implication $A $B) $Truth))
   ((Implication $B $A) (Truth_inversion (STV $B) $Truth)))

; Equivalence to Implication Rule, see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/equivalence-to-implication.scm
(= (|- ((Equivalence $A $B) $Truth))
   ((Implication $A $B) (Truth_equivalenceToImplication (STV $A)
                                                        (STV $B) $Truth)))

(= (|- ((Equivalence $A $B) $Truth))
   ((Implication $B $A) (Truth_equivalenceToImplication (STV $A)
                                                        (STV $B) $Truth)))

; transitive similarity Rule, see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/transitive-similarity.scm
(= (|- ((Similarity $A $B) $T1)
       ((Similarity $B $C) $T2))
   ((Similarity $A $C) (Truth_transitiveSimilarity (STV $A)
                                                   (STV $B)
                                                   (STV $C) $T1 $T2)))

; Evaluation Implication Rule, see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/evaluation-implication.scm
(= (|- ((Evaluation $A $B) $TruthAB)
       ((Implication $A $C) $TruthAC))
   ((Evaluation $C $B) (Truth_evaluationImplication (STV $A)
                                                    (STV $B)
                                                    (STV $C) $TruthAB $TruthAC)))

; extensional - Member deduction rule, see https://github.com/opencog/pln/blob/master/opencog/pln/rules/extensional/member-deduction.scm
; we are using the truth of deduction here
(= (|- ((Member $A $B) $T1)
       ((Inheritance $B $C) $T2))
   ((Member $A $C) (Truth_Deduction (STV $A)
                                    (STV $B)
                                    (STV $C) $T1 $T2)))

;;How many derivation steps (task selections)
(= (PLN.Config.MaxSteps) 100)

;;Size of active tasks PQ
(= (PLN.Config.TaskQueueSize) 10)

;;Max beliefs buffer size
(= (PLN.Config.BeliefQueueSize) 100)

;;Whether evidence was just counted once
(= (StampDisjoint $Ev1 $Ev2)
   (== () (collapse (let* (($x (superpose $Ev1))
                           ($y (superpose $Ev2)))
                          (case (== $x $y) ((True True)))))))

;;Concat stamp with sorting
(= (StampConcat $stamp $addition)
   (if (== $addition ())
       $stamp
       (InsertionSort (TupleConcat $stamp $addition) ())))

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
   (if (< (TupleCount $L) $size)
       $L
       (let $lowestPriorityItem (BestCandidate PriorityRankNeg () $L)
            (LimitSize (Without $L $lowestPriorityItem) $size))))

;;Priority-queue based task ranking deriver with belief buffer
(= (PLN.Derive $Tasks $Beliefs $steps $maxsteps $taskqueuesize $beliefqueuesize)
   (if (or (> $steps $maxsteps) (== $Tasks ()))
       ($Tasks $Beliefs)
       (let (Sentence $x $Ev1) (BestCandidate PriorityRank () $Tasks)
            (let $derivations
                 (collapse (superpose ((let* (((Sentence $y $Ev2) (superpose $Beliefs))
                                              ($stamp (InsertionSort (TupleConcat $Ev1 $Ev2) ())))
                                             (if (StampDisjoint $Ev1 $Ev2)
                                                 (case (superpose ((|- $x $y)
                                                                   (|- $y $x)))
                                                       ((($T $TV) (Sentence ($T $TV) $stamp))))
                                                 (empty)))
                                       (case (|- $x) ((($T3 $TV3) (Sentence ($T3 $TV3) $Ev1)))))))
                 (let $temp (trace! (SELECTED $steps (Sentence $x $Ev1)) 42)
                            (PLN.Derive (LimitSize (Without (Unique (TupleConcat $Tasks $derivations) ()) (Sentence $x $Ev1)) $taskqueuesize)
                                        (LimitSize (Unique (TupleConcat $Beliefs $derivations) ()) $beliefqueuesize)
                                        (+ $steps 1)
                                        $maxsteps
                                        $taskqueuesize
                                        $beliefqueuesize))))))

(= (PLN.Derive $Tasks $Beliefs $maxsteps $taskqueuesize $beliefqueuesize)
   (PLN.Derive $Tasks $Beliefs 1 $maxsteps $taskqueuesize $beliefqueuesize))

(= (PLN.Derive $Tasks $Beliefs $maxsteps)
   (PLN.Derive $Tasks $Beliefs $maxsteps (PLN.Config.TaskQueueSize) (PLN.Config.BeliefQueueSize)))

(= (PLN.Derive $Tasks $Beliefs)
   (PLN.Derive $Tasks $Beliefs (PLN.Config.MaxSteps)))

;;Candidate evaluation based on confidence
(= (ConfidenceRank ((stv $f $c) $Ev)) $c)
(= (ConfidenceRank ()) 0)

;;Pose a question of a certain term to the system on some knowledge base
(= (PLN.Query $Tasks $Beliefs $term $maxsteps $taskqueuesize $beliefqueuesize)
   (BestCandidate ConfidenceRank () (collapse (let ($TasksRet $BeliefsRet) (PLN.Derive $Tasks $Beliefs $maxsteps $taskqueuesize $beliefqueuesize)
                                                   (case (superpose $BeliefsRet)
                                                         (((Sentence ($Term $TV) $Ev) (case (== $Term $term)
                                                                                            ((True ($TV $Ev)))))))))))

(= (PLN.Query $kb $term $maxsteps $taskqueuesize $beliefqueuesize)
   (PLN.Query $kb $kb $term $maxsteps $taskqueuesize $beliefqueuesize))

(= (PLN.Query $kb $term $maxsteps)
   (PLN.Query $kb $term $maxsteps (PLN.Config.TaskQueueSize) (PLN.Config.BeliefQueueSize))) ;default space bound

(= (PLN.Query $kb $term)
   (PLN.Query $kb $term (PLN.Config.MaxSteps))) ;default steps bound

