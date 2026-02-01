!(import! &self (library lib_import))
!(import_prolog_functions_from_file (library lib_snapshot.pl) (qsave_program_continuation))
!(import! &self (library lib_spaces))
!(import_prolog_function statistics)
!(import_prolog_function reset)

(= (BudgetLimit) (empty))

(= (BudgetLimitCheck)
   (if (> (- (statistics inferences) (get-state &initinferences)) (BudgetLimit))
       (translatePredicate (shift (pay-for-inferences)))
       True))

(= (ResetBudgetLimit)
   (progn (change-state! &initinferences (statistics inferences))
          (change-state! &inferenceslimit $N)))

(= (InjectBudgetLimitCheck)
   (match &self $func (let (cons = ($head $body)) $func
                           (if (not (is-member (car-atom $head)
                                               (quote (BudgetLimit BudgetLimitCheck ResetBudgetLimit InjectBudgetLimitCheck
                                                       CallWithSnapshotsInternal CallWithSnapshots ResumeComputation))))
                               (progn (remove-atom &self $func)
                                      (add-atom &self (= $head (progn (BudgetLimitCheck) $body))))))))

(= (CallWithSnapshotsInternal $predicate $var)
   (let $cont (reset $predicate $signal)
        (if (is-var $signal)
            $var
            (progn ;metadata with clause refs needs to be dropped:
                   (translatePredicate (retractall (Predicate (translated_from $1 $2))))
                   ;save current atom space with computation continuation entry point ResumeComputation:
                   (qsave_program_continuation "continue.p" $cont $var ResumeComputation)
                   (progn (ResetBudgetLimit)
                          ;continue computation:
                          (CallWithSnapshotsInternal $cont $var))))))

(: CallWithSnapshots (-> Expression Atom Atom))
(= (CallWithSnapshots $G $Limit)
   (progn (add-atom &self (= (BudgetLimit) $Limit))
          (ResetBudgetLimit)
          (collapse (InjectBudgetLimitCheck))
          (let $ext (Predicate (union-atom $G ($X)))
               (CallWithSnapshotsInternal $ext $X))))

;Function to resume computation with "swipl -x continue.p":
(= (ResumeComputation $cont $var)
   (progn (ResetBudgetLimit)
          ;Continue computation:
          (let $result (CallWithSnapshotsInternal $cont $var)
               (println! $result))
          (translatePredicate (halt))))
