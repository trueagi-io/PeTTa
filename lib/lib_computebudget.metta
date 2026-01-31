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
                                                       CallWithBudgetLimitInternal CallWithBudgetLimit ResumeComputation))))
                               (progn (remove-atom &self $func)
                                      (add-atom &self (= $head (progn (BudgetLimitCheck) $body))))))))

(= (CallWithBudgetLimitInternal $predicate $var)
   (let $cont (reset $predicate $signal)
        (if (is-var $signal)
            $var
            (progn ;metadata with clause refs needs to be dropped:
                   (translatePredicate (retractall (Predicate (translated_from $1 $2))))
                   ;save current atom space with computation continuation entry point ResumeComputation:
                   (qsave_program_continuation "continue.p" $cont $var ResumeComputation)
                   (println! "Out of compute budget. Do you want to continue now, or later? Answer (yes) or (no):")
                   (let $response (catch (readln!))
                        (if (== $response (yes))
                            (progn (ResetBudgetLimit)
                                   ;continue computation:
                                   (CallWithBudgetLimitInternal $cont $var))
                            (Ok bye and execute "swipl -x continue.p" to continue)))))))

(: CallWithBudgetLimit (-> Expression Atom))
(= (CallWithBudgetLimit $G)
   (let $ext (Predicate (union-atom $G ($X)))
        (CallWithBudgetLimitInternal $ext $X)))

;Function to resume computation with "swipl -x saveme.p":
(= (ResumeComputation $cont $var)
   (progn (ResetBudgetLimit)
          ;Continue computation:
          (let $result (CallWithBudgetLimitInternal $cont $var)
               (println! $result))
          (translatePredicate (halt))))
