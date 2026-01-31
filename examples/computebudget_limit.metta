!(import! &self (library lib_computebudget))

;Functions as usual:
(= (fib-tr $n $a $b)
   (if (== $n 0)
       $a
       (fib-tr (- $n 1) $b (+ $a $b))))

(= (fib $n)
   (fib-tr $n 0 1))

;Setup budget:
(= (BudgetLimit) 300)
!(ResetBudgetLimit)
!(InjectBudgetLimitCheck)

;3. Start computation:
!(CallWithBudgetLimit (fib 100))
