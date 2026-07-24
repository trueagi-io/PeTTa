
(= (find-divisor $n $test-divisor)
   (if (> (* $test-divisor $test-divisor) $n)
       $n
       (if (== 0 (% $n $test-divisor)) 
           $test-divisor
           (find-divisor $n (+ $test-divisor 1)))))

(= (prime? $n)
   (== $n (find-divisor $n 2)))

;hyperpose should work with a list passed through a variable
!(test (msort (collapse (let $xs (3 1 2) (hyperpose $xs))))
       (1 2 3))

;multi-threaded concurrency running each option till the bitter end:
!(test (collapse (hyperpose ((prime? 5353725700019)    ;cheap
                             (prime? 5378181100003)    ;cheap
                             (prime? 5421844300001)    ;cheap
                             (prime? 5473443100001)))) ;cheap => cheap overall
       (True True True True))

;multi-threaded concurrency running only until the first one succeeds:
!(test (once (hyperpose ((prime? 535372570000000063)    ;expensive
                         (prime? 537818110000000001)    ;expensive
                         (prime? 5421844300001)         ;cheap
                         (prime? 547344310000000013)))) ;expensive => cheap overall
       True)
