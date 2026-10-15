;;; EFFICIENT AMORTIZED O(1) OP FUNCTIONAL QUEUE ;;;

;The builtin reverse can enumerate when its input is open.  Keep that honest
;at this internal boundary while retaining the element type in strict mode.
(: queue-reverse (-[nondet]-> (List $a) (List $a)))
(= (queue-reverse $items)
   (the (List $a) (reverse $items)))

;Enqueue adds 1 to N
(: enqueue
   (-[det]-> $a
              (queue (List $a) (List $a) Number)
              (queue (List $a) (List $a) Number)))
(= (enqueue $E (queue $In $Out $N))
   (queue (cons $E $In) $Out (+ $N 1)))

;Normal dequeue subtracts 1
(: dequeue
   (-[nondet]-> $a
                 (queue (List $a) (List $a) Number)
                 (queue (List $a) (List $a) Number)))
(= (dequeue $E (queue $In (cons $E $Out) $N))
   (queue $In $Out (- $N 1)))

;When out empty, reverse in and subtract 1
(= (dequeue $E (queue $In () $N))
   (let (cons $E $R) (queue-reverse $In)
        (queue () $R (- $N 1))))

;Empty queue
(: empty-queue
   (-[det]-> (queue (List $a) (List $a) Number)))
(= (empty-queue) (queue () () 0))

;;; EFFICIENT O(1) LOOKUP SPACE ;;;

;Fast Hashmap-like existence check via repra:
(: add-unique-or-fail
   (-[semidet]-> %Undefined% Expression Bool))
(= (add-unique-or-fail $space $Expression)
   (let $st (s (repra $Expression))
        (if (== () (collapse (once (match $space $st True))))
            (add-atom $space $st)
            (empty))))
