!(import! &self (library lib_datastructures))

!(test (empty-queue) (queue () () 0))
!(test (enqueue 1 (empty-queue)) (queue (1) () 1))
!(test
   (let $next (dequeue $item (queue () (2) 1))
        ($item $next))
   (2 (queue () () 0)))
