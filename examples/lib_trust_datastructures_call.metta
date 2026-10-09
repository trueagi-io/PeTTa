!(import! &self (library lib_datastructures))

(mystery-queue-element 2)

(= (trusted-datastructures-result)
   (enqueue
      (match &self (mystery-queue-element $value) $value)
      (queue (1) () 1)))

!(test (trusted-datastructures-result) (queue (2 1) () 2))
