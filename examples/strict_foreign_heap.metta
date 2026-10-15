!(import! &self (library lib_builtin_types))
!(import! &self (library lib_import))

; SWI library(heaps) values are opaque Prolog terms. The type parameter tracks
; the payload type statically without inspecting the heap representation.
(: Heap (Foreign 1))

(: empty_heap (-> (Heap $value)))
(: add_to_heap (-> (Heap $value) Number $value (Heap $value)))
(: heap_peek_native (-> (Heap $value) $value))

!(import_prolog_functions_from_file "./examples/foreign_heap_helpers.pl"
                                    (empty_heap add_to_heap heap_peek_native))

; let* threads the native value through several calls and across this declared
; function boundary.
(: sample-heap (-> (Heap String)))
(= (sample-heap)
   (let* (($heap0 (empty_heap))
          ($heap1 (add_to_heap $heap0 2 "two"))
          ($heap2 (add_to_heap $heap1 1 "one")))
     $heap2))

(: heap-peek (-> (Heap $value) $value))
(= (heap-peek $heap)
   (heap_peek_native $heap))

; A separate let carries the opaque value from one declared function to
; another.
(: sample-min (-> String))
(= (sample-min)
   (let $heap (sample-heap)
     (heap-peek $heap)))

!(test (sample-min) "one")
