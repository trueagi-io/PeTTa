!(import! &self (library lib_builtin_types))
!(import! &self (library lib_import))

; Opaque heap values carry their payload parameter only in the type layer.
(: Heap (Foreign 1))
(: Goal Type)
(: CPU (-> Symbol Goal))

(: empty_heap (-> (Heap $value)))
(: add_to_heap (-> (Heap $value) Number $value (Heap $value)))
(: heap_pop_native (-> (Heap $value)
                       (Number $value (Heap $value))))

!(import_prolog_functions_from_file
    "./examples/foreign_heap_helpers.pl"
    (empty_heap add_to_heap heap_pop_native))

; These concrete parameters force every destructured field to be known:
; priority:Number, item:Goal, and rest:(Heap Goal). If heap_pop_native's $value
; were not instantiated from its (Heap Goal) argument, strict mode would leave
; residual guards at this call.
(: consume-pop-fields (-> Number Goal (Heap Goal) Goal))
(= (consume-pop-fields $priority $item $rest)
   (let $_checked-priority (+ $priority 0)
     (let $_checked-rest (add_to_heap $rest 4 $item)
       $item)))

(: pop-goal (-> (Heap Goal) Goal))
(= (pop-goal $heap)
   (let ($priority $item $rest) (heap_pop_native $heap)
     (consume-pop-fields $priority $item $rest)))

(: sample-popped-goal (-> Goal))
(= (sample-popped-goal)
   (let* (($heap0 (empty_heap))
          ($heap1 (add_to_heap $heap0 3 (CPU op))))
     (pop-goal $heap1)))

!(test (sample-popped-goal) (CPU op))
