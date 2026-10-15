!(import! &self (library lib_builtin_types))

(: Heap (Foreign 1))

(: heap-size-placeholder (-> (Heap Number) Number))
(= (heap-size-placeholder $_heap) 0)

; A Number is not an opaque Heap value; no runtime guard can repair this.
(: wrong-heap-call (-> Number))
(= (wrong-heap-call)
   (heap-size-placeholder 42))
