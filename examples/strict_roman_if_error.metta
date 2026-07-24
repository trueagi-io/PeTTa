!(import! &self (library lib_roman))

!(test (if-error 42 0 42) 42)
!(test (if-error (Error failed) 0 42) 0)
!(test (if-error true false true) true)
!(test (if-error (Error failed) false true) false)
