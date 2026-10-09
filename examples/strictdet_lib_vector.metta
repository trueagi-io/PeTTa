!(import! &self (library lib_vector))

!(test (dot (1.0 2.0) (3.0 4.0)) 11.0)
!(test (norm (3.0 4.0)) 5.0)
