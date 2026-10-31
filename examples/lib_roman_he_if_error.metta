; lib_roman and lib_he both define a deterministic if-error;
; importing both must work.
!(import! &self (library lib_roman))
!(import! &self (library lib_he))
!(test (if-error 1 a b) b)
!(test (if-error (Error failed) a b) a)
