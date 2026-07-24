; lib_he must be fully importable under --strict: every function is declared,
; branch-polymorphic helpers with an explicitly dynamic (%Undefined%) output.
!(import! &self (library lib_builtin_types))
!(import! &self (library lib_he))

!(test (if-equal 1 1 yes no) yes)
!(test (if-equal 1 2 yes "hetero") "hetero")
!(test (assertEqual 3 3) True)
!(test (is-function (-> Number Number)) True)
!(test (if-error (catch (/ 1 0)) True False) True)
