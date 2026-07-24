!(if (py-call (builtins.bool 1)) py-true-taken py-true-not-taken)
!(if (py-call (builtins.bool 0)) py-false-taken py-false-not-taken)
