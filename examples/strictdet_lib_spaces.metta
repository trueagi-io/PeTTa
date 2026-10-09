!(import! &self (library lib_spaces))

(strictdet-lib-spaces fact)
!(test (succeedsPredicate (&self strictdet-lib-spaces fact)) True)
