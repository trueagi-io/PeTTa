!(import! &self (library lib_spaces))

; migrateAtoms moves the atoms matching a pattern from one space to another.
!(bind! &migrate-from (new-space))
!(bind! &migrate-to (new-space))
!(add-atom &migrate-from (item 1))
!(add-atom &migrate-from (item 2))
!(add-atom &migrate-from (other 3))
!(migrateAtoms &migrate-from &migrate-to (item $x))

!(test (collapse (match &migrate-to (item $x) $x)) (1 2))
!(test (collapse (match &migrate-from (item $x) $x)) ())
!(test (collapse (match &migrate-from (other $x) $x)) (3))
!(test (collapse (match &migrate-to (other $x) $x)) ())
