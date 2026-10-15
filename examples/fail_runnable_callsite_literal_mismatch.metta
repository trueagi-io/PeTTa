!(import! &self (library lib_builtin_types))

(: sid (-> String String))
(= (sid $x) $x)

!(sid 7)
