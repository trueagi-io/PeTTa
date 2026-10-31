;Shared error-handling helpers: lib_roman and lib_he both need if-error, and
;loading two identical definitions made them impossible to import together.
(: if-error (-[det]-> $value $result $result $result))
(= (if-error $value $then $else)
   (if (= $value (cons Error $_)) $then $else))
