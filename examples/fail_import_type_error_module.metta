; Imported by fail_import_type_error.metta. Contains a static type error, so
; both this file and any file importing it must fail compilation.
(: bad (-> Number Number))
(= (bad $x) (+ $x "not-a-number"))
