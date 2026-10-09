!(import! &self (library lib_builtin_types))

(: &jobs (SpaceOf (job-row Number String)))

; A literal String in the Number field is definitely incompatible.
!(add-atom &jobs (job-row "wrong" "compile"))
