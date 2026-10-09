(: &schema-jobs (SpaceOf (job-row Number)))
!(add-atom &schema-jobs (job-row 1))

(: schema-read (-> Number))
(= (schema-read) (match &schema-jobs (job-row $n) $n))

; Removing the schema invalidates and recompiles schema-read. Strict mode must
; reject the output obligation that is no longer statically certified.
!(remove-atom &self (: &schema-jobs (SpaceOf (job-row Number))))
