(: JobValue (Alias Number))
(: &alias-schema-jobs (SpaceOf (job JobValue)))

(: read-alias-job (-[nondet]-> Number))
(= (read-alias-job)
   (match &alias-schema-jobs (job $value) $value))

; Rebuilding the SpaceOf declaration after this removal must notify its
; consumers.  The result is no longer statically Number, so strict mode
; rejects the recompiled read-alias-job rather than retaining a stale proof.
!(remove-atom &self (: JobValue (Alias Number)))
