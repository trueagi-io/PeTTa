!(import! &self (library lib_builtin_types))

; A static space schema may be a union of tagged structural row shapes.
(: &jobs (SpaceOf (| (job-row Number String)
                     (note-row String))))

!(add-atom &jobs (job-row 4 "compile"))
!(add-atom &jobs (job-row 8 "verify"))
!(add-atom &jobs (note-row "queued"))

; The tag selects one union member, so $id is Number without an ascription.
(: next-job-id (-> Number Number))
(= (next-job-id $wanted)
   (match &jobs (job-row $wanted $_label) (+ $wanted 1)))

!(test (next-job-id 4) 5)

; The pattern field type also flows through the match result and collapse.
(: job-ids (-> (List Number)))
(= (job-ids)
   (collapse (match &jobs (job-row $id $_label) $id)))

!(test (job-ids) (4 8))

; An undeclared space keeps the existing permissive match/add behavior.
!(add-atom &loose (loose-row raw))
!(test (match &loose (loose-row $x) $x) raw)
