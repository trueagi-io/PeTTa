; Conforming and open pre-existing rows are accepted by a later schema.
!(add-atom &jobs (job 7))
!(add-atom &jobs (job $runtime-filled))
(: &jobs (SpaceOf (job Number)))

; The open row also matches 7 once its field is supplied by the pattern.
!(test (collapse (match &jobs (job 7) 7)) (7 7))
