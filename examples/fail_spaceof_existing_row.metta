; A SpaceOf declaration validates rows that predate the schema.
!(add-atom &jobs (job "oops"))
(: &jobs (SpaceOf (job Number)))
