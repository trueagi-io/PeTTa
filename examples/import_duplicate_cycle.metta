!(import! &self imports/overhaul/duplicate)
!(import! &self ./imports/overhaul/./duplicate.metta)
!(import! &self imports/overhaul/cycle_a)

!(test (collapse (duplicate-import-result)) (loaded-once))
!(test (cycle-a) a)
!(test (cycle-b) b)
