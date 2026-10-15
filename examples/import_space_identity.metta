!(bind! &import-space-a (new-space))
!(bind! &import-space-b (new-space))

!(import! &import-space-a imports/overhaul/space_payload)
!(import! &import-space-b imports/overhaul/space_payload)

!(test (collapse (match &import-space-a (import-space-marker) present)) (present))
!(test (collapse (match &import-space-b (import-space-marker) present)) (present))
