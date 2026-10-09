; Multi-file --oracle-det case, part 2 of 2 - load AFTER late_det_decl_1_defs.metta.
;
; The determinism declaration arrives after the definition it constrains.
; process_metta_string/3's declaration prepass makes a file's declarations
; visible to every definition IN THAT FILE, which is why this cannot be
; reproduced in one file - the overlap check would fire immediately. Across
; files there was no such prepass: pick's clauses were already compiled and
; validated, nothing re-validated them, and the compiler warned and then
; believed the declaration anyway. The call below had two solutions for a
; function declared to have exactly one, and --no-det-cut could not see it
; either - the clauses were compiled before the declaration existed, so they
; never got a commit cut to remove.
;
; A declaration that arrives late now RECOMPILES the clauses it constrains
; (enforce_late_declaration/1), which is exactly what the in-file prepass
; would have done had the two files been one. Recompiled with the declaration
; in place, pick's two variable-free heads overlap, and overlapping clauses
; under a -[det]-> promise are the ordinary static error they have always
; been. Enforced, or rejected - never believed unchecked.
(: pick (-[det]-> Number))

!(test (collapse (pick)) (1))
