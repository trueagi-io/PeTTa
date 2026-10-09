; A declared function whose definition has not arrived yet: calls compile as
; data and stay symbolic - standard MeTTa non-reduction. When the definition
; arrives later (see late_import.metta), the embedding clauses are
; retranslated into real calls.
(: late-defined-probe (-> Number Number))
(: call-late-defined-probe (-> Number %Undefined%))
(= (call-late-defined-probe $x)
   (late-defined-probe $x))
!(test (call-late-defined-probe 41) (late-defined-probe 41))
