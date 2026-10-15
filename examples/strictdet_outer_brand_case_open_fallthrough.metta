!(import! &self (library lib_builtin_types))

; An explicit brand over a case result replaces every representation-compatible
; branch candidate with the nominal result type. Expression is an open
; representation, so both the CPU branch and the raw fallthrough are valid
; Proof payloads.
(: Proof (Newtype Expression))

(: Goal Type)
(: CPU (-[det]-> Atom Expression Atom Goal))
(: FactRow (Alias (Atom Atom Atom Atom)))
(: WorkItem (Alias (| Goal FactRow)))

(: proof-from-item (-[det]-> WorkItem Proof))
(= (proof-from-item $item)
   (brand Proof
      (case $item (
         ((CPU $_fun $_args $_result) cpu)
         ($_other $item)))))

!(test (proof-from-item (CPU op args result)) cpu)
!(test (proof-from-item (fact context proof tv))
       (fact context proof tv))
