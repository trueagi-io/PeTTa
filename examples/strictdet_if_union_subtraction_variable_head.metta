!(import! &self (library lib_builtin_types))

; The first equality-as-case consumes CpuGoal. Its fallthrough is the
; positional FactRow product; the second inert variable-headed tuple can
; therefore bind KBContext's structural fields by position.
(: KB (Newtype Expression))
(: KBContext (Newtype (KB Expression Expression)))

(: CpuGoal Type)
(: CPU (-[det]-> Atom Expression Atom CpuGoal))
(: FactRow (Alias (Atom KBContext Atom Atom)))
(: WorkItem (Alias (| CpuGoal FactRow)))
(: Fact (-[det]-> KBContext Atom Atom FactRow))

(: item-kbid (-[det]-> WorkItem KB))
(= (item-kbid $item)
   (if (= $item (CPU $_fun $_args $_result))
      (brand KB cpu)
      (if (= $item ($_type ($kbid $_ctx $_vars) $_proof $_tv))
         $kbid
         (brand KB missing))))

!(test (item-kbid (CPU op args result)) cpu)
!(test
   (item-kbid
      (Fact
         (brand KBContext (data (brand KB row-kb) context variables))
         proof tv))
   row-kb)
