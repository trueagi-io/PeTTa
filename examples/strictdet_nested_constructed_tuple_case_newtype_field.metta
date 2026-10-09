!(import! &self (library lib_builtin_types))

; A case scrutinee constructed from typed parameters has a positional product
; type. Deep destructuring uses that product before expanding KBContext's
; structural newtype representation, so the selected field retains KB.
(: KB (Newtype Expression))
(: KBContext (Newtype (KB Expression Expression)))
(: Proof (Newtype Expression))
(: Statement (Newtype Expression))
(: TV (Alias Atom))

(: Goal Type)
(: : (-[det]-> KBContext Proof Statement TV Goal))

(: nested-kbid (-[det]-> (List Goal) Goal KB))
(= (nested-kbid $premises $conclusion)
   (case ($premises $conclusion) (
      ((((: ($kbid $_ctx $_vars) $_proof $_type $_tv))
        (: ($kbid $_ctx2 $_vars2) $_proof2 $_type2 $_tv2))
         $kbid))))

!(test
   (nested-kbid
      (make-list
         (: (brand KBContext
                 (data (brand KB premise-kb) premise-context premise-vars))
            premise-proof premise-statement premise-tv))
      (: (brand KBContext
              (data (brand KB premise-kb) conclusion-context conclusion-vars))
         conclusion-proof conclusion-statement conclusion-tv))
   premise-kb)
