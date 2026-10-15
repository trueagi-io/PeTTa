!(import! &self (library lib_builtin_types))

; Structural newtype representations propagate field types into ordinary
; clause-head destructuring, just as they do for let/case/match patterns.
(: KB (Newtype Expression))
(: KBContext (Newtype (KB Expression Expression)))

(: kb-context-kbid (-[det]-> KBContext KB))
(= (kb-context-kbid ($kbid $_ctx $_vars)) $kbid)

!(test
   (kb-context-kbid
      (brand KBContext ((brand KB main-kb) context variables)))
   main-kb)

; Representation recursion is transitive: NestedKBContext expands through
; KBContext before the positional fields are bound.
(: NestedKBContext (Newtype KBContext))

(: nested-kb-context-kbid (-[det]-> NestedKBContext KB))
(= (nested-kb-context-kbid ($kbid $_ctx $_vars)) $kbid)
