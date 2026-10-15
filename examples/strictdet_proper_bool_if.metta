; Both if branches construct an actual Bool.  That producer result can feed a
; Boolean builtin without re-inspecting its implementation at the call site.
(: bool-from-if (-[det]-> Bool Bool))
(= (bool-from-if $choice)
   (if $choice true false))

(: consume-if-bool (-[det]-> Bool Bool))
(= (consume-if-bool $choice)
   (and (bool-from-if $choice) true))

!(test (consume-if-bool true) true)
!(test (consume-if-bool false) false)
