; The result is a proper Bool because the closed Bool case is exhaustive.
; This is intentionally a shape the legacy source walker cannot certify, so a
; strict consumer in another file exercises the unified summary cache.
(: unified-cache-bool-case (-[det]-> Bool Bool))
(= (unified-cache-bool-case $value)
   (case $value
      ((true true)
       (false false))))
