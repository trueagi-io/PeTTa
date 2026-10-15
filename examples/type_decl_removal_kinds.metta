; Alias removal reconstructs dependent stores from their surviving raw
; declarations. `token` and accept-alias therefore return to the opaque
; AliasGone name instead of retaining the stale Number expansion.
(: AliasGone (Alias Number))
(: token AliasGone)
(: accept-alias (-> AliasGone AliasGone))
(= (accept-alias $x) $x)
!(remove-atom &self (: AliasGone (Alias Number)))
!(test (accept-alias token) token)

; Removing a Newtype or Foreign declaration frees the name for another kind.
(: ReusedNewtype (Newtype String))
!(remove-atom &self (: ReusedNewtype (Newtype String)))
!(add-atom &self (: ReusedNewtype (Alias Number)))
(: accept-reused-newtype (-> ReusedNewtype Number))
(= (accept-reused-newtype $x) $x)
!(test (accept-reused-newtype 3) 3)

(: ReusedForeign (Foreign))
!(remove-atom &self (: ReusedForeign (Foreign)))
!(add-atom &self (: ReusedForeign (Alias Number)))
(: accept-reused-foreign (-> ReusedForeign Number))
(= (accept-reused-foreign $x) $x)
!(test (accept-reused-foreign 4) 4)

; Removing SpaceOf withdraws row checking from the named space.
(: &jobs (SpaceOf (job Number)))
!(remove-atom &self (: &jobs (SpaceOf (job Number))))
!(add-atom &jobs (job "open"))
!(test (match &jobs (job $x) $x) "open")
