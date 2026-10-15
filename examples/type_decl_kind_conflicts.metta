; The first declaration of a type name wins. Conflicting Newtype
; representations and cross-kind redeclarations are warned and ignored in
; either order.
(: Stable (Newtype Number))
(: Stable (Newtype String))
!(remove-atom &self (: Stable (Newtype Number)))
!(add-atom &self (: Stable (Alias Number)))
(: use-stable (-> Stable Number))
(= (use-stable $x) $x)
!(test (use-stable 7) 7)

(: AliasFirst (Alias Number))
(: AliasFirst (Newtype String))
(: use-alias-first (-> AliasFirst Number))
(= (use-alias-first $x) $x)
!(test (use-alias-first 8) 8)

(: NominalFirst (Newtype Number))
(: NominalFirst (Alias String))
!(remove-atom &self (: NominalFirst (Newtype Number)))
!(add-atom &self (: NominalFirst (Alias Number)))
(: use-nominal-first (-> NominalFirst Number))
(= (use-nominal-first $x) $x)
!(test (use-nominal-first 9) 9)
