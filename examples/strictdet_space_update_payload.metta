; add-atom and remove-atom store their payload raw, so under --strict-det
; a row is data whatever its first field: a variable there is stored, not
; called, and the update stays det.
(: Key Type)
(: key (-[det]-> Number Key))
(: tag-of (-[det]-> Key Atom))
(= (tag-of $_key) tagged)

!(bind! &flags (new-space))

; A row whose first field is a typed parameter.
(: set-flag (-[det]-> Key Bool Bool))
(= (set-flag $k $on)
   (let $_new (add-atom &flags ($k $on)) $on))

(: clear-flag (-[det]-> Key Bool Bool))
(= (clear-flag $k $on)
   (let $_old (remove-atom &flags ($k $on)) $on))

; A row whose first field is an Atom-typed variable.
(: put-head (-[det]-> Key Atom))
(= (put-head $key)
   (let* (($head (tag-of $key))
          ($_row (add-atom &flags ($head $key))))
      ()))

!(set-flag (key 1) true)
!(set-flag (key 2) false)
!(clear-flag (key 2) false)
!(test (collapse (match &flags ((key $n) $on) ($n $on))) ((1 true)))

!(put-head (key 3))
!(test (collapse (match &flags (tagged $k) $k)) ((key 3)))
