; Both fields are inferred; the callee keeps native guard-free arithmetic.
(= (tvf2 (stv $s $c)) (* $s $c))
!(test (tvf2 (stv 2 3)) 6)

; An atom with no known type is checked at the call, before native arithmetic.
!(test (repr (catch (tvf2 (stv x 3))))
       "(Error (literal_type_mismatch (stv x 3) (stv Number Number)) typecheck)")

(: a A)
(: b B)
(: only-a (-> A %Undefined%))
(= (only-a $x) (ok $x))
(= (fa (pair $a $unused)) (only-a $a))

; Space reads without a schema have unknown actual field types.
!(add-atom &self (bad-pair-value b))
!(add-atom &self (good-pair-value a))
; Unknown actual fields also get caller checks against partial shapes.
!(test (repr (catch (fa (pair (match &self (bad-pair-value $x) $x) 1))))
       "(Error (literal_type_mismatch (pair b 1) (pair A %Undefined%)) typecheck)")
!(test (fa (pair (match &self (good-pair-value $x) $x) 1)) (ok a))

; An actual partial type also leaves its wildcard field unresolved. It must
; not certify the complete shape tvf2 needs through wildcard compatibility.
(: partial-stv-caller (-> (stv %Undefined% Number) %Undefined%))
(= (partial-stv-caller $tv) (tvf2 $tv))
!(test (partial-stv-caller (stv 2 3)) 6)
!(test (repr (catch (partial-stv-caller (stv x 3))))
       "(Error (literal_type_mismatch (stv x 3) (stv Number Number)) typecheck)")

; Incomplete constructed outputs must not become positive type evidence.
(= (make-unknown-stv $x) (stv $x 3))
!(test (repr (catch (tvf2 (make-unknown-stv x))))
       "(Error (literal_type_mismatch (stv x 3) (stv Number Number)) typecheck)")

; Literal manual dispatch follows the same inferred field obligations.
!(test (repr (catch (reduce (tvf2 (stv x 3)))))
       "(Error (literal_type_mismatch (stv x 3) (stv Number Number)) typecheck)")
