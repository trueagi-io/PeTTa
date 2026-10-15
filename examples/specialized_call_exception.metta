; A specialized call behaves like the generic call it replaces: an exception
; raised inside the callee reaches the caller's catch whether the callee is
; known when invoke is compiled (invoke is specialized for it) or only at
; runtime (the generic invoke applies it through apply_fn1).
(= (bad $x) (/ $x 0))
(= (invoke $f) ($f 1))
(callee bad)

!(test (let (Error $_e $_ctx) (catch (invoke bad)) caught) caught)
!(test (let (Error $_e $_ctx) (catch (match &self (callee $f) (invoke $f))) caught) caught)
