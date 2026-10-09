!(import! &self (library lib_builtin_types))
!(import! &self (library lib_he))

(: a A)
(: b B)
(: aa (-> A A))
(= (aa a) a)

; catch produces its argument's type or an (Error Detail Ctx) expression -
; neither is ever a B, so the declared output conflicts at compile time
; (this was only a runtime failure before catch results were typed):
(: bad-catch (-> A B))
(= (bad-catch $x) (catch (aa $x)))
