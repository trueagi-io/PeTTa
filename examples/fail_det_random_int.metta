; random_between/3 FAILS when Max < Min (it does not raise, and it does not
; pick anything). (random-int 5 1) therefore has zero solutions, which is
; exactly what -[det]-> promises does not happen, so random-int is semidet and
; a -[det]-> body may not call it. The table used to say det, and this program
; compiled clean and then silently produced nothing at all - the caller
; disappeared, and execution carried on with the next form.
(: rr (-[det]-> Number))
(= (rr) (random-int 5 1))

!(rr)
