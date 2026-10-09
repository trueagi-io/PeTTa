; Multi-file soundness case, part 2 of 2 - load AFTER
; late_ctor_exhaustive_1_ranks.metta.
;
; A third Colour, arriving after rank was already certified exhaustive. Under
; the old snapshot rule nothing revisited that verdict: (rank Blue) produced
; zero solutions for a function declared to produce exactly one, and only
; --oracle-det noticed.
;
; The verdict is now recorded with the constructor set it saw, and this
; declaration - not the call below - is what re-runs it. The error is reported
; against the clause that made the claim, in the file that made it, because
; that is what has to change: cover Blue, or say -[semidet]->.

(: Blue Colour)

!(test (rank Blue) 0)
