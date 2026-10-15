; A space that was never written reads as empty, and the read creates it:
; reading it repeatedly costs no more than reading a written space.

(= (reads $space $n) (if (== $n 0) 0 (let $r (collapse (match $space (foo $x) $x)) (reads $space (- $n 1)))))
(= (time-reads $space) (let $t0 (current-time) (let $_ (reads $space 5000) (- (current-time) $t0))))

!(add-atom &written (bar 1))
!(test (< (time-reads &unwritten) (* 10 (+ 0.001 (time-reads &written))))
       True)

!(test (collapse (match &unwritten (foo $x) $x))
       ())
!(test (collapse (get-atoms &unwritten))
       ())
!(test (remove-atom &unwritten (foo 1))
       true)
!(add-atom &unwritten (foo 1))
!(test (collapse (match &unwritten (foo $x) $x))
       (1))
