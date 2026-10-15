; Two -[semidet]-> calls feed a -[det]-> call whose pair result is destructured
; by (let ($left $right) ... ($left $right)). The semidet arguments make the
; whole thing may_fail, which -[semidet]-> accepts. The reconstruction body
; ($left $right) is data, not a dynamic dispatch: $left/$right are fields of
; add-pair's declared (Number Number) output, so in well-typed code they are
; numbers, never function symbols, and reduce/2 leaves such a head unevaluated -
; exactly one solution. let_determinism/4 binds those field types before
; analysing the body so the var-head reading is data construction, not unknown.

(: maybe-number (-[semidet]-> Number Number))
(= (maybe-number $x)
   (if (> $x 0) $x (empty)))

(: add-pair (-[det]-> Number Number (Number Number)))
(= (add-pair $a $b) ($a $b))

(: through-nested-calls (-[semidet]-> Number (Number Number)))
(= (through-nested-calls $x)
   (let ($left $right)
      (add-pair (maybe-number $x) (maybe-number $x))
      ($left $right)))

!(test (through-nested-calls 1) (1 1))
!(test (collapse (through-nested-calls 0)) ())
