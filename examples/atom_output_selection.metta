; The cached plain-arrow declaration works in either source order.
(: held_before (-> Number Atom))
(= (held_before $x) (+ $x 1))
!(test (held_before 5) (quote (+ 5 1)))
(= (held_after $x) (+ $x 1))
(: held_after (-> Number Atom))
!(test (held_after 5) (quote (+ 5 1)))

; Each source arity selects its own result mode, including zero arguments.
(= (atom_pick $x $y) (+ $x $y))
(= (atom_pick $x) (+ $x 1))
(: atom_pick (-> Number Atom))
(: atom_pick (-[det]-> Number Number Atom))
!(test (atom_pick 5) (quote (+ 5 1)))
!(test (atom_pick 5 6) 11)
(: atom_zero (-> Atom))
(: atom_zero (-[det]-> Number Atom))
(= (atom_zero) (+ 1 2))
(= (atom_zero $x) (+ $x 1))
!(test (atom_zero) (quote (+ 1 2)))
!(test (atom_zero 5) 6)

; Overloads at the same arity must also agree with the selected clause.
(: mixed_same (-> Number Atom))
(: mixed_same (-[det]-> String Atom))
(= (mixed_same 5) (+ 5 1))
(= (mixed_same "hi") (+ 1 2))
!(test (mixed_same 5) (quote (+ 5 1)))
!(test (mixed_same "hi") 3)
(: both_held (-> Number Atom))
(: both_held (-> String Atom))
(= (both_held $x) (+ $x 1))
!(test (both_held 5) (quote (+ 5 1)))
!(test (both_held "hi") (quote (+ "hi" 1)))

; Specialization inherits the hoisted plain-arrow mode before its row is added.
(= (atom_inc $x) (+ $x 1))
(= (ho_held $f $x) ($f $x))
(= (caller_held $x) (ho_held atom_inc $x))
(: ho_held (-> (-> Number Number) Number Atom))
!(test (ho_held atom_inc 5) (quote (atom_inc 5)))
!(test (caller_held 5) (quote (atom_inc 5)))

; Analyzers must count the quoted form as one datum, even if it names empty.
(: held_empty (-> Atom))
(= (held_empty) (empty))
(: calls_empty (-[det]-> Number Atom))
(= (calls_empty $x) (held_empty))
!(test (calls_empty 5) (quote (empty)))

; Cached aliases and live declaration changes retain their existing behavior.
(: SyntaxResult (Alias Atom))
(: alias_held (-> Number SyntaxResult))
(= (alias_held $x) (+ $x 1))
!(test (alias_held 5) (quote (+ 5 1)))
(= (live_held $x) (+ $x 1))
!(test (live_held 5) 6)
!(add-atom &self (: live_held (-> Number Atom)))
!(test (live_held 5) (quote (+ 5 1)))
!(remove-atom &self (: live_held (-> Number Atom)))
!(test (live_held 5) 6)

; Variable bodies preserve identity and fresh variables during analysis.
(: id_poly (-> $a Atom))
(= (id_poly $x) $x)
!(test (id_poly 42) 42)
!(test (id_poly "hi") "hi")
(: id_num (-> Number Atom))
(= (id_num $x) $x)
!(test (id_num 42) 42)
(: fresh_result (-> Number Atom))
(= (fresh_result $x) $y)
!(test (is-var (fresh_result 42)) true)

; Explicit quoting of a variable also preserves the caller's value.
(: quoted_var (-[det]-> Atom Atom))
(= (quoted_var $x) (quote $x))
!(test (quoted_var 42) 42)
!(test (quoted_var false) false)
!(test (is-var (quoted_var $free)) true)

; Select the result mode before functional head patterns become value slots.
(: numeric_head (-> Number Number))
(= (numeric_head $x) $x)
(: held_head (-> Number Atom))
(: held_head (-[det]-> String Atom))
(= (held_head (numeric_head $x)) (== $x 4))
!(test (held_head 4) (quote (== 4 4)))
!(test (held_head 5) (quote (== 5 4)))

; Higher-order names inside quoted bodies remain data under functional heads.
(: ho_head (-> Number (-> Number Number) Atom))
(: ho_head (-[det]-> String (-> Number Number) Atom))
(= (ho_head (numeric_head $x) $f) ($f $x))
!(test (ho_head 4 atom_inc) (quote (atom_inc 4)))

; Specialization must not narrow the source head used to choose a result mode.
(: mode_inc (-> Number Number))
(= (mode_inc $x) (+ $x 1))
(: mode_str (-> String String))
(= (mode_str $s) $s)
(: mode_over (-> (-> Number Number) Number Atom))
(: mode_over (-[det]-> (-> String String) String Atom))
(= (mode_over $f $x) ($f $x))
!(test (mode_over mode_inc 5) 6)
!(test (let $fn mode_inc (mode_over $fn 5)) 6)
!(test (mode_over mode_str "hi") "hi")
!(test (let $fn mode_str (mode_over $fn "hi")) "hi")

; Literal heads still select the plain and committed overloads respectively.
(: mode_typed (-> (-> Number Number) Number Atom))
(: mode_typed (-[det]-> (-> String String) String Atom))
(= (mode_typed $f 5) ($f 5))
(= (mode_typed $f "hi") ($f "hi"))
!(test (mode_typed mode_inc 5) (quote (mode_inc 5)))
!(test (let $fn mode_inc (mode_typed $fn 5)) (quote (mode_inc 5)))
!(test (mode_typed mode_str "hi") "hi")
!(test (let $fn mode_str (mode_typed $fn "hi")) "hi")

; Unresolved rows must not invent a plain-arrow result mode or hang compilation.
(: $unresolved_target $unresolved_type)
(: still_valued (-[det]-> Number Atom))
(= (still_valued $x) (+ $x 1))
!(test (still_valued 5) 6)
