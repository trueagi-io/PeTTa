; A late declaration makes the second old clause invalid. Revalidation stages
; both replacements before erasing either old clause, so catching the error
; leaves the complete prior executable clause set rather than only clause b.
(= (staged-choice a) 1)
(= (staged-choice b) "old")

!(let $ignored
   (catch
      (eval
         (quote
            (add-atom &self
               (: staged-choice (-[det]-> Atom Number))))))
   ; The failed declaration and its origin are rolled back automatically, and
   ; both old executable clauses remain live.
   (test (collapse (staged-choice $which)) (1 "old")))
