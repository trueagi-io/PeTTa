; A direct parameter under a committed arrow whose type is NOMINAL is bound at
; the boundary, and every value of a nominal type is a constructor application
; (a nonempty list spine), so remove-atom/add-atom commit exactly once: det.
(: Entry Type)
(: entry (-[det]-> Number Entry))

(: remember (-[det]-> Entry Bool))
(= (remember $e) (add-atom &self $e))

(: forget (-[det]-> Entry Bool))
(= (forget $e) (remove-atom &self $e))

!(test (remember (entry 1)) true)
!(test (forget (entry 1)) true)
