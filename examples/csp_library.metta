!(import! &self (library lib_csp))

; -------- Assignment --------

!(test (csp-assignment-opt ((9 2 7)
                            (6 4 3)
                            (5 8 1)))
       (assignment-solution 9 ((assign 0 1) (assign 1 0) (assign 2 2))))

!(test (csp-assignment-topk 2 ((9 2 7)
                               (6 4 3)
                               (5 8 1)))
       ((assignment-solution 9 ((assign 0 1) (assign 1 0) (assign 2 2)))
        (assignment-solution 10 ((assign 0 1) (assign 1 2) (assign 2 0)))))

; -------- Timetable --------

!(test (csp-timetable-solve ((class c1 t1 g1 35)
                             (class c2 t1 g2 25)
                             (class c3 t2 g1 20))
                            ((room r1 40)
                             (room r2 30))
                            (0 1 2)
                            ((teacher t1 2)
                             (group g1 0)))
       (timetable-solution ((at c1 1 r1)
                            (at c2 0 r1)
                            (at c3 2 r1))))
