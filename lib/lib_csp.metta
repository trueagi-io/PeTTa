; Import helper functions for loading Prolog-backed predicates
!(import! &self (library lib_import))

; Import CSP backend predicates from Prolog
!(import_prolog_functions_from_file (library lib_csp.pl)
    (csp_assignment_opt csp_assignment_topk csp_timetable_solve))

; -------- Assignment API --------

(: csp-assignment-opt (-> Expression Expression))
(= (csp-assignment-opt $cost-matrix)
   (csp_assignment_opt $cost-matrix))

(: csp-assignment-topk (-> Number Expression Expression))
(= (csp-assignment-topk $k $cost-matrix)
   (csp_assignment_topk $k $cost-matrix))

; -------- Timetable API --------

(: csp-timetable-solve (-> Expression Expression Expression Expression Expression))
(= (csp-timetable-solve $classes $rooms $slots $blocked)
   (csp_timetable_solve $classes $rooms $slots $blocked))
