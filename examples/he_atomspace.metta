!(import! &self lib/lib_he)

!(add-atom &self (= (addnormal) (+ 1 3)))
!(add-reduct &self (= (addreduct) (+ 1 3)))

!(test (match &self (= (addnormal) $X) $X) (quote (+ 1 3)))
!(test (match &self (= (addreduct) $X) (quote $X)) (4))

!(get-type 1)

(: a A)
!(test (get-type-space &self a) A)

(hello world)
!(test (unify &self (hello world) Yes No) Yes)
!(test (unify &self (hello dream) Yes No) No)
