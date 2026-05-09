

(= (space) my_space_name)

!(add-atom (space) (my test atom))
!(test (match (space) $a $a) (my test atom))
