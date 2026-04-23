(= (f $a $b) (+ $a $b))

!(test (repr (f 1)) "(partial f (1))")
