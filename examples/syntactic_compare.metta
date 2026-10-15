!(test (@< a b) true)
!(test (@> a b) false)
!(test (@<= 1 1) true)
!(test (@>= (f a) (f b)) false)
