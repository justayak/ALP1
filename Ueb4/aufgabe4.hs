-- Aufgabe1 a
listOfSums::[Int]->[Int]
listOfSums a = step a a [] where 
	step::[Int]->[Int]->[Int]->[Int]
	step list (x:xs) acc = step list xs (acc ++ [i + x| i<-list])
	step list [] acc = acc
-- Aufgabe1 b
primesTo::Int->[Int]
primesTo n = filter isPrime [2 .. n] where 
	isPrime n = go 2
	  where
		go d
		  | d*d > n        = True
		  | n `rem` d == 0 = False
		  | otherwise      = go (d+1)
