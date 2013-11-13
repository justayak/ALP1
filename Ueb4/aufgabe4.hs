-- Aufgabe1 a
listOfSums::[Int]->[Int]
listOfSums a = step a a [] where 
	step::[Int]->[Int]->[Int]->[Int]
	step list (x:xs) acc = step list xs (acc ++ [i + x| i<-list])
	step list [] acc = acc
-- Aufgabe1 b
