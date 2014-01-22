-- test
powset :: [a]->[[a]]
powset [] = [[]]
powset(x:xs) = powset' ++ [x:ys | ys <- powset']
--powset(x:xs) =  [x:ys | ys <- powset']
	where 
		powset' = powset xs
