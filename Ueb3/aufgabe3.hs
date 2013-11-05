-- Aufgabe 1
twoComplement::[Int]->[Int]
twoComplement a = addone (map (1-)  a)
	where
	addone::[Int]->[Int]
	addone [] = []
	addone x | last x == 0 = take ((length x) - 1) x ++ [1] 
		| last x == 1 = (addone (take ((length x) - 1) x)) ++ [0]

-- Aufgabe 3
balanced:: [Char ] -> Bool
balanced text = bal [] text 
	where
	bal:: [Char] -> [Char] -> Bool
	bal [] [] = True
	bal stapel ('(':xs) = bal (')':stapel) xs
	bal stapel ('[':xs) = bal (']':stapel) xs
	bal stapel ('{':xs) = bal ('}':stapel) xs
	bal (s:stapel) (x:xs) | s==x = bal stapel xs
	bal _ _ = False
	
-- Aufgabe 4
updateList::[Int]->Int->Int->[Int]
updateList (x:xs) 0 elem = elem:xs
updateList (x:xs) i elem = x:updateList xs (i-1) elem

-- Aufgabe 5
random :: Int -> Int 
random seed = (25173*seed + 13849) `mod` 65536 
randList :: Int -> [Int]
randList n = randList' n [random n]
	where
	randList' n xs 
		| (length xs) == n = xs 
		| otherwise = randList' n (a:xs) 
						where 
						a = random (head xs)

