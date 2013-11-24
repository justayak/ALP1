-- Aufgabe 2a
flatten::[[a]]->[a]
flatten l = foldr (++) [] l
-- Aufgabe 2b
flattenL::[[a]]->[a]
flattenL l = foldl (++) [] l
-- Aufgabe 2c
-- foldr ist besser, da die Funktionswerte nicht vertauscht
-- werden müssen (foldr f ist besser als foldl (flip f))
-- Außerdem kommt foldr dank lazy ev. mit unendlichen Listen
-- besser klar blabla
-- Aufgabe 3
bin2dec::[Int]->Int
bin2dec l = foldl (\x y -> x*2+y) 0 l

temp::[Int]->Int
temp l = foldl (+) 2 l

-- Aufgabe 4
majority :: (Eq a) => [a] -> Maybe a
majority l | (length l) > 5 = Just (l !! 0)
	| otherwise = Nothing where 
		half = ( (length l) `div` 2) + 1;
		