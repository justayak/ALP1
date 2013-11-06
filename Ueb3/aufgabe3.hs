import Data.Time.Clock.POSIX
import System.IO.Unsafe

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
randUntilRepeat::[Int]
randUntilRepeat = helper [] (random millis)
 	where 
 	helper::[Int]->Int->[Int]
 	helper rx r | r `elem` rx = rx
		| otherwise = helper (rx ++ [r]) (random (millis - r))
	
millis::Int
millis = unsafePerformIO $ ( round `fmap` getPOSIXTime )

-- Aufgabe 6
onlyPara::[Char]->[Char]
onlyPara x = [i | i<-x, i=='('||i==')'||i=='['||i ==']'||i=='{'||i== '}']

-- Aufgabe 7
encode::[Char]->Int->[Char]
encode str c = [i|i<-str, ces i]
	where 
	

