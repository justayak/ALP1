import Data.Time.Clock.POSIX
import System.IO.Unsafe
import Data.Char

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
encode str c = [toEnum ( fromEnum(i) + c) |i<-str]
decode::[Char]->Int->[Char]
decode str c = [toEnum ( fromEnum(i) - c) | i <- str]
	
-- Aufgabe 8
calculateWhile::(a->a)->(a->Bool)->[a]->[a]
calculateWhile f p list = yuss f p [] (list!!0) (tail list) False
	where 
		yuss::(a->a)->(a->Bool)->[a]->a->[a]->Bool->[a]
		yuss f p front elem [] running = front ++ [elem]
		yuss f p front elem back running = if p elem then
			yuss f p (front ++ [ f elem ]) (back!!0) (tail back) True
			else if running then
				front ++ [elem] ++ back
			else 
				yuss f p (front ++ [elem]) (back!!0) (tail back) False

-- Aufgabe 9
text2words::[Char]->[[Char]]
text2words text = text2wordsRec text [] []
	where
	text2wordsRec::[Char]->[Char]->[[Char]]->[[Char]]
	text2wordsRec [] word acc = acc ++ [word]
	text2wordsRec (c:text) word acc = if c == ' ' || c == '.' || c == ',' || c == '?' 
		then text2wordsRec text [] (if length word > 0 then (acc ++ [word]) else acc)
		else text2wordsRec text (word ++ [c]) acc