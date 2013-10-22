import Data.List
-- Aufgabe 1
ispyth (x,y,z) = x*x+y*y == z*z

-- Aufgabe 2
isleap j | 0==j`mod`100 = 0 == j`mod`400
	   | otherwise    = 0 == j`mod`4
	   
-- Aufgabe 3
isInt x = x == fromInteger (round x)
ismulti (a,b,c) = isInt(s!!2/s!!1) && isInt(s!!2/s!!0) where s = sort[a,b,c]
	
-- Aufgabe 4
toAscii :: Char -> Int
toAscii s = fromEnum s
isCapital s = n > 64 && n < 91 where n = toAscii s
toLower s = if isCapital s then toEnum lowerint::Char else s 
	where lowerint = toAscii(s) + 32
	
-- Aufgabe 5
isLetter s = isCapital s || i > 96 && i < 123 where i = toAscii s

-- Aufgabe 6
toGreg (d m y) = 