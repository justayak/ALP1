import Data.List
-- Aufgabe 1
ispyth ::(Int,Int,Int) -> Bool
ispyth (x,y,z) = s!!0*s!!0+s!!1*s!!1 == s!!2*s!!2 where s = sort[x,y,z]

-- Aufgabe 2
isleap :: Int -> Bool
isleap j | 0==j`mod`100 = 0 == j`mod`400
	   | otherwise    = 0 == j`mod`4
	   
-- Aufgabe 3
ismulti::(Int,Int,Int) -> Bool
ismulti (a,b,c) = s!!2`mod`s!!1==0 && s!!2`mod`s!!0==0 where s = sort[a,b,c]
	
-- Aufgabe 4
toAscii :: Char -> Int
toAscii s = fromEnum s
isCapital s = n > 64 && n < 91 where n = toAscii s
toLower s = if isCapital s then toEnum lowerint::Char else s 
	where lowerint = toAscii(s) + 32
	
-- Aufgabe 5
isLetter :: Char -> Bool
isLetter s = isCapital s || i > 96 && i < 123 where i = toAscii s

-- Aufgabe 6
weekday :: Int -> Int -> Int -> String
weekday d m y | m == 2 && isleap(y) && validDay(29) = getName name
	| m == 2 && not(isleap(y)) && validDay(28) =  getName name
	| m `mod` 2 == 0 && not(m == 2) && validMonthRange && validDay(31) = getName name
	| m `mod` 2 == 1 && validMonthRange && validDay(30) = getName name
	| otherwise = "wrong "
	where validDay max = d > 0 && d <= max;
		validMonthRange = m > 0 && m < 13;
		y0 = y - ( ( 14-m )`div` 12);
		x = y0 + (y0 `div`4) - (y0`div`100)+(y0`div`400);
		m0 = m + 12 *( (14 - m)`div`12)-2;
		name = (d + x + (31*m0)`div`12)`mod`7;
		getName v = case v of 
		0 -> "So"
		1 -> "Mo"
		2 -> "Di"
		3 -> "Mi"
		4 -> "Do"
		5 -> "Fr"
		6 -> "Sa"
		_ -> "wtf";
		
-- Aufgabe 7
areaPoly :: Float -> Float -> Float
areaPoly s n = (n * s * a)/2
	where a = s/(2 * tan(pi/n));
	