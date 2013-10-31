import Data.List
import Text.Regex
import Numeric

--Aufgabe 1
type Point=(Double, Double)
type Rectangle=(Point,Point)
area::Rectangle -> Double
area (a,b)= abs(fst(a)-fst(b)) * abs(snd(a)-snd(b))

overlaps::Rectangle->Rectangle->Bool
overlaps ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) = x4 > y4 
	where l1 = sort[x1,x2]!!0;
		l2 = sort[x3,x4]!!0;
		r1 = sort[x1,x2]!!1;
		r2 = sort[x3,x4]!!1;

--contains::Rectangle->Rectangle->Bool

--Aufgabe 2.a
oddsum::Int->Int
oddsum 1 = 1
oddsum n | n`mod`2==0 = oddsum(n-1)
	|otherwise = n + oddsum(n-1)

--Aufgabe 2.b
oddsumL::Int->Int
oddsumL n = sum([x | x <- [1..n], x`mod`2 /= 0])

--Aufgabe 3
true::Int
true=1
false::Int
false=0
oder::Int->Int->Int
oder 0 0 = 0
oder _ _ = 1
exoder::Int->Int->Int
exoder 0 1 = 1
exoder 1 0 = 1
exoder _ _ = 0
und::Int->Int->Int
und 1 1 = 1
und _ _ = 0
negation::Int->Int
negation 1 = 0
negation 0 = 1

--Aufgabe 4
binsum::Int->Int
binsum 0 = 0
binsum n | n`mod`2==0 = binsum(n-1)
	|otherwise = 1 + binsum(n-1)

--Aufgabe 5
hex2okt::String->String
hex2okt n = bin2okt(replacePattern n [("0","0000"),("1","0001"),("2","0010"),
	("3","0011"),("4","0100"),("5","0101"),("6","0110"),("7","0111"),("8","1000"),
	("9","1001"),("A","1010"),("B","1011"),("C","1100"),("D","1101"), ("E","1110"), 
	("F","1111")])

bin2okt::String->String
bin2okt	[] = ""
bin2okt (a:b:c:ls) = fuz(a:b:c:[]) ++ bin2okt(ls)
bin2okt (b:c:ls) = fuz('0':(b:c:[])) ++ bin2okt(ls)
bin2okt (c:ls) = fuz("00"++(c:[])) ++ bin2okt(ls)

fuz n = replacePattern n [("000", "0"), ("001", "1"),("010", "2"),("011", "3"),
	("100", "4"),("101", "5"),("110", "6"),("111", "7")];

-- ersetzt die pattern in dem Zielstring:
-- replacePattern "hallo" [("a", "z"),("ll","_")] -> hz_o	
replacePattern:: String->[(String,String)]->String
replacePattern a [] = a
replacePattern a (b:bs) = replacePattern w bs where 
	w = replace a g h;
	g = fst(b);
	h = snd(b);

replace::String->String->String->String
replace a b c = subRegex (mkRegex b) a c

--Aufgabe 6
-- KP ob ich die Aufgabe richtig verstanden hab: was mein Prog 
-- machen soll ist: aus der ausgerechneten Quersumme wieder die
-- Quersumme ausrechnen, wenn die Quersumme > 9 ist
digitSum::Int->Int
digitSum n | n < 0 = error "nope.."
	| n < 10 = n
	| otherwise = digitSum (qu n)
qu::Int->Int	
qu = f 0 where 
	f a 0 = a
	f a n = f (a+r) q where 
		(q,r) = n `divMod`10

--Aufgabe 7
addList::[Int]->[Int]->[Int]
addList a b | length(a) /= length(b) = error "fucked up"
	| otherwise = addListRec a b
addListRec [] [] = []
addListRec (a:as) (b:bs) = (a+b) : addList as bs