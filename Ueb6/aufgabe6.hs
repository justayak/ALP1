import QRationals
-- Aufgabe 1a/b
ngerade::Nat->Bool
ngerade n = (nat2Int n) `mod`2 == 0
nmax::Nat->Nat->Nat
nmax a b | ai > bi = a
	|otherwise = b 
	where ai = nat2Int a;
		bi = nat2Int b;
nat2Int::Nat->Int
nat2Int n = countLetters (show n) 'S' where
	countLetters::String -> Char -> Int
	countLetters str c = length $ filter (==c) str
int2Nat::Integer->Nat
int2Nat i | i == 0 = Zero
	| otherwise = add one (int2Nat(i-1))
-- Aufgabe 1c/d
zint2Int::ZInt->Int
zint2Int (Z Zero a) = nat2Int a
zint2Int (Z a Zero) = (nat2Int a) * (-1)

int2Zint::Integer->ZInt
int2Zint i | i >= 0 = (Z Zero (int2Nat(i)))
	| otherwise = (Z (int2Nat(-i)) Zero)