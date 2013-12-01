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
	countLetters::String -> Char -> Int -- dumm aber egal
	countLetters str c = length $ filter (==c) str
int2Nat::Int->Nat
int2Nat i | i == 0 = Zero
	| otherwise = add one (int2Nat(i-1))
-- Aufgabe 1c/d
zint2Int::ZInt->Int
zint2Int (Z Zero a) = nat2Int a
zint2Int (Z a Zero) = (nat2Int a) * (-1)
int2Zint::Int->ZInt
int2Zint i | i >= 0 = (Z Zero (int2Nat(i)))
	| otherwise = (Z (int2Nat(-i)) Zero)
zpow::ZInt->Nat->ZInt
zpow a po = int2Zint( (zint2Int(a)) ^ (nat2Int(po)) )
zabs::ZInt->ZInt
zabs (Z Zero a) = (Z Zero a)
zabs (Z a Zero) = (Z a Zero) `zmult` mzone
zggt::ZInt->ZInt->ZInt
zggt a b = int2Zint(gcd (zint2Int(a)) (zint2Int(b)))
-- Aufgabe 2
data SBTree = L|N SBTree SBTree deriving Show
testTree::SBTree
testTree = N (N (N L L) (N L L)) (N (N L L) L)
startTree::SBTree
startTree = (N L L)
depth::SBTree->Integer
depth L = 0
depth (N lt rt) = (max (depth lt) (depth rt)) + 1
balanced::SBTree->Bool
balanced L = True
balanced (N lt rt) = (balanced lt) && (balanced rt) && depth lt == depth rt
insertLeaf::SBTree->SBTree
insertLeaf L = N L L
insertLeaf (N L L) = N (N L L) L
insertLeaf (N lt L) = N lt (N L L)
insertLeaf (N lt rt) | (((depth lt)>(depth rt))&&not(balanced lt))||
	(((depth lt)==(depth rt))&&(balanced rt)) = 
	(N (insertLeaf lt) rt)
	| otherwise = (N lt (insertLeaf rt))
insertLeafs::SBTree->Integer->SBTree
insertLeafs a 0 = a
insertLeafs a count = insertLeafs (insertLeaf(a)) (count - 1)
deleteLeaf::SBTree->SBTree
deleteLeaf (N L L) = L
deleteLeaf (N lt L) = (N L L)
deleteLeaf (N lt rt) | (((depth lt)>(depth rt))) = (N (deleteLeaf lt) rt)
	|otherwise = (N lt (deleteLeaf rt))
deleteLeafs::SBTree->Integer->SBTree
deleteLeafs a 0 = a
deleteLeafs a count = deleteLeafs (deleteLeaf(a)) (count - 1)

full::SBTree->Bool
full (N L L) = True
full (N lt L) = False
full (N L rt) = False -- sollte nie auftreten..
full (N lt rt) = full(lt) && full(rt)