-- Aufgabe1 a
listOfSums::[Int]->[Int]
listOfSums a = step a a [] where 
	step::[Int]->[Int]->[Int]->[Int]
	step list (x:xs) acc = step list xs (acc ++ [i + x| i<-list])
	step list [] acc = acc
-- Aufgabe1 b
goldbachPairs::Int->[(Int,Int)]
goldbachPairs n = goldbachInner n pr pr [] where
	pr = primesTo n;
	h = (quot (length pr) 2) + 1;
	goldbachInner::Int->[Int]->[Int]->[(Int,Int)]->[(Int,Int)]
	goldbachInner n [] b acc = acc
	goldbachInner n a [] acc = acc
	goldbachInner n a (b:tb) acc | length(a) <= h = acc
		| (last(a) +  b) == n = goldbachInner n (init a) tb (acc ++ [(last(a), b)])
		| (last(a) +  b) < n = goldbachInner n a tb acc
		| otherwise = goldbachInner n (init a) ([b]++tb) acc
primesTo::Int->[Int]
primesTo n = filter isPrime [2 .. n] where 
	isPrime n = go 2 where
		go d
		  | d*d > n        = True
		  | n `rem` d == 0 = False
		  | otherwise      = go (d+1)
-- Aufgabe 2 a
binom_naiv::Int->Int->Int
binom_naiv n k = product [1..n] `div` ( product [1..k] * product [1..(n-k)] )
-- Aufgabe 2 b
binom::Int->Int->Int
binom n 0 = 1
binom 0 k = 0
binom n k = binom (n-1) (k-1) * n `div` k
-- Aufgabe 2 c
binom_behndrt::Integer->Integer->Integer
binom_behndrt n k = truncate( inner (fromInteger  n) (fromInteger  k) 0.0 ) where
	inner::Float->Float->Float->Float
	inner n k acc | acc == k = 1
		| otherwise = ((n - k + 1 + acc) / (k-acc) ) * inner n k (acc+1)
-- Aufgabe 3 a
positions::Char->[Char]->[Int]
positions c str = inner c str 0 [] where
	inner::Char->[Char]->Int->[Int]->[Int]
	inner c [] i acc = acc
	inner c (f:str) i acc | f == c = inner c str (i+1) (acc ++ [i])
		|otherwise = inner c str (i+1) acc
-- Aufgabe 3 b (higher order my arse)
positions_list::Char->[Char]->[Int]
positions_list c str = [i |i <-[0..((length str)-1)], (str!!i) == c]
-- Aufgabe 4
paintPicture :: ((Int, Int, Int) -> Char) -> Int -> [Char]
paintPicture f size = paint size (map f [(x,y,size) | x <- [1..size], y <- [1..size]])
                      where
                        paint 0  []     = []
                        paint 0 (c:cs)  = '\n' : (paint size (c:cs))
                        paint n (c:cs)  = c: (paint (n-1) cs)

diag (x,y,size) = if (x==y) then 'a' else ' '

test4 = putStrLn (paintPicture diag 30)