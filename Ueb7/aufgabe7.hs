-- Aufgabe1
data Queue a = EmptyQ | Q { listHead :: a, listTail :: Queue a} 
makeQueue::Queue a
makeQueue = EmptyQ
enQ::a -> Queue a -> Queue a
enQ value q = (Q value q)
deQ:: Queue a -> Queue a
deQ (Q head tail) = tail
isEmpty:: Queue a->Bool
isEmpty EmptyQ = True
isEmpty q = False

instance Show (Queue a) where
	show (EmptyQ) = ""
	show (Q value q) = show value ++ "|" ++ show q

instance Eq (Queue a) where
	EmptyQ == EmptyQ = True
	(Q h1 t1) == (Q h2 t2) = (h1 == h2) && (t1 == t2) 
	_ == _ = False

instance Ord (Queue a) where
	(Q h1 t1) `compare` (Q h2 t2) = compare t1 t2