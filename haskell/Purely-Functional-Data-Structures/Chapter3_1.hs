module Chapter3_1 where

-- LEFTIST HEAPS

type Rank = Int

data Heap a = Empty | Heap Rank a (Heap a) (Heap a) deriving (Show, Eq)

rank :: Heap a -> Rank
rank Empty = 0
rank (Heap r _ _ _) = r

merge :: Ord a => Heap a -> Heap a -> Heap a
merge x Empty = x
merge Empty y = y
merge h1@(Heap r1 x a1 b1) h2@(Heap r2 y a2 b2) =
    if x < y then make x a1 (merge b1 h2)
             else make y a2 (merge h1 b2)

make :: a -> Heap a -> Heap a -> Heap a
make x a b = if rank a < rank b then Heap (rank a + 1) x b a
                                else Heap (rank b + 1) x a b

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (Heap 1 x Empty Empty)

findMin :: Heap a -> a
findMin (Heap _ x _ _) = x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Heap _ _ a b) = merge a b

-- Exercise 3.2
insert' :: Ord a => a -> Heap a -> Heap a
insert' x Empty = Heap 1 x Empty Empty
insert' x (Heap r y a b) = if x < y then make x a (insert' y b)
                                    else make y a (insert' x b)

-- Exercise 3.3
fromList :: Ord a => [a] -> Heap a
fromList = head . (until ((<= 1).length) mergeList) . map (\x -> Heap 1 x Empty Empty) 

mergeList :: Ord a => [Heap a] -> [Heap a]
mergeList [] = []
mergeList [x] = [x]
mergeList (a:b:cs) = merge a b : mergeList cs