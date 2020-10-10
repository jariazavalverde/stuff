module Chapter3_2 where

type Rank = Int

-- | Binomial trees
-- A binomial tree of rank r is a node with r children t_1...t_r, where each t_i 
-- is a binomial tree of rank r-1.
data Tree a = Tree {
    rank :: Rank,
    root :: a,
    children :: [Tree a]
} deriving (Show, Eq)

-- | linking binomial trees
-- Each list of children is maintained in decreasing order of rank, and elements
-- are stored in heap order. We maintain heap order by always linking trees with
-- larger roots under trees with smaller roots.
link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Tree r x xs) t2@(Tree _ y ys) = if x < y then Tree (r+1) x (t2:xs)
                                                  else Tree (r+1) y (t1:ys)

-- | Binomial heaps
-- A binomial heap is a collection of heap-ordered binomial trees in which no
-- two trees have the same rank.
type Heap a = [Tree a]

-- | inserting elements into binomial heaps
-- We first create a new singleton tree. We then step through the existing trees
-- in increasing order of rank until we find a missing rank, linking trees of
-- equal rank.
insert :: Ord a => a -> Heap a -> Heap a
insert x = insertTree (Tree 0 x [])

insertTree :: Ord a => Tree a -> Heap a -> Heap a
insertTree x [] = [x]
insertTree x (y:ys) = if rank x < rank y then (x:y:ys)
                                         else insertTree (link x y) ys

-- | merging binomial heaps
-- We step through both lists of trees in increasing order of rank, linking 
-- trees of equal rank.
merge :: Ord a => Heap a -> Heap a -> Heap a
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | rank x < rank y = x : merge xs (y:ys)
                    | rank y < rank x = y : merge (x:xs) ys
                    | otherwise = insertTree (link x y) (merge xs ys)

-- | selecting minimum tree
-- Finds the tree with the minimum root and removes it from the list, returning
-- both the tree and the remaining list.
selectMinTree :: Ord a => Heap a -> (Tree a, Heap a)
selectMinTree [x] = (x, [])
selectMinTree (x:xs) = let (x', xs') = selectMinTree xs
                       in if root x < root x' then (x, xs)
                                              else (x', x:xs')

findMin :: Ord a => Heap a -> a
findMin = root . fst . selectMinTree

deleteMin :: Ord a => Heap a -> Heap a
deleteMin [] = []
deleteMin xs = let (x, xs') = selectMinTree xs in merge (reverse $ children x) xs'

-- | Exercise 3.5
-- Define findMin directly rather than via a call to removeMinTree.
findMin' :: Ord a => Heap a -> a
findMin' = minimum . map root