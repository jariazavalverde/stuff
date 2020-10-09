module Chapter2 where

data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)

member :: Ord a => a -> Tree a -> Bool
member _ Empty = False
member x (Tree left root right)
    | x < root = member x left 
    | x > root = member x right
    | otherwise = True

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Tree Empty x Empty
insert x tree@(Tree left root right)
    | x < root = Tree (insert x left) root right
    | x > root = Tree left root (insert x right)
    | otherwise = tree

-- Exercise 2.5 (a)
complete :: a -> Int -> Tree a
complete _ 0 = Empty
complete x n = let subtree = complete x (n-1) in Tree subtree x subtree

-- Exercise 2.5 (b)
create :: a -> Int -> Tree a
create x = fst . create' x

create' :: a -> Int -> (Tree a, Tree a)
create' x 0 = (Empty, Tree Empty x Empty)
create' x n = let (tree, tree') = create' x (div (n-1) 2)
              in if mod (n-1) 2 == 0
                 then (Tree tree x tree, Tree tree x tree')
                 else (Tree tree x tree', Tree tree' x tree')