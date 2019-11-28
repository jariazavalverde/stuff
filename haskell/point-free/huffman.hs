import Data.List(sortOn)
import Data.Function(on)

data Tree a = EmptyTree | Tree {
    root :: a,
    left :: Tree a,
    right :: Tree a
} deriving (Read, Show, Eq, Ord)

huffman :: (Show a, Read a, Ord a, Num a) => [Tree a] -> Tree a
huffman = head . until ((==1).length)
    (sortOn root . ((:) <$> (Tree <$> uncurry ((+) `on` root) <*> fst <*> snd) . 
    read . ('(':) . (++")") . init . tail . show . take 2 <*> drop 2))