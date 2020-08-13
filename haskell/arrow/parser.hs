{-# LANGUAGE Arrows #-}

import Control.Category
import Control.Arrow
import Data.List(union)
import Data.Char(isDigit)

data StaticParser s = SP Bool [s]
data DynamicParser s a b = DP ((a,[s]) -> Maybe (b,[s]))
data Parser s a b = Parser {
    getStaticParser :: StaticParser s,
    getDynamicParser :: DynamicParser s a b
}

instance Eq s => Category (Parser s) where
    id = Parser (SP True []) (DP $ \(a,s) -> Just (a,s))
    (Parser (SP b2 s2) (DP g)) . (Parser (SP b1 s1) (DP f)) = Parser
        (SP (b1 && b2) (if b1 then s1 `union` s2 else s1))
        (DP $ \x -> case f x of
            Nothing -> Nothing
            Just y -> g y)

instance Eq s => Arrow (Parser s) where
    arr f = Parser (SP True []) (DP $ \(x,s) -> Just (f x,s))
    first (Parser static (DP f)) = Parser static (DP $ \((x,y),s) -> case f (x,s) of
        Nothing -> Nothing
        Just (x',s') -> Just ((x',y),s'))


instance Eq s => ArrowZero (Parser s) where
    zeroArrow = Parser (SP False []) (DP $ \_ -> Nothing)

instance Eq s => ArrowPlus (Parser s) where
    (Parser (SP b1 s1) (DP f)) <+> (Parser (SP b2 s2) (DP g)) = Parser
        (SP (b1 || b2) (s1 `union` s2))
        (DP $ \(x,s) -> case s of
            [] -> if b1 then f (x,s) else
                  if b2 then g (x,s) else
                  Nothing
            (h:t) -> if h `elem` s1 then f (x,s) else
                     if h `elem` s2 then g (x,s) else
                     if b1 then f (x,s) else
                     if b2 then g (x,s) else
                     Nothing)

instance Eq s => ArrowChoice (Parser s) where
    left (Parser static (DP f)) = Parser static (DP $ \(x,s) -> case x of
        Left a -> case f (a,s) of
            Nothing -> Nothing
            Just (a',s') -> Just (Left a',s')
        Right b -> Just (Right b,s))

runParser :: (Eq s) => Parser s a b -> [s] -> a -> Maybe (b,[s])
runParser (Parser (SP b s) (DP f)) xs a = case xs of
    [] -> if b then f (a,xs) else Nothing
    (h:t) -> if h `elem` s || b then f (a,xs) else Nothing

liftA2 :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = (f &&& g) >>^ \(b,c) -> b `op` c

testA :: Arrow a => a b Bool -> a b (Either b b)
testA f = (f &&& arr Control.Category.id) >>^ \(b,x) -> if b then Left x else Right x

check :: ([s] -> a -> Bool) -> Parser s a Bool
check p = Parser (SP True []) (DP $ \(a,s) -> Just (p s a,s))

symbol :: s -> Parser s a s
symbol x = Parser (SP False [x]) (DP $ \(a,(_:xs)) -> Just (x,xs))

digit :: Parser Char a Char
digit = Parser (SP False "0123456789") (DP $ \(_,x:xs) -> Just (x,xs))