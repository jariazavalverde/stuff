{-# LANGUAGE Arrows #-}

import Control.Category
import Control.Arrow
import Data.List(union)

data StaticParser s = SP {
    getEmpty :: Bool,
    getStarters :: [s]
}

data DynamicParser s a b = DP {
    runDynamicParser :: ((a,[s]) -> Maybe (b,[s]))
}

data Parser s a b = Parser {
    getStaticParser :: StaticParser s,
    getDynamicParser :: DynamicParser s a b
}

instance Eq s => Category (Parser s) where
    id = Parser (SP True []) (DP $ \(a,s) -> Just (a,s))
    (Parser (SP b2 s2) (DP g)) . ~(Parser (SP b1 s1) (DP f)) = Parser
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

runParser :: Eq s => Parser s a b -> [s] -> a -> Maybe (b,[s])
runParser (Parser (SP b s) (DP f)) xs a = case xs of
    [] -> if b then f (a,xs) else Nothing
    (h:t) -> if h `elem` s || b then f (a,xs) else Nothing

-- | Combinators.

liftA2 :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = (f &&& g) >>^ \(b,c) -> b `op` c

(>$>) :: Arrow a => a b (c -> d) -> a b c -> a b d
(>$>) = liftA2 (\f x -> f x)

opt :: Eq s => Parser s b c -> c -> Parser s b c 
p `opt` x = p <+> empty x

some :: Eq s => Parser s a b -> Parser s a [b]
some p = (liftA2 (:) p (some p `opt` []))

-- | Basic parsers.

empty :: b -> Parser s a b
empty x = Parser (SP True []) (DP $ \(_,xs) -> Just (x,xs))

symbol :: s -> Parser s a s
symbol x = Parser (SP False [x]) (DP $ \(a,(_:xs)) -> Just (x,xs))

ignore :: s -> Parser s a a
ignore x = Parser (SP False [x]) (DP $ \(a,(_:xs)) -> Just (a,xs))

-- | Sample parser.

digit :: Parser Char a Char
digit = Parser (SP False "0123456789") (DP $ \(_,x:xs) -> Just (x,xs))

integer :: Parser Char a Int
integer = some digit >>^ read

expr, factor, term :: Parser Char a Int
expr = liftA2 (+) factor ((ignore '+' >>> expr) `opt` 0)
factor = liftA2 (*) term ((ignore '*' >>> factor) `opt` 1)
term = (ignore '(' >>> expr >>> ignore ')') <+> integer