{-# LANGUAGE Arrows #-}

import qualified Control.Category as Cat
import Control.Arrow
import Prelude hiding((<*>))

data StaticParser s = SP Bool [s]
data DynamicParser s a b = DP ((a,[s]) -> Maybe (b,[s]))
data Parser s a b = Parser {
    getStaticParser :: StaticParser s,
    getDynamicParser :: DynamicParser s a b
}

instance Cat.Category (Parser s) where
    id = Parser (SP True []) (DP $ \(a,s) -> Just (a,s))
    (Parser (SP b2 s2) (DP g)) . (Parser (SP b1 s1) (DP f)) = Parser
        (SP (b1 && b2) (s1 ++ if b1 then s2 else []))
        (DP $ \x -> case f x of
            Nothing -> Nothing
            Just y -> g y)

instance Arrow (Parser s) where
    arr f = Parser (SP True []) (DP $ \(x,s) -> Just (f x,s))
    first (Parser static (DP f)) = Parser static (DP $ \((x,y),s) -> case f (x,s) of
        Nothing -> Nothing
        Just (x',s') -> Just ((x',y),s'))

instance ArrowZero (Parser s) where
    zeroArrow = Parser (SP False []) (DP $ \_ -> Nothing)

instance Eq s => ArrowPlus (Parser s) where
    (Parser (SP b1 s1) (DP f)) <+> (Parser (SP b2 s2) (DP g)) = Parser
        (SP (b1 || b2) (s1 ++ s2))
        (DP $ \(x,s) -> case s of
            [] -> if b1 then f (x,s) else
                  if b2 then g (x,s) else
                  Nothing
            (h:t) -> if h `elem` s1 then f (x,s) else
                     if h `elem` s2 then g (x,s) else
                     Nothing)

runParser :: (Eq s) => Parser s a b -> [s] -> a -> Maybe (b,[s])
runParser (Parser (SP b s) (DP f)) xs a = case xs of
    [] -> if b then f (a,xs) else Nothing
    (h:t) -> if h `elem` s then f (a,xs) else Nothing

symbol :: a -> Parser a b a
symbol x = Parser (SP False [x]) (DP $ \(a,(_:xs)) -> Just (x,xs))

digit :: Parser Char a Int
digit = Parser (SP False "0123456789") (DP $ \(_,x:xs) -> Just (read (x:[]),xs))

add :: Parser Char a Int
add = (symbol '(' >>> digit) &&& (symbol '+' >>> digit &&& symbol ')' >>^ fst) >>^ uncurry (+)