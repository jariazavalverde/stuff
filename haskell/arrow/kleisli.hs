{-# LANGUAGE Arrows #-}

import qualified Control.Category as Cat
import Control.Arrow

data K m a b = K {
    runK :: a -> m b
}

instance Monad m => Cat.Category (K m) where
    id = K (\x -> return x)
    (K g) . ~(K f) = K (\x -> f x >>= g)

instance Monad m => Arrow (K m) where
    arr = K . ((.) return)
    first (K f) = K (\(x,y) -> f x >>= \x' -> return (x',y))

instance Monad m => ArrowChoice (K m) where
    left (K f) = K (\e -> case e of
        Left x -> fmap Left (f x) 
        Right x -> return (Right x)) 

instance Monad m => ArrowApply (K m) where
    app = K (\(K f,x) -> f x)

-- | Combinators.

liftA2 :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = (f &&& g) >>^ \(b,c) -> b `op` c

test :: Arrow a => a b Bool -> a b (Either b b)
test f = (f &&& arr id) >>> arr (\(b,x) -> if b then Left x else Right x)

-- | Sample.

powerM :: [a] -> [[a]]
powerM [] = [[]]
powerM (x:xs) = do p <- powerM xs
                   [p,(x:p)]

powerM' :: [a] -> [[a]]
powerM' xs = if null xs then [[]] else powerM' (tail xs) >>= \p -> [p,(head xs:p)]

powerA :: K [] [a] [a]
powerA = test (arr (\xs -> null xs)) >>>
    (returnA ||| ((arr head &&& (arr tail ^>> powerA)) >>> (K $ \(h,p) -> [p,h:p])))