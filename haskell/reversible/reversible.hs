{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.List(elemIndex)
import Data.Maybe(fromJust)

instance (Eq a, Bounded a, Enum a) => Enum [a] where
    toEnum x = take 0 bounds ++ ((gen [[]]) !! x)
        where bounds = [minBound..maxBound]
              gen xs = let ys = (:) <$> bounds <*> xs in xs ++ gen ys
    fromEnum [] = 0
    fromEnum xs = sum $ zipWith (\x y -> l^x*y) [0..] (reverse $ map ((1+).fromJust.(`elemIndex` bounds)) xs)
        where bounds = [minBound..maxBound]
              l = length bounds

instance (Bounded a) => Bounded [a] where
    minBound = []
    maxBound = repeat maxBound

values :: (Bounded a, Enum a) => Maybe a -> [a]
values (Just x) = [x]
values Nothing = [minBound..]

reversible :: (Bounded a, Bounded b, Enum a, Enum b, Eq b) => (a -> b) -> (Maybe a, Maybe b) -> [(a, b)]
reversible f (a, Nothing) = map ((,) <$> id <*> f) (values a)
reversible f (a, Just b) = map ((flip (,)) b) $ filter ((== b) . f) (values a)

reversible2 :: (Bounded a, Bounded b, Bounded c, Enum a, Enum b, Enum c, Eq c) => (a -> b -> c) -> (Maybe a, Maybe b, Maybe c) -> [((a, b), c)]
reversible2 f (a, b, Nothing) = map ((,) <$> id <*> uncurry f) ((,) <$> values a <*> values b)
reversible2 f (a, b, Just c) = map ((flip (,)) c) $ filter ((== c) . (uncurry f)) ((,) <$> values a <*> values b)