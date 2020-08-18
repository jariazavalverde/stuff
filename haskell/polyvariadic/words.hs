{-# LANGUAGE FlexibleInstances #-}

class PolyWords a where
    wordsAll :: String -> a

instance PolyWords String where
    wordsAll acc = acc

instance (PolyWords a) => PolyWords (String -> a) where
    wordsAll acc = \x -> wordsAll (if null acc then x else acc ++ ' ':x)

polyWords :: PolyWords a => a
polyWords = wordsAll ""