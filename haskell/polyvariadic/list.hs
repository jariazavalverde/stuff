{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

class PolyList a b | a -> b where
    listAll :: b -> a

instance PolyList [a] [a] where
    listAll acc = acc

instance (PolyList b [a]) => PolyList (a -> b) [a] where
    listAll acc = \x -> listAll (acc ++ [x])

polyList :: PolyList b [a] => b
polyList = listAll []