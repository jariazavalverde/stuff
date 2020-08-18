{-# LANGUAGE GADTs #-}

class PolyAdd a where
    addAll :: Int -> a

instance PolyAdd Int where
    addAll acc = acc

instance (b ~ Int, PolyAdd a) => PolyAdd (b -> a) where
    addAll acc = \x -> addAll (acc + x)

polyAdd ::PolyAdd a => a
polyAdd = addAll 0
