{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

data Nil = Nil deriving Show
data Cons h t = Cons h t deriving Show

data Z = Z deriving Show
data S a = S a deriving Show

class Append x y z | x y -> z where
    append :: x -> y -> z
-- append([], X, X).
instance Append Nil x x where
    append Nil x = x
-- append([H|T], X, [H|S]) :- append(T, X, S).
instance Append t x s => Append (Cons h t) x (Cons h s) where
    append (Cons h t) x = Cons h (append t x)

class Powerset x y where
    powerset :: x -> y
-- powerset([], []).
instance Powerset Nil Nil where
    powerset Nil = Nil
-- powerset([H|T], P) :- powerset(T, P).
instance {-# OVERLAPPABLE #-} Powerset t p => Powerset (Cons h t) p where
    powerset (Cons _ t) = powerset t
-- powerset([H|T], [H|P]) :- powerset(T, P).
instance {-# OVERLAPPABLE #-} Powerset t p => Powerset (Cons h t) (Cons h p) where
    powerset (Cons h t) = Cons h (powerset t)
