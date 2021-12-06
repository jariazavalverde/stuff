{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
import Prelude hiding(const, id, pred)

data Nat = Z | S Nat deriving (Show, Read, Eq, Ord)

toNat :: Int -> Nat
toNat 0 = Z
toNat n = S (toNat $ n-1)

fromNat :: Nat -> Int 
fromNat Z = 0
fromNat (S n) = 1 + fromNat n

naturals :: [Nat]
naturals = Z : map S naturals

-- | Zero function
z :: a -> Nat
z _ = Z

-- | Successor function
s :: Nat -> Nat
s = S

-- | Identity functions
class Id a where
    id :: Int -> a -> Nat

instance Id Nat where
    id 1 a = a

instance Id (Nat,Nat) where
    id 1 (a,b) = a
    id 2 (a,b) = b

instance Id (Nat,Nat,Nat) where
    id 1 (a,b,c) = a
    id 2 (a,b,c) = b
    id 3 (a,b,c) = c

-- | Composition functions
class Cn a where
    cn :: (a -> Nat) -> [b -> Nat] -> (b -> Nat)

instance Cn Nat where
    cn f [g] = f . g

instance Cn (Nat,Nat) where
    cn f [g1,g2] x = f (g1 x, g2 x)

instance Cn (Nat,Nat,Nat) where
    cn f [g1,g2,g3] x = f (g1 x, g2 x, g3 x)

-- | Constant function
const :: Int -> a -> Nat
const 0 = z
const n = cn s [const (n-1)]

-- | Primitive recursion
class Pr a b c | a -> b c where
    pr :: (b -> Nat) -> (c -> Nat) -> (a -> Nat)

instance Pr Nat () (Nat,Nat) where
    pr f g Z = f ()
    pr f g (S y) = g (y, pr f g y)

instance Pr (Nat,Nat) Nat (Nat,Nat,Nat) where
    pr f g (x, Z) = f x
    pr f g (x, S y) = g (x, y, pr f g (x,y))

instance Pr (Nat,Nat,Nat) (Nat,Nat) (Nat,Nat,Nat,Nat) where
    pr f g (x, y, Z) = f (x,y)
    pr f g (x, y, S z) = g (x, y, z, pr f g (x,y,z))

-- | Addition
add :: (Nat,Nat) -> Nat
add = pr (id 1) (cn s [id 3])

-- | Multiplication
mul :: (Nat,Nat) -> Nat
mul = pr z (cn add [id 1, id 3])

-- | Exponentiation
pow :: (Nat,Nat) -> Nat
pow = pr (const 1) (cn mul [id 1, id 3])

-- | Subtraction
sub :: (Nat,Nat) -> Nat
sub = pr (id 1) (cn pred [id 3])

-- | Factorial
fact :: Nat -> Nat
fact = pr (const 1) (cn mul [id 2, cn s [id 1]])

-- | Predecessor function
pred :: Nat -> Nat
pred = pr z (id 1)

-- | Sign function
sg, sg' :: Nat -> Nat
sg = pr z (const 1)
sg' = pr (const 1) z

-- | Minimization
class Mn a b | a -> b where
    mn :: (b -> Nat) -> (a -> Nat)

instance Mn Nat (Nat,Nat) where
    mn f x = head $ filter (\y -> f (x,y) == Z) naturals

instance Mn (Nat,Nat) (Nat,Nat,Nat) where
    mn f (x,y) = head $ filter (\z -> f (x,y,z) == Z) naturals
