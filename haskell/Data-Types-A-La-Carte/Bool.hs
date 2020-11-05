{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

data Expr f = In (f (Expr f))

data Val e = Val Bool
type BoolExpr = Expr Val

data Or e = Or e e
type OrExpr = Expr Or

data And e = And e e
type AndExpr = Expr And

infixr 1 :+:
data (f :+: g) e = Inl (f e) | Inr (g e)

instance Functor Val where
    fmap _ (Val x) = Val x

instance Functor Or where
    fmap f (Or x y) = Or (f x) (f y)

instance Functor And where
    fmap f (And x y) = And (f x) (f y)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl x) = Inl (fmap f x)
    fmap f (Inr x) = Inr (fmap f x)

class Functor f => Eval f where
    evalAlgebra :: f Bool -> Bool

instance Eval Val where
    evalAlgebra (Val x) = x

instance Eval And where
    evalAlgebra (And x y) = x && y

instance Eval Or where
    evalAlgebra (Or x y) = x || y

instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlgebra (Inl x) = evalAlgebra x
    evalAlgebra (Inr y) = evalAlgebra y

class (Functor sub, Functor sup) => sub :<: sup where
    inj :: sub a -> sup a

instance {-# OVERLAPPING #-} Functor f => f :<: f where
    inj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
    inj = Inl

instance {-# OVERLAPPING #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
    inj = Inr . inj

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

eval :: Eval f => Expr f -> Bool
eval = foldExpr evalAlgebra

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val :<: f) => Bool -> Expr f
val x = inject (Val x)

infixl 3 <&&> 
(<&&>):: (And :<: f) => Expr f -> Expr f -> Expr f
x <&&> y = inject (And x y)

infixl 2 <||> 
(<||>) :: (Or :<: f) => Expr f -> Expr f -> Expr f
x <||> y = inject (Or x y)
