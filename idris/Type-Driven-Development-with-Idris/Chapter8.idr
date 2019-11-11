{- EXERCISES 8.1 -}

-- Exercise 1
||| Implement the following function, which states that if you add the same
||| value onto the front of equal lists, the resulting lists are also equal:
same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons = cong
--same_cons {x} = cong {f = (x::)}

-- Exercise 2
||| Implement the following function, which states that if two values, x and y,
||| are equal, and two lists, xs and ys, are equal, then the two lists x :: xs
||| and y :: ys must also be equal:
same_lists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x::xs = y::ys
same_lists Refl Refl = Refl

-- Exercise 3
||| Define a type, ThreeEq, that expresses that three values must be equal.
data ThreeEq : a -> b -> c -> Type where
     Same3 : (a : _) -> ThreeEq a a a

-- Exercise 4
||| Implement the following function, which uses ThreeEq:
allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS x x x (Same3 x) = Same3 (S x)
