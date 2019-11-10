import Data.Vect

allLengths : List String -> List Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

insert : Ord a => (x : a) -> (xsSorted : Vect len a) -> Vect (S len) a
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

insSort : Ord a => Vect k a -> Vect k a
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted

-- Exercise 1 (3.2)
||| Define your own version of length using interactive editing in Atom.
my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = S $ my_length xs

-- Exercise 2 (3.2)
||| Define your own version of reverse using interactive editing in Atom.
my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = reverse xs ++ [x]

-- Exercise 3 (3.2)
||| Define your own version of map using interactive editing in Atom.
my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: map f xs

-- Exercise 4 (3.2)
||| Define your own version of map using interactive editing in Atom.
my_vect : (a -> b) -> Vect k a -> Vect k b
my_vect f [] = []
my_vect f (x :: xs) = f x :: my_vect f xs
