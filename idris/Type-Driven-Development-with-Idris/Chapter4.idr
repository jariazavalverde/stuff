import Data.Vect
import Data.Fin

data Shape = ||| Triangle with length and height
             Triangle Double Double
           | ||| Rectangle with length and height
             Rectangle Double Double
           | ||| Circle with radius
             Circle Double

%name Shape shape, shape1

area : Shape -> Double
area (Triangle x y) = 0.5 * x * y
area (Rectangle x y) = x * y
area (Circle x) = pi * x * x



data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

%name Picture pic, pic1



data Tree a = ||| A tree with no data
              Empty
            | ||| A node with a left subtree, a value, and a right subtree
              Node (Tree a) a (Tree a)

%name Tree tree, tree1

insert : Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x tree@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => tree
                                      GT => Node left val (insert x right)



{- EXERCISES 4.1 -}

-- Exercise 1
||| Write a function that inserts every element of a list into a binary search
||| tree.
listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x $ listToTree xs

-- Exercise 2
||| Write a corresponding function that flattens a tree into a list using
||| in-order traversal.
treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ val :: treeToList right

-- Exercise 3
||| Define a recursive data type that can be used to represent such expressions.
data Expr = ||| A single integer
            Val Int
          | ||| Addition of an expression to an expression
            Add Expr Expr
          | ||| Subtraction of an expression from an expression
            Sub Expr Expr
          | ||| Multiplication of an expression with an expression
            Mult Expr Expr

%name Expr expr, expr1, expr2

-- Exercise 4
||| Write a function that evaluates an integer arithmetic expression.
evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add expr expr1) = evaluate expr + evaluate expr1
evaluate (Sub expr expr1) = evaluate expr - evaluate expr1
evaluate (Mult expr expr1) = evaluate expr * evaluate expr1

-- Exercise 5
||| Write a function that returns the larger of the two inputs, or Nothing if
||| both inputs are Nothing.
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe x y = liftA2 max x y <|> x <|> y

allTriangles : Picture -> List Shape
allTriangles (Primitive triangle@(Triangle _ _)) = [triangle]
allTriangles (Primitive _) = []
allTriangles (Combine pic pic1) = allTriangles pic ++ allTriangles pic1
allTriangles (Rotate _ pic) = allTriangles pic
allTriangles (Translate _ y pic) = allTriangles pic

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

-- Exercise 6
||| Write a function that returns the area of the biggest triangle in a picture,
||| or Nothing if there are no triangles.
biggestTriangle : Picture -> Maybe Double
biggestTriangle pic = (case allTriangles pic of
                            [] => Nothing
                            xs => Just $ foldr max 0 (map area xs))



{- EXERCISES 4.2 -}

-- Exercise 1
||| Extend the Vehicle data type so that it supports unicycles and motorcycles,
||| and update wheels and refuel accordingly.

-- Exercise 2
||| Extend the PowerSource and Vehicle data types to support electric vehicles
||| (such as trams or electric cars).
data PowerSource = Petrol | Pedal | Electricity
data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Unicycle : Vehicle Pedal
     Motorcycle: (fuel : Nat) -> Vehicle Petrol
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol
     Tram : Vehicle Electricity
     ElectricCar : Vehicle Electricity

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Tram = 0
wheels ElectricCar = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200

-- Exercise 3
||| The take function, on List, has type Nat -> List a -> List a . What’s an
||| appropriate type for the corresponding vectTake function on Vect?
-- vectTake : Fin n -> Vect (n+m) a -> Vect n a

-- Exercise 4
||| Implement vectTake.
vectTake : (n : Nat) -> Vect (n+m) a -> Vect n a
vectTake Z _ = []
vectTake (S k) (x::xs) = x :: vectTake k xs

-- Exercise 5
||| Write a sumEntries function with the following type:
||| sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
||| It should return the sum of the entries at position pos in each of the
||| inputs if pos is within bounds, or Nothing otherwise
sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = (case integerToFin pos n of
                                 Nothing => Nothing
                                 (Just f) => Just $ index f xs + index f ys)



{- EXERCISES 4.3 -}

-- Exercise 1
{- Add a size command that displays the number of entries in the store. -}

-- Exercise 2
{- Add a search command that displays all the entries in the store containing a
given substring. -}

-- Exercise 3
{- Extend search to print the location of each result, as well as the string. -}

data DataStore : Type where
     MkData : (size : Nat) -> (items : Vect size String) -> DataStore

data Command = DSAdd String
             | DSGet Integer
             | DSSize
             | DSSearch String
             | DSQuit

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (items ++ [newitem])

parse : (input : String) -> Maybe Command
parse input = case Strings.span (/= ' ') input of
                   ("add", str) => Just $ DSAdd (ltrim str)
                   ("search", str) => Just $ DSSearch (ltrim str)
                   ("get", id) => Just $ DSGet (cast (ltrim id))
                   ("size", rest) => if ltrim rest == "" then Just DSSize else Nothing
                   ("quit", rest) => if ltrim rest == "" then Just DSQuit else Nothing
                   _ => Nothing

getEntry : (id : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry id store = let entries = items store in
                        (case integerToFin id (size store) of
                              Nothing => Just ("Out of range\n", store)
                              (Just id) => Just (Vect.index id entries ++ "\n", store))

searchEntries : (item : String) -> (store : DataStore) -> String
searchEntries item store = Foldable.concat $ map (\(id,str) => show id ++ " " ++ str ++ "\n") $
                           List.filter ((isInfixOf item).snd) (List.zip [0..size store] (toList $ items store))


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                                Nothing => Just ("Invalid command\n", store)
                                (Just DSSize) => Just (show (size store) ++ "\n", store)
                                (Just (DSAdd item)) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                (Just (DSSearch item)) => Just (searchEntries item store, store)
                                (Just (DSGet id)) => getEntry id store
                                (Just DSQuit) => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
