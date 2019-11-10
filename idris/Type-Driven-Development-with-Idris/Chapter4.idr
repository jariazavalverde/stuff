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

--- Exercise 5
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
