module Matrix
import Data.Vect  

data Matrix : Nat -> Nat -> Type -> Type where
    MkMatrix : Vect m (Vect n a) -> Matrix m n a

x : Matrix 2 2 Int
x = MkMatrix [[1,2],[3,4]]

y : Matrix 2 3 Int
y = MkMatrix [[1,2,3],[4,5,6]]

add : (Num a) => Matrix m n a -> Matrix m n a -> Matrix m n a
add (MkMatrix u) (MkMatrix v) = MkMatrix $ Data.Vect.zipWith (\u, v => Data.Vect.zipWith (+) u v) u v

transpose : Matrix m n a -> Matrix n m a
transpose {n = Z} (MkMatrix _) = MkMatrix Nil
transpose {n = S z} (MkMatrix u) = let (MkMatrix v) = transpose (MkMatrix $ map Data.Vect.tail u)
    in MkMatrix $ map Data.Vect.head u :: v
