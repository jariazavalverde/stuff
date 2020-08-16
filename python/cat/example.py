from function import Function
from applicative import liftA2

succ = Function(lambda x: x+1)
double = Function(lambda x: x*2)
add = Function(lambda x, y: x+y)
add3 = Function(lambda x, y, z: x+y+z)

# fmap (+1) (*2) 2
print( "fmap (+1) (*2) 2", "==", succ.fmap(double)(2) )
# liftA2 (+) (+1) (*2) 2
print( "liftA2 (+) (+1) (*2) 2", "==", liftA2(add, succ, double)(2) )
# ((\x y z -> x+y+z) <$> (+1) <*> (+1) <*> (+1)) 2
print( "((\\x y z -> x+y+z) <$> (+1) <*> (+1) <*> (+1)) 2", "==",
    Function.pure(add3).ap(succ).ap(succ).ap(succ)(2) )
# ((+1) >>= \x -> (*2) >>= \y -> return (x+y)) 2
print( "((+1) >>= \\x -> (*2) >>= \\y -> return (x+y)) 2", "==",
    succ.bind(lambda x: double.bind(lambda y: Function.pure(x+y)))(2) )

# returnA 2
print( "returnA 2", "==", Function.returnA()(2) )
# ((+1) <<< (*2)) 2
print( "((+1) <<< (*2)) 2", "==", (succ << double)(2) )
# ((+1) >>> (*2)) 2
print( "((+1) >>> (*2)) 2", "==", (succ >> double)(2) )
# ((+1) *** (*2)) (2,3)
print( "((+1) *** (*2)) (2,3)", "==", (succ * double)((2,3)) )
# ((+1) &&& (*2)) 2
print( "((+1) &&& (*2)) 2", "==", (succ & double)(2) )