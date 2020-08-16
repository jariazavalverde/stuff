from types import FunctionType as function

class Category:

    """A class for categories."""

    @staticmethod
    def id():
        """The identity morphism.
        id :: Category a a"""
        raise NotImplementedError

    def __mul__(g, f):
        """Morphism composition.
        (*) :: Category b c -> Category a b -> Category a c"""
        raise NotImplementedError
    
    def __gt__(g, f):
        return f * g

class Arrow(Category):

    """The basic arrow class."""

    @staticmethod
    def arr(f):
        """Lift a function to an arrow.
        arr :: (b -> c) -> Arrow b c"""
        raise NotImplementedError
    
    def first(f):
        """Send the first component of the input through the argument arrow,
        and copy the rest unchanged to the output.
        first :: Arrow b c -> Arrow (b, d) (c, d)"""
        raise NotImplementedError
    
    def second(f):
        """A mirror image of first.
        second :: Arrow b c -> Arrow (d, b) (d, c)"""
        arrow = f.__class__
        swap = arrow.arr(lambda xs: (xs[1],xs[0]))
        return ((arrow.id().first() > swap) > f.first()) > swap

class Function(Arrow):

    def __init__(self, fn):
        self.fn = fn

    def __str__(self):
        return str(self.fn)
    
    def __call__(self, *args, **kargs):
        return self.fn(*args, **kargs)

    @staticmethod
    def id():
        """The identity morphism.
        id :: a -> a"""
        return Function(lambda x: x)

    def __mul__(g, f):
        """Morphism composition.
        (*) :: (b -> c) -> (a -> b) -> (a -> c)"""
        return Function(lambda x: g(f(x)))
    
    @staticmethod
    def arr(f):
        """Lift a function to an arrow.
        arr :: (b -> c) -> (b -> c)"""
        return Function(f)
    
    def first(f):
        """Send the first component of the input through the argument arrow,
        and copy the rest unchanged to the output.
        first :: (b -> c) -> ((b, d) -> (c, d))"""
        return Function(lambda bd: (f(bd[0]), bd[1]))

print((Function(lambda x: x+1).second())((2,3)))