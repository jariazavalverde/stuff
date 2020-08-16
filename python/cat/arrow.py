from types import FunctionType as function
from category import Category

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
        arrow = f.__class__
        return f * arrow.id()

    def second(f):
        """A mirror image of first.
        second :: Arrow b c -> Arrow (d, b) (d, c)"""
        arrow = f.__class__
        return arrow.id() * f
    
    @classmethod
    def returnA(arrow):
        """The identity arrow.
        returnA :: Arrow a a"""
        return arrow.arr(lambda x: x)

    def __mul__(f, g):
        """Split the input between the two argument arrows and combine their
        output.
        (*) :: Arrow b c -> Arrow b' c' -> Arrow (b, b') (c, c')"""
        arrow = f.__class__
        swap = arrow.arr(lambda xs: (xs[1],xs[0]))
        return f.first() >> swap >> g.first() >> swap
    
    def __and__(f, g):
        """Send the input to both argument arrows and combine their output.
        (&) :: Arrow b c -> Arrow b c' -> Arrow b (c, c')"""
        arrow = f.__class__
        return arrow.arr(lambda x: (x,x)) >> (f * g)
