from inspect import signature
from arrow import Arrow
from monad import Monad

class Function(Arrow, Monad):

    def __init__(self, fn):
        self.fn = fn
        self.length = len(signature(fn).parameters)

    def __str__(self):
        return str(self.fn)
    
    def __call__(self, *args):
        if(len(args) >= self.length):
            return self.fn(*args)
        return Function(lambda *args2: self(*(args + args2)))

    # Category interface

    @staticmethod
    def id():
        """The identity morphism.
        id :: a -> a"""
        return Function(lambda x: x)

    def __lshift__(g, f):
        """Morphism composition.
        (<<) :: (b -> c) -> (a -> b) -> (a -> c)"""
        return Function(lambda x: g(f(x)))
    
    # Arrow interface

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
    
    # Functor interface

    def fmap(f, g):
        """fmap :: (a -> b) -> (b -> c) -> (a -> c)"""
        return Function(lambda x: g(f(x)))
    
    # Applicative interface

    @staticmethod
    def pure(x):
        """Lift a value.
        pure :: a -> (b -> a)"""
        return Function(lambda _x: x)
    
    def ap(f, g):
        """Sequential application.
        ap :: Applicative (a -> b) -> Applicative a -> Applicative b"""
        return Function(lambda x: f(x)(g(x)))

    # Monad interface

    def bind(f, g):
        """Sequentially compose two actions, passing any value produced by the
        first as an argument to the second.
        bind :: (a -> b) -> (b -> (a -> c)) -> (a -> c)"""
        return Function(lambda a: g(f(a))(a))