from functor import Functor

class Applicative(Functor):

    """A functor with application, providing operations to embed pure
    expressions, and sequence computations and combine their results."""

    @staticmethod
    def pure(x):
        """Lift a value.
        pure :: a -> Applicative a"""
        raise NotImplementedError
    
    def ap(u, v):
        """Sequential application.
        ap :: Applicative (a -> b) -> Applicative a -> Applicative b"""
        raise NotImplementedError

def liftA2(f, a, b):
    """Lift a binary function to actions.
    liftA2 :: (a -> b -> c) -> Applicative a -> Applicative b -> Applicative c"""
    applicative = a.__class__
    return applicative.pure(lambda x: lambda y: f(x,y)).ap(a).ap(b)