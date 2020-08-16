from applicative import Applicative

class Monad(Applicative):

    """The Monad class defines the basic operations over a monad, a concept from
    a branch of mathematics known as category theory."""
    
    def bind(x, f):
        """Sequentially compose two actions, passing any value produced by the
        first as an argument to the second.
        bind :: Monad a -> (a -> Monad b) -> Monad b"""
        raise NotImplementedError
    
    def then(x, y):
        """Sequentially compose two actions, discarding any value produced by
        the first.
        then :: Monad a -> Monad b -> Monad b"""
        return x.bind(lambda _x: y)