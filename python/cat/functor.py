class Functor:

    """A type f is a Functor if it provides a function fmap which, given any
    types a and b, lets you apply any function of type (a -> b) to turn an f a
    into an f b, preserving the structure of f."""

    def fmap(x, f):
        """fmap :: Functor a -> (a -> b) -> Functor b"""
        raise NotImplementedError
    
    def replaceAll(x, c):
        """Replace all locations in the input with the same value.
        replaceAll :: Functor a -> b -> Functor b"""
        return x.fmap(lambda _x: c)

    def void():
        """Discards or ignores the result of evaluation.
        void :: Functor a -> Functor ()"""
        return x.replaceAll(())