from arrow import Arrow

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

    def __lshift__(g, f):
        """Morphism composition.
        (<<) :: (b -> c) -> (a -> b) -> (a -> c)"""
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