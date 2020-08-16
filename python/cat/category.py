class Category:

    """A class for categories."""

    @staticmethod
    def id():
        """The identity morphism.
        id :: Category a a"""
        raise NotImplementedError

    def __lshift__(g, f):
        """Morphism composition.
        (<<) :: Category b c -> Category a b -> Category a c"""
        raise NotImplementedError
    
    def __rshift__(g, f):
        """Left-to-right composition.
        (>>) :: Category a b -> Category b c -> Category a c"""
        return f << g