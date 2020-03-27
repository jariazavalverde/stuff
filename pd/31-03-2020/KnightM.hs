module KnightM(
    moveKnightM,
    in3M
) where

import Control.Monad(guard)
import GHC.Base(Alternative)
import Knight(KnightPos, onBoard)

-- moveKnightM :: [KnightPos] -> KnightPos -> [KnightPos]
moveKnightM :: (Monad m, Alternative m) => m KnightPos -> KnightPos -> m KnightPos
moveKnightM m (x,y) = do (a,b) <- m
                         guard $ onBoard (a+x,b+y)
                         return (a+x,b+y)

-- in3M :: [KnightPos] -> KnightPos -> [KnightPos]
in3M :: (Monad m, Alternative m) => m KnightPos -> KnightPos -> m KnightPos
in3M m pos0 = do pos1 <- moveKnightM m pos0
                 pos2 <- moveKnightM m pos1
                 moveKnightM m pos2