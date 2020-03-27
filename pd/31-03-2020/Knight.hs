module Knight(
    KnightPos,
    jump,
    onBoard,
    moveKnight,
    in3,
    reachIn3
) where

import Control.Monad(guard)
type KnightPos = (Int,Int)

jump :: [KnightPos]
jump = do x <- [-2,-1,1,2]
          y <- [-2,-1,1,2]
          guard (3 == abs x + abs y)
          return (x,y)

onBoard :: KnightPos -> Bool
onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]

moveKnight :: KnightPos -> [KnightPos]
moveKnight (x,y) = do (a,b) <- jump
                      guard $ onBoard (a+x,b+y)
                      return (a+x,b+y)

in3 :: KnightPos -> [KnightPos]
in3 pos0 = do pos1 <- moveKnight pos0
              pos2 <- moveKnight pos1
              moveKnight pos2

reachIn3 :: KnightPos -> KnightPos -> Bool
reachIn3 from to = to `elem` in3 from