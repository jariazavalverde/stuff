module CurryHT(
    genCurries
) where

import Control.Monad
import Language.Haskell.TH

genCurry :: Int -> Q Dec
genCurry n = do
    f <- newName "f"
    xs <- replicateM n (newName "x")
    let args = map VarP (f:xs)
        tuple = TupE $ map VarE xs
        name = mkName $ "curry" ++ show n
        body = AppE (VarE f) tuple
    return $ FunD name [Clause args (NormalB body) []]

genCurries :: Int -> Q [Dec]
genCurries = (mapM genCurry) . (enumFromTo 1)