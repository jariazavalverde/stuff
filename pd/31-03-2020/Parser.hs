module Parser(
    Parser(..),
    expresion,
    natural,
    knightpos
) where

import GHC.Base(Alternative(..))
import Data.Char(isDigit)
import Control.Monad(liftM, ap)

data Parser a = Parser {
    runParser :: String -> [(a,String)]
}

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Monad Parser where
    return x = Parser $ \input -> [(x,input)]
    Parser p >>= f = Parser $ \input ->
        concat $ map (\(v,str) -> runParser (f v) str) (p input)

instance Alternative Parser where
    empty = Parser (\_ -> [])
    Parser f <|> Parser g = Parser $ \input ->
        let xs = f input in if null xs then g input else xs

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \input ->
    if null input || not (p $ head input) then [] else [(head input,tail input)]

digit :: Parser Char
digit = satisfy isDigit

char :: Char -> Parser Char
char x = satisfy (== x)

-- <Natural> -> <Digito> <Natural> | <Digito>
natural :: Parser Int
natural = do xs <- some digit
             return (read xs)

-- <Expresion> -> <Factor> + <Expresion> | <Factor>
expresion :: Parser Int
expresion = (do x <- factor
                char '+'
                y <- expresion
                return (x+y)) <|> factor

-- <Factor> -> <Termino> * <Factor> | <Termino>
factor :: Parser Int
factor = (do x <- termino
             char '*'
             y <- factor
             return (x*y)) <|> termino

-- <Termino> -> (<Expresion>) | <Natural>
termino :: Parser Int
termino = (do char '('
              x <- expresion
              char ')'
              return x) <|> natural

integer :: Parser Int
integer = (char '-' >> (fmap (*(-1)) natural)) <|> natural

knightpos :: Parser (Int,Int)
knightpos = do char '('
               a <- integer
               char ','
               b <- integer
               char ')'
               return (a,b)