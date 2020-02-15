import Data.List(sort, intercalate)
import Data.Maybe(isJust)
import Control.Monad(guard)

data Term = Var String | Atom String [Term] deriving (Ord, Eq)

type Goal = [Term]
type Program = [(Term, Goal)]
type Substitution = [(String, Term)]
type State = (Int, Goal, Substitution)

instance Show Term where
    show (Var x) = x
    show (Atom "nil" []) = "[]"
    show (Atom "cons" [x, xs]) = '[' : show x ++ showCons xs ++ "]"
    show (Atom x []) = x
    show (Atom x args) = x ++ '(' : intercalate "," (map show args) ++ ")"

showCons :: Term -> String
showCons (Atom "nil" []) = ""
showCons (Atom "cons" [x, xs]) = ',' : show x ++ showCons xs
showCons x = '|' : show x

variables :: Goal -> [String]
variables [] = []
variables (Var x:xs) = x : variables xs
variables (Atom _ x:xs) = variables x ++ variables xs

rename :: Int -> Program -> Program
rename _ [] = []
rename n ((head, body):xs) = (rename' n head, map (rename' n) body) : rename n xs

rename' :: Int -> Term -> Term
rename' n (Var x) = Var $ '$' : x ++ show n
rename' n (Atom x xs) = Atom x $ map (rename' n) xs

unify :: Term -> Term -> Maybe Substitution
unify x y = unify' [x] [y] []

unify' :: [Term] -> [Term] -> Substitution -> Maybe Substitution
unify' [] [] s = Just s
unify' (Var x:xs) (y:ys) s = unify' (map (apply' x y) xs) (map (apply' x y) ys) (compose' x y s)
unify' (x:xs) (Var y:ys) s = unify' (map (apply' y x) xs) (map (apply' y x) ys) (compose' y x s)
unify' (Atom x xa:xs) (Atom y ya:ys) s = if x == y && length xa == length ya then unify' (xa++xs) (ya++ys) s else Nothing

apply :: Substitution -> Term -> Term
apply [] t = t
apply ((u, x):xs) t = apply xs (apply' u x t)

apply' :: String -> Term -> Term -> Term
apply' v t (Var x) = if x == v then t else Var x
apply' v t (Atom x xs) = Atom x $ map (apply' v t) xs

compose :: Substitution -> Substitution -> Substitution
compose [] s = s
compose ((u, x):xs) s = compose xs (compose' u x s)

compose' :: String -> Term -> Substitution -> Substitution
compose' v t s = (v, t) : map (\(a, b) -> (a, apply' v t b)) s

step :: Program -> State -> [State]
step program (n, [],          sub) = [(n, [], sub)]
step program (n, (atom:goal), sub) = do (head', body') <- rename n program
                                        let mmgu = unify head' atom
                                            Just mgu = mmgu
                                         in do guard (isJust mmgu)
                                               return (n+1, map (apply mgu) (body' ++ goal), compose mgu sub)

resolution :: Program -> State -> [State]
resolution program state = do state'@(_, goal, _) <- step program state
                              if null goal then return state'
                                           else resolution program state'

query :: Program -> Goal -> [Substitution]
query program goal = map (answer $ variables goal) $ resolution program (0, goal, [])

answer :: [String] -> State -> Substitution
answer vs (_, _, sub) = sort $ filter ((`elem` vs) . fst) sub

appendProgram :: Program
appendProgram = [
    -- append([], X, X).
    (Atom "append" [Atom "nil" [], Var "X", Var "X"], []),
    -- append([H|T], X, [H|S]) :- append(T, X, S).
    (Atom "append" [Atom "cons" [Var "H", Var "T"], Var "X", Atom "cons" [Var "H", Var "S"]], [Atom "append" [Var "T", Var "X", Var "S"]])]

appendGoal :: Goal
-- append(X, Y, [a,b,c]).
appendGoal = [Atom "append" [Var "X", Var "Y", Atom "cons" [Atom "a" [], Atom "cons" [Atom "b" [], Atom "cons" [Atom "c" [], Atom "nil" []]]]]]
