import System



{- EXERCISES 5.1 -}

-- Exercise 1
||| Using do notation, write a printLonger program that reads two strings and
||| then displays the length of the longer string.
printLonger : IO ()
printLonger = do
  x <- getLine
  y <- getLine
  putStrLn $ show $ max (length x) (length y)

-- Exercise 2
||| Write the same program using of do notation.
printLonger' : IO ()
printLonger' = getLine >>=
  (\x => getLine >>=
    (\y => putStrLn $ show $ max (length x) (length y)))



{- EXERCISES 5.2 -}

getNat : IO (Maybe Nat)
getNat = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just $ cast input)
    else pure Nothing

-- Exercise 1
||| Write a function that implements a simple “guess the number” game.
guess : (target : Nat) -> IO ()
guess target = do
  putStrLn "Enter a number:"
  Just x <- getNat | Nothing => do
    putStrLn "You must enter a natural number."
    guess target
  (case compare x target of
        EQ => putStrLn "You win"
        LT => putStrLn "Too low" >>= (\_ => guess target)
        GT => putStrLn "Too high" >>= (\_ => guess target))

-- Exercise 2
||| Implement a main function that chooses a random number between 1 and 100
||| and then calls guess.
random_guess : IO ()
random_guess = time >>= guess . fromIntegerNat . (`mod` 100)

-- Exercise 3
||| Extend guess so that it counts the number of guesses the user has taken and
||| displays that number before the input is read.
guess' : (target : Nat) -> (guesses : Nat) -> IO ()
guess' target guesses = do
  putStrLn $ "This is the attempt number " ++ show guesses
  putStrLn "Enter a number:"
  Just x <- getNat | Nothing => do
    putStrLn "You must enter a natural number."
    guess' target guesses
  (case compare x target of
        EQ => putStrLn "You win"
        LT => putStrLn "Too low" >>= (\_ => guess' target (guesses+1))
        GT => putStrLn "Too high" >>= (\_ => guess' target (guesses+1)))

-- Exercise 4
||| Implement your own versions of repl and replWith . Remember that you’ll
||| need to use different names to avoid clashing with the names defined in the
||| Prelude.
repl' : String -> (String -> String) -> IO ()
repl' prompt onInput = do putStr prompt
                          input <- getLine
                          putStr (onInput input)
                          repl' prompt onInput

replWith' : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
replWith' state prompt onInput = do putStr prompt
                                    input <- getLine
                                    (case onInput state input of
                                          Nothing => pure ()
                                          (Just (result, state')) => do putStr result
                                                                        replWith' state' prompt onInput)
