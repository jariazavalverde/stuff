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
