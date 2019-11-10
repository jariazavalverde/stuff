--- Exercise 1
ex1_a : (String, String, String)
ex1_a = ("A", "B", "C")

ex1_b : List String
ex1_b = ["A", "B", "C"]

ex1_c : ((Char, String), Char)
ex1_c = (('A', "B"), 'C')

-- Exercise 2
||| Returns whether the input reads the same backwards as forwards.
palindrome : String -> Bool
palindrome xs = xs == reverse xs

-- Exercise 3
||| Modify the palindrome function so that it’s not case sensitive.
palindrome_insensitive : String -> Bool
palindrome_insensitive = palindrome . toLower

-- Exercise 4
||| Modify the palindrome function so that it only returns True for
||| strings longer than 10 characters.
palindrome_larger_ten : String -> Bool
palindrome_larger_ten xs = length xs > 10 && palindrome xs

-- Exercise 5
||| Modify the palindrome function so that it only returns True for
||| strings longer than some length given as an argument.
palindrome_larger_than : Nat -> String -> Bool
palindrome_larger_than n xs = length xs > n && palindrome xs

-- Exercise 6
||| Returns a pair of the number of words in the input and the number
||| of characters in the input.
counts : String -> (Nat, Nat)
counts xs = (length $ words xs, length xs)

-- Exercise 7
||| Returns the ten largest values in a list.
top_ten : Ord a => List a -> List a
top_ten = List.take 10 . sortBy (flip compare)

-- Exercise 8
||| Returns the number of strings in the list longer than the given number
||| of characters.
over_length : Nat -> List String -> Nat
over_length n = length . filter (> n) . map length

-- Exercise 9
||| For each of palindrome and counts, write a complete program that prompts
||| for an input, calls the function, and prints its output.
main : IO ()
main = repl "Enter a palindrome: " ((++ "\n") . show . palindrome)
