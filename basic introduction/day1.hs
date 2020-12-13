-- file: day1.hs
-- Haskell Learning

main = interact wordCount
    where wordCount input = show (length input) ++ " "

    -- `length input` returns the number of all the characters 
    -- in the file

    -- `length (words input)` => words input separates
    -- the input into words and length counts the number of words

    -- `length (lines input)` => lines input separates the input
    -- into lines and length counts the number of lines