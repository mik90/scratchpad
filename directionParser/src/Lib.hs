module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"




-- Reads a line, tells us if the line is a palindrome
-- main = interact respondPalindromes
-- '\' means that this is an anonymous function (lambda)
-- using 'interact' means that the input strings will be transformed into "palindrome" or "not a palindrome"

respondPalindromes = unlines . map (\xs -> if isPalindrome xs the
repeatCommand = do
    putStrLn "Enter a command"
    cmd <- getLine
    if null cmd
        then do
            -- Return an empty IO action, don't call main again
            return ()
            putStrLn "Exiting..."
        else do
            -- Parrot back the command and recursively call main
            putStrLn $ "This was your command:" ++ cmd
            repeatCommand