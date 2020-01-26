module Lib
    ( cliFunc
    ) where

import Data.List.Split

cliFunc :: IO ()
cliFunc = do
    putStrLn "Enter a command"
    cmdInput <- getLine
    if null cmdInput
        then
            putStrLn "Exiting..."
        else do
            -- Parrot back the command and recursively call main
            putStrLn $ "This was your input:" ++ cmdInput
            putStrLn $ "commands:" ++ show (filterCommands cmdInput)
            cliFunc

-- Filter out any words in the input that don't match
-- on of the words in the command list
filterCommands :: String -> [String]
filterCommands x = filter (`elem` cmdList) (splitOneOf "., " x)
    where cmdList = ["forward", "backward", "left", "right"]