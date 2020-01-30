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
            -- Parrot back the command and go again
            putStrLn $ "This was your input:" ++ cmdInput
            putStrLn $ "commands: " ++ show (filterCommands cmdInput)
            drawScreen 4 3

-- Filter out any words in the input that don't match
-- on of the words in the command list
filterCommands :: String -> [String]
filterCommands x = filter (`elem` cmdList) (splitOneOf ".,;() " x)
    where cmdList = ["forward", "backward", "left", "right"]

-- Next thing to do is have a grid that prints out to the screen
-- and make it into a game
-- Redraw the screen after every input
data Pos = Pos { row :: Int
               , col :: Int
               } deriving (Show)
-- Not drawing the car yet
-- drawScreen :: Int -> Int -> Pos -> IO ()

-- Draw boxes like: |_||_||_| with a newline at the end
createRow :: Int -> String
createRow x = concat $ replicate x "|_|" ++ ["\n"]


drawScreen :: Int -> Int -> IO ()
drawScreen height width = putStrLn $ concat $ replicate height (createRow width)
