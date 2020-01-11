module Main where

import Lib

-- Idea: have this be a CLI interface that parses written english commands like
-- "move forward"
-- "move right and then left"
-- "stop"
-- and outputs commands towards an arduino motor controller that handles it

-- >  name <- getline
--    getLine :: IO String
-- Performs IO function and returns type String

-- Can define directions
-- unsure how Haskell works best
-- maybe:
-- data Direction = Forward | Backward | Left | Right | Stop
-- Although I want to compare it against strings so maybe an enumeration isn't best

main :: IO ()
main = someFunc