main :: IO ()
main = do
  putStrLn (show $ addThree 1 2 3)

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- :t (==)
-- (==) :: (Eq a) => a -> a -> bool
-- Everything before => is a class constraint
-- Constraint: the two values must be a member of the Eq class
--
-- elem is (Eq a) => a -> [a] -> Bool
-- It uses == to check over a list to see if the given value is in it
-- Eq class member implement == and /+
--
-- Ord class members have ordering
-- A class must be in Eq to be in Ord
--
-- Members of Show can be presented as strings
--
-- Read is the opposite of show, it takes a string and returns a type that's a member of Read
-- Haskell needs to have enough info to evaluate an argument to Read so read "5" does not
-- have enough info.
--
-- Enum members are sequentially ordered types. succ and pred are successor and preceding
--
-- Bounded members have upper and lower bound. A tuple of bounded components is itself bounded
--
-- Num is a numeric typeclass.
-- Integral and Floating are classes within Num
--
-- fromIntegral converts values from integrals into another numeric type
--
-- Syntax in functions
--
-- Adding two vectors in 2D space
-- 'a' must be Numeric
-- Takes two pairs of numerics and returns a numeric pair
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- How to pattern match 3 element tuples:
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- Lists can be pattern matched
