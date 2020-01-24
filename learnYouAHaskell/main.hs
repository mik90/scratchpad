-- Now on: http://learnyouahaskell.com/higher-order-functions

main :: IO ()
main = do
  putStrLn "Running quicksort [1,5,1,3,6,3]"
  putStrLn (show $ quicksort [1,5,1,3,6,3])
--  putStrLn (show $ addThree 1 2 3)

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
-- [1,2,3] == 1:2:3:[]
-- x:xs binds head of the list to x and the rest to x
-- x:y:z:zs would match against lists with at least 3 elements

-- Define our own head
-- Takes a list of a and returns an element of type a
head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
-- Just grab the first element and use _ to capture the rest of the list
-- In order to bind several variables, we must surround them with parenthesis
head' (x:_) = x

-- 'a' must be a member of Show which means it must be able to be presented
-- as a String
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
-- This is the same as [x]
tell (x:[]) = "The list has one element: " ++ show x
-- This is the same as [x,y]
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list has many elements, the first two are: " ++ show x ++ " and " ++ show y

-- Implement length with pattern matching
-- The list can be any time but this function's return value is a Numeric 
length' :: (Num b) => [a] -> b
-- Base case
length' [] = 0
-- Consume the head and split the list from head:rest of the list
-- and add one since we consumed an element. Recursively continue
length' (_:xs) = 1 + length' xs

-- Similar to length' except the value returned is also an a and the list must be a member
-- of Numeric
sum' :: (Num a) => [a] -> a
-- Empty list has no value
sum' [] = 0
-- Pattern-match the head
-- The value of this is the head + the sum of the rest
sum' (x:xs) = x + sum' xs

-- We can bind a part of a pattern to a name while still keeping the reference to the
-- entire value/list/matched thing
-- this is done with @ in front of a pattern

capital :: String -> String
capital "" = "This is an empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- You can't pattern match against ++

-- Guards test a property, or multiple properties of a value and ensure they are
-- true or false

-- Takes in 'a' which must be a RealFloat
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "Normal weight"
  | bmi <= 30.0 = "Overweight"
  | otherwise   = "Obese"

-- Guards are indicated by pipes after the function's name and parameters
-- the last guard is usually 'otherwise', which is defined as 'otherwise = True'
-- 
-- With gaurds, there is no '=' right after the function name and parameters

-- The parameters and return type must all be members of Ord (ordered)
max' :: (Ord a) => a -> a -> a
max' a b
  | a > b     = a
  | otherwise = b

-- Guards can also be inline like this:  max' a b | a > b = a | otherwise = b

-- Returns a member of Ordering
myCompare :: (Ord a) => a -> a -> Ordering
-- Use backticks ` to either define or call functions with infix notation
a `myCompare` b
  | a > b     = GT
  | a == b    = EQ
  | otherwise = LT

-- Using 'where' clauses allows you to bind names within a function's scope
bmiTell2 :: (RealFloat a) => a -> a -> String  
bmiTell2 weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat    = 30.0  

-- If you want sevrel patterns of a function to access some shared name, it has to be
-- defined globally
--
-- You can pattern match within a 'where' binding
-- e.g.
-- where bmi = weight / height ^ 2
-- (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- Takes in a first name and last name, returns their initials
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname -- Grab the head of 'firstname' here
        (l:_) = lastname

-- It would've been better to do the pattern matching directly on the parameters but this
-- shows that it is possible to do it in a 'where' binding

-- Takes a list of weight:height pairs and returns a list of bmis
calcBmis :: (RealFloat a) => [(a,a)] -> [a]
-- Use a list comprehension to return 'bmi' on each weight,height pair in the list
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

-- The function had to examine each element in the list that was passed into it

-- 'where' bindings can also be nested
-- It's common to make a function and define some helper function in its 'where' clause and
-- those helper functions can each have their own 'where' clause if needed

-- 'let' bindings are similar to 'where' bindings
-- They let you bind to variables anywhere and are themselves expressions

-- Cylinder's surface area based on height and radius
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

-- Form is
-- let <bindings> in <expression>
-- The bound names are accessible in the expression
-- 'where' bindings are just syntactic constructs while 'let' bindings are expressions
-- 'let' bindings can be used pretty much anywhere
-- They can introduce functions in local scope
-- 'let' can define variables inline and separate them with semicolons instead of a newline
-- They can also be put in list comprehensions

-- Takes in a list of pairs
-- calcBmis :: (RealFloat a) => [(a,a)] -> [a]
-- This would take in a list of bmis and return the list with bmis over 25.0
-- 'let' can be used as a predicate and the 'in' can be omitted if the names
-- in the expression are already visible (like in thsi predicate)
-- calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- Since 'let' bindings are expressions and are local in their scope, they cannot be used
-- across guards 

-- Haskell has 'case' expressions

-- These two head' functions are equivalent
-- head' :: [a] -> a
-- head' [] = error "The list is empty"
-- head' (x:_) = x

-- head' :: [a] -> a
-- head' xs = case xs of [] -> error "The list is empty"
--                       (x:_) -> x


-- Syntax is:
-- case expression of pattern -> result
--                    pattern -> result
--                    pattern -> result
--                    ...
--
-- where 'expression' is matched against the patterns

-- Pattern matching on function parameters can be used only in function definitions but
-- case expressions can go almost anywhere 

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "a singleton list"
                                               xs -> "a multi-element list"

-- This could also be defined using 'where' bindings
describeList2 :: [a] -> String
describeList2 xs = "The list is " ++ what xs
  where what [] = "empty"
        what [x] = "a singleton list"
        what xs = "multi-element list"

-- Recursion
-- Edge condition == base case
-- In Haskell, recursion is used instead of iteration (as found in imperative languages)


-- Finding a maximum recursively
-- Edge condition would be that the maximum of a singleton list is the only element
-- in it
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x -- The maximum of the tail is recursive itself
  | otherwise = maxTail
  where maxTail = maximum' xs

-- Pattern matching to split head an tail is a common idiom when doing recursion
-- with lists

maximum2' :: (Ord a) => [a] -> a
maximum2' [] = error "Empty list"
maximum2' [x] = x
maximum2' (x:xs) = max x (maximum2' xs)

-- i must be a value that is both numeric and ordered
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x -- replicate x n times
    | n <= 0     = []                   -- empty list if replicating 0 times
    | otherwise  = x:replicate' (n-1) x -- return x and replicate x n - 1 times


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []  -- 0 or less elements returns us an empty list, otherwise will fall through
take' _ []     = []  -- cannot take anything from an empty list
take' n (x:xs) = x : take' (n-1) xs -- take an element and call n-1 on the rest


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x] -- flip the x:xs and run reverse' on xs

-- Infinitely recursive function
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- However, calling something like take 5 (repeat 3) will return a list of five 3's
-- even though repeat will recurse forever on its own

-- Takes a list of one or two types and returns a list of pairs
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = [] -- second list is empty, return empty list
zip' [] _ = [] -- first list is empty, return empty list
zip' (x:xs) (y:ys) = (x,y):zip' xs ys -- take the heads of boths lists

-- Checks if an element is in a list
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False -- empty list
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs -- disregard the head and keep searching


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = [] -- Edge condition
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x] -- grab all less than/eq to x
        biggerSorted  = quicksort [a | a <- xs, a > x]  -- grab all greater than x
    in smallerSorted ++ [x] ++ biggerSorted


-- Higher order functions

-- Technically all functions in haskell take one parameter
-- Using more parameters is done via currying
-- max 4 5 actually creates a function that takes a parameter and returns
-- either 4 or that parameter e.g. "max 4 a" which looks like
-- (max 4) 5

-- Putting a space between two things is function application
-- Calling a function with too few parameters returns a partially applied function

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- Can bind it to another name with a parameter
-- Can multiply tow numbers with 9
-- Need to bind it with 'let' or pass it to another function if we want
-- to use multThree in its partially applied form
let multTwoWithNine = multThree 9


-- Infix functions
-- Can be partially applied by using sections. To do this, surround it with parenthesis
-- and only supply a parameter on one side
divideByTen :: (Floating a) => a -> a
divideByTen = (/10) -- Surrounded by parens
-- >  divideByTen 200 is the same as 200 / 10 and the same as (/10) 200

-- Checks if the supplied character is uppercase
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- Some higher-orderism is in order
-- Takes a function and applies it twice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- First param is a funciton that takes two things and returns a third
-- Second/third params are lists of things
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = [] -- Edge conditions are that a list is empty
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- Runs on the head then recursively runs on the tail

-- flip takes a function and returns the function with the arguments flipped
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

-- map takes a function and a list and applies the function to every element in the list

-- map (+3) [1,5,3,1,6] is the same as [x+3 | x <- [1,5,3,1,6]] but map can be more idiomatic

-- filter takes a function and a predicate and returns a list of elements that satisfy
-- the predicate

quickSortFiltered :: (Ord a) => [a] -> [a]
quickSortFiltered [] = []
quickSortFiltered (x:xs) =
    let smallerSorted = quickSortFiltered (filter (<=x) xs)
        biggerSorted = quickSortFiltered (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

-- Make use of Haskell's laziness, if you map and filter over a list multiple times, it will
-- only go over the list once

-- Lambdas
-- Used with \ and usually surrounded with parentheses (but don't have to be)
-- e.g. (\xs -> length xs > 15) is a function that tell you if the list passed to it
-- has a length greater than 15

-- Folds reduce a list to a single value
-- Summing fold left, starting with 0
sumFold' :: (Num a) => [a] ->
sumFold' xs = foldl (+) 0

-- foldl starts from the left, foldr starts from the right
-- The type of the accumulator and the end result are the same


