-- Create a higher-order function that takes 3 parameters: A function and the two parameters that that function takes, and
-- flips the order of the parameters.
-- For example this: `(/) 6 2` returns `3`. But this: `flip' (/) 6 2` returns `0.3333333333`
combineFirstLastName :: String -> String -> String
combineFirstLastName x y = x ++ y

interchangeableFistLastName :: String -> String -> String
interchangeableFistLastName x y =flip combineFirstLastName x y


-- Create the `uncurry'` function that converts a curried function to a function on pairs. So this: `(+) 1 2` that returns `3` can be written as
-- `uncurry' (+) (1,2)` (with the two different arguments inside a pair).
uncurriedFunc :: (Double, Double) -> Double
uncurriedFunc (x, y) = x + y

-- Create the `curry'` function that converts an uncurried function to a curried function. So this: `fst (1,2)` that returns `1` can be written as
-- `curry' fst 1 2` (with the tuple converted into two different arguments).
curriedFunc :: a -> a -> a 
curriedFunc = \a b -> a

-- Use higher-order functions, partial application, and point-free style to create a function that checks if a word has an uppercase letter.
-- Start with using just higher-order functions and build from there. 
upperCaseLetter :: [Char]
upperCaseLetter = ['A'..'Z']

checkUpperCaseLetter :: Char -> Bool
checkUpperCaseLetter x = x `elem` upperCaseLetter

checkUpperCaseWord :: String -> Bool
checkUpperCaseWord = any checkUpperCaseLetter --word


-- Create the `count` function that takes a team ("Red", "Blue", or "Green") and returns the amount of votes the team has inside `votes`.

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

countVote :: String -> Int
countVote x = length $ filter (== x) votes -- filter (\a -> a == x) votes

-- Create a one-line function that filters `cars` by brand and then checks if there are any left.

cars :: [(String,Int)]
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]

checkCarAmount :: String -> Bool
checkCarAmount x = any (\(a, b) -> a == x && b > 0) cars