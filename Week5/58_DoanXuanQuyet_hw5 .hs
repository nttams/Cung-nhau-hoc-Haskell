-- Create a higher-order function that takes 3 parameters: A function and the two parameters that that function takes, and
-- flips the order of the parameters.
-- For example this: `(/) 6 2` returns `3`. But this: `flip' (/) 6 2` returns `0.3333333333`

flip2 :: (a -> a -> a) -> a -> a -> a 
flip2 f x y = f y x
-- flip2 (/) 2 6
-- flip2 (+) 2 6
-- flip2 (+) 6 2
-- flip2 (-) 6 2
-- flip2 (-) 2 6
-- flip2 (++) "12" "34"
-- flip2 (++) "34" "12"

-- Create the `uncurry'` function that converts a curried function to a function on pairs. So this: `(+) 1 2` that returns `3` can be written as
-- `uncurry' (+) (1,2)` (with the two different arguments inside a pair).

uncurry2 :: (a -> a -> a) -> (a,a) -> a 
uncurry2 f (x,y) = f x y
-- uncurry2 (+) (1,2)
-- uncurry2 (-) (2,2)
-- uncurry2 (++) ("12","23")

-- Create the `curry'` function that converts an uncurried function to a curried function. So this: `fst (1,2)` that returns `1` can be written as
-- `curry' fst 1 2` (with the tuple converted into two different arguments).

curry2 :: ((a,b) -> c) -> a -> b -> c 
curry2 f x y = f (x,y)
-- curry2 fst 1 2
-- curry2 fst "test" 2
-- curry2 fst 3 "test"

-- Use higher-order functions, partial application, and point-free style to create a function that checks if a word has an uppercase letter.
-- Start with using just higher-order functions and build from there. 

--letterIsUppercase letter =  letter `elem` ['A'..'Z']
--(++ "ing") "Think" Tương tự như \x -> x ++ "ing"
letterIsUppercase :: Char -> Bool
letterIsUppercase = (`elem` ['A'..'Z'])
hasAnUppercase :: String -> Bool
hasAnUppercase = any letterIsUppercase
-- hasAnUppercase "abc"
-- hasAnUppercase "abC"


-- Create the `count` function that takes a team ("Red", "Blue", or "Green") and returns the amount of votes the team has inside `votes`.

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: String -> Int
count [] = 0
count color = length (filter (== color) votes)
-- count ""
-- count "Red"
-- count "Green"
-- count "black"
-- count "Blue"
-- count "blue"

-- Create a one-line function that filters `cars` by brand and then checks if there are any left.

cars :: [(String,Int)]
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]

filterCar = filter (\(_,x) -> x > 0) cars