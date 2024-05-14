-- Create a higher-order function that takes 3 parameters: A function and the two parameters that that function takes, and
-- flips the order of the parameters.
-- For example this: `(/) 6 2` returns `3`. But this: `flip' (/) 6 2` returns `0.3333333333`



flip' :: (a->b->c)->b->a->c
flip' f x y = f y x
-- Create the `uncurry'` function that converts a curried function to a function on pairs. So this: `(+) 1 2` that returns `3` can be written as
-- `uncurry' (+) (1,2)` (with the two different arguments inside a pair).

uncurry' :: (a->b->c)->(a,b)->c
uncurry' f (x,y)= f x y


-- Create the `curry'` function that converts an uncurried function to a curried function. So this: `fst (1,2)` that returns `1` can be written as
-- `curry' fst 1 2` (with the tuple converted into two different arguments).
curry' :: ((a,b)->c)->a->b->c
curry'  f x y = f (x,y)



-- Use higher-order functions, partial application, and point-free style to create a function that checks if a word has an uppercase letter.
-- Start with using just higher-order functions and build from there. 



-- checkUpper :: String ->Bool
-- checkUpper = any isUpper
--     where
--         isUpper :: Char -> Bool
--         isUpper c = c `elem` ['A'..'Z']


checkUpper :: String ->Bool
checkUpper = any (`elem` ['A'..'Z'])



-- Create the `count` function that takes a team ("Red", "Blue", or "Green") and returns the amount of votes the team has inside `votes`.

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

countVotes :: String -> Int
countVotes vote= length.filter (==vote)$votes



-- Create a one-line function that filters `cars` by brand and then checks if there are any left.

cars :: [(String,Int)]
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]

checkCarLeft :: String->String
checkCarLeft brand
    |result == True = "Have left "++quantity++" "++brand
    |otherwise = "Empty"
    where
        result =any ((>0).snd ).filter ((==brand).fst )$ cars
        quantity = show.snd.(!!0).filter ((==brand).fst )$ cars
