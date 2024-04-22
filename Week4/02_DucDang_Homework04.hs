-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

getFour nested = 4

-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.

-- multiple function definitions
removeListFunc :: [a] -> [a]
removeListFunc [] = []
removeListFunc [x] = [x]
removeListFunc [x,y] = [x,y]
removeListFunc (x:rest) = []

-- case expression
removeListFunc' :: [a] -> [a]
removeListFunc' x = 
    case x of
        [] -> x
        [_] -> x
        [_,_] -> x
        (x:rest) -> []

-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together
sum3Tuple :: (Int,Int,Int) -> Int
sum3Tuple (x,y,z) = x + y + z

-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.
checkList :: [a] -> Bool
checkList [] = True
checkList (x:rest) = False

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.
-- case
tailFunc :: [a] -> [a]
tailFunc x = 
    case x of
        [] -> []
        (x:rest) -> rest

-- func
tailFunc' :: [a] -> [a]
tailFunc' [] = []
tailFunc' (x:rest) = rest

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing. 
-- (Use the `even` function to check if the number is even.)
addEven :: Int -> Int
addEven x =
    case even x of
        True -> x + 1
        False -> x