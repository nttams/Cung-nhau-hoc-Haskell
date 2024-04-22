-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?
nested :: [([Int], [Int])]
nested = [([1, 2], [3, 4]), ([5, 6], [7, 8])]

getValue :: [([Int], [Int])] -> Int
getValue ((_, [_, x]):_) = x

-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.
removeMorethen3 :: [a] -> [a]
removeMorethen3 [x, y, z]  = [x, y, z]
removeMorethen3 (_:_:_:xs) = xs
removeMorethen3 xs         = xs

removeMorethen3' x =
  case x of
    [x, y, z]  -> [x, y, z]
    (_:_:_:xs) -> xs
    xs         -> xs

-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together
take3Element :: (Integer, Integer, Integer) -> Integer
take3Element (x, y, z) = x + y + z

-- take3Element':: [Integer]->Integer
-- take3Element' (x:y:z:_) =x+y+z
-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.
returnEmpty :: [a] -> Bool
returnEmpty x =
  case x of
    []  -> True
    (_) -> False

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.
tailFunc :: [a] -> [a]
tailFunc x =
  case x of
    (_:rest) -> rest
    []       -> []

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing.
-- (Use the `even` function to check if the number is even.)
addOneIfEven :: Int -> Int
addOneIfEven x =
  case even (x) of
    True  -> x + 1
    False -> x
