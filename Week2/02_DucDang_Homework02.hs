
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)

f1 :: Double -> Double -> Double -> Double
f1 x y z = x ** (y/z)

f2 :: Double -> Double -> Double -> Double
f2 x y z = sqrt (x/y - z)

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]

--f4 :: Eq a => [a] -> [a] -> [a] -> Bool
f4 :: [Int] -> [Int] -> [Int] -> Bool
f4 x y z = x == (y ++ z)


-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?
{- to make it clear that which types of inputs and which a type of a return value,
    help us to know types of values when reading code.
-}

-- Question 3
-- Why should you define type signatures for variables? How can they help you?
{- regarding polymorphic values, in case we need inputs and an output which have different types for a function
-}

-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.
{- Yes, there are. `show` function will transform any type to String, 
for example: show 3 -> "3" -}

-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
{- Yes, we can. Ex:
mulList = [[1,2,3],[4,5,6],[7,8,9]]
We use !! to access each element of lists. Ex: 
mulList !! 1 !! 1 -}