-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)

f1 :: Double -> Double -> Double -> Double
f1 x y z = x ** (y/z)

{- test case
f1 2 4 2
f1 2 9 3
-}

f2 :: Double -> Double -> Double -> Double
f2 x y z = sqrt (x/y - z)

{- test case
f2 32 2 0
f2 16 2 4
-}

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]

{- test case
f3 False False
f3 False True
-}

f4 :: ([Bool]) -> [Bool] -> [Bool] -> Bool
f4 x y z = x == (y ++ z)

{- test case
f4 ([False]) [False, True] [False]
f4 ([False, True, False]) [False, True] [False]
-}


-- Question 2 -------------------------------------------------------------------------------------------------------------------
-- Why should we define type signatures of functions? How can they help you? How can they help others?
{- Answer
 Documentation and Readability: Type signatures serve as a form of documentation for your code. They communicate to both yourself and others what types of input the function expects and what type of output it produces. 
 Error Detection: Type signatures help catch type-related errors at compile time rather than at runtime.
-}


-- Question 3 -------------------------------------------------------------------------------------------------------------------
-- Why should you define type signatures for variables? How can they help you?
{- Answer
 Explicitness and Clarity: Specifying the type of a variable explicitly makes the code more readable and understandable. 
 Error Prevention: By specifying the type of a variable, you can catch type-related errors early in the development process.
 Facilitates Refactoring: When you have explicit type signatures for variables, refactoring becomes easier and safer.
-}


-- Question 4 -------------------------------------------------------------------------------------------------------------------
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.
{- Answer
 Yes, Haskell provides functions for converting between different types.
 fromIntegral: This function will convert from any Integral type into any Numeric type (which includes Int, Integer, Rational, and Double)
 realToFrac: This function will convert from any Real type into any Fractional type (which includes Rational and Double)
 SOURCES: https://wiki.haskell.org/Converting_numbers
-}


-- Question 5 -------------------------------------------------------------------------------------------------------------------
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
{- Answer
 Yes, In Haskell, you can define a list of lists.
 Example: 
    matrix :: [[Int]]
    matrix = [[1, 2, 3],
            [4, 5, 6],
            [7, 8, 9]]

 To access the innermost elements of the matrix:
    -- Accessing the element at row 2, column 3 (indexing starts from 0)
    matrix = [[1, 2, 3], [4, 5, 6],[7, 8, 9]]
    element23 = matrix !! 1 !! 2
    print element23
-}