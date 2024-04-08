
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)

f1 :: Float -> Float -> Float -> Float
f1 x y z = x ** (y/z)


f2 :: Float -> Float ->Float->Float
f2 x y z = sqrt (x/y - z)


f3 :: Bool->Bool->[Bool]
f3 x y = [x == True] ++ [y]


f4 :: String->String->String->Bool
f4 x y z = x == (y ++ z)


-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?

-- Answers:

-- Defining type signatures of functions in programming languages that support them  can be very beneficial for several reasons:

-- Readability: Type signatures make your code easier to understand. They serve as a form of documentation that clearly states what kind of arguments a function expects and what it returns.

-- Error Prevention: Type signatures can help catch errors at compile time rather than at runtime. If you try to pass an argument of the wrong type to a function, the compiler will throw an error.

-- Tooling Support: Many IDEs use type signatures to provide better autocompletion, linting, and hover tooltips. This can make the development process smoother and more efficient.

-- Code Maintainability: They make the code more maintainable. If a function's implementation is changed in a way that affects its input or output types, any incompatible uses of the function elsewhere in the codebase will be flagged by the type checker.

-- Collaboration: They can help others understand your code. If you're working in a team, type signatures can help your teammates understand what your functions do without having to read through the entire implementation.








-- Question 3
-- Why should you define type signatures for variables? How can they help you?

-- Answer:

-- Defining type signatures for variables in statically typed languages can be very beneficial for several reasons:

-- Error Prevention: Type signatures can help catch errors at compile time rather than at runtime. If you try to assign a value of the wrong type to a variable, the compiler will throw an error.

-- Readability and Maintainability: Type signatures make your code easier to understand and maintain. They serve as a form of documentation that clearly states what kind of value a variable is expected to hold. This is especially helpful in large codebases or when working in a team.

-- Tooling Support: Many IDEs use type signatures to provide better autocompletion, linting, and hover tooltips. This can make the development process smoother and more efficient.

-- Performance: In some languages, specifying types can allow the compiler to optimize the generated code, leading to better performance.

-- Code Robustness: They make the code more robust. With type signatures, you can be sure that a variable always has a specific type, which can prevent unexpected behavior.





-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.


-- Answer:

-- Yes, Haskell provides several ways to transform one type to another. Here are a few examples:


-- show 123 -- "123"
-- read "123" :: Int -- 123
-- fromIntegral 123 :: Float -- 123.0



-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?

-- Yes, you can define a list of lists in Haskell. Here's an example:
-- listOfLists :: [[Int]]

-- listOfLists = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

-- In this example, listOfLists is a list where each element is itself a list of integers.

-- To access the innermost elements, you would first index into the outer list to get one of the inner lists, and then index into that list. Here's how you can access the number 5 in the above list of lists:

-- innerList :: [Int]

-- innerList = listOfLists !! 1  -- Get the second list ([4, 5, 6])

-- element :: Int
-- element = innerList !! 1  -- Get the second element of the inner list (5)

-- You can also do this in one step:
-- element :: Int

-- element = (listOfLists !! 1) !! 1  -- 5


