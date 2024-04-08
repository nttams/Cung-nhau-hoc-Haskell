
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)

f1: Floar -> Float -> Float -> Float
f1 x y z = x ** (y/z)

f2: Int -> Int -> Int -> Float
f2 x y z = sqrt (x/y - z)

f3: Bool -> Int -> [Bool]
f3 x y = [x == True] ++ [y]

f4: Eq a => a -> [a] -> [a] -> Bool
f4 x y z = x == (y ++ z)


-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?
{-
    Answer:
        - Type signatures are important because they help you to understand the type of the function and the type of the
          arguments that the function takes. This is important because it helps you to understand what the function does and
          how it can be used. It also helps you to understand what the function returns.
        - Helping You as the Developer: Type signatures provide clarity and documentation for your own code. By explicitly 
          specifying the types of the function's arguments and return value, you can better understand the intended behavior
          of the function. This can be especially helpful when you revisit your code after a long time or when working on a complex project.
        - Enabling Type Checking: Haskell is a statically typed language, which means that the types of values are checked at compile-time. 
          By providing type signatures, you allow the Haskell compiler to verify that the function is used correctly throughout your codebase. 
          This helps catch type-related errors early in the development process, reducing the likelihood of runtime errors
        - Facilitating Code Reuse: Type signatures make it easier for others to understand and use your functions. When you provide clear 
          type information, it becomes simpler for other developers to integrate your functions into their own code. They can easily determine
          the expected types of arguments and the resulting type, allowing for seamless integration and composition of functions.
        - Improving Readability and Maintainability: Type signatures serve as a form of documentation for your code. When someone reads your code,
          they can quickly understand the types involved and the purpose of the function. This makes the codebase more readable and maintainable, 
          as developers can easily reason about the behavior of functions without having to dive into the implementation details.

-}


-- Question 3
-- Why should you define type signatures for variables? How can they help you?
{-
    - Documentation and Readability: Type signatures serve as a form of documentation for your code. They provide a clear and explicit description of
      the types of values that a variable can hold. This makes your code more readable and helps other developers understand how to use your code correctly.
    - Error Detection and Prevention: Type signatures help catch type-related errors at compile-time. Haskell is a statically typed language, which means
      that the types of variables are checked at compile-time. By providing type signatures, you can catch type mismatches or inconsistencies early on,
      before the code is executed. This helps prevent runtime errors and makes your code more robust.
    - Code Maintenance and Refactoring: Type signatures make it easier to refactor and modify your code. When you change the implementation of a function 
      or modify its behavior, the type signature acts as a contract that ensures you don't accidentally introduce type errors. It provides a safety net that
      helps you maintain the correctness of your code as you make changes.
    - Tooling and IDE Support: Type signatures enable powerful tooling and IDE features. With type information available, IDEs can provide intelligent code 
      completion, type inference, and error checking. This can significantly improve your development experience and productivity.
-}


-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.
{-
    Yes, there are many functions in Haskell that allow you to transform one type to another. These are often called conversion functions. 
    Here are a few examples:

    - `show`: This function converts a value of any type that is a member of the Show class into a string. For example, `show 123` returns "123".
            showInt :: Int -> String
            showInt = show
    - `read`: This function converts a string into a value of any type that is a member of the Read class. For example, `read "123"` returns 123.
            readInt :: String -> Int
            readInt = read
    - `fromIntegral`: This function converts an integral number to a more general number. For example, `fromIntegral 10` can be a Float, Double, etc.
            fromIntegralInt :: Int -> Float
            fromIntegralInt = fromIntegral
    - `round`, `floor`, `ceiling`: These functions convert floating point numbers to integral numbers.
            roundFloat :: Float -> Int
            roundFloat = round
            floorFloat :: Float -> Int
            floorFloat = floor
            ceilingFloat :: Float -> Int
            ceilingFloat = ceiling

    It's important to note that Haskell is a statically typed language, so you can't arbitrarily convert between types without a specific conversion function.
-}


-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
{-
    Yes, in Haskell you can define a list of lists. Here is an example:

        listOfLists :: [[Int]]
        listOfLists = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

    To access the innermost elements, you would first index into the outer list to get an inner list, and then index into the inner list. 
    Here is an example:

        firstInnerList :: [Int]
        firstInnerList = listOfLists !! 0  -- This will give you the first inner list: [1, 2, 3]

        firstElement :: Int
        firstElement = firstInnerList !! 0  -- This will give you the first element of the first inner list: 1

    You can also do this in one step:

        firstElementOneStep :: Int
        firstElementOneStep = (listOfLists !! 0) !! 0  -- This will also give you the first element of the first inner list: 1
-}
