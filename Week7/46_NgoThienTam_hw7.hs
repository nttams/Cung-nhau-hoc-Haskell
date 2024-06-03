-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?

-- Answer: the `Bounded` type class requires its type must implement 2 functions: minBound, maxBound.
-- They are like lower and upwer limits of the value of that type


-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.

-- Answer:
-- + Int is signed integer, range [-2^63..2^63-1]
-- + Word is unsigned integer, range [0..2^64-1]


-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?

-- Answer: the Enum type class require its type to provide sequentially functions, like succ, pred, toEnum, fromEnum, ...
-- succ :: a -> a
-- pred :: a -> a
-- toEnum :: Int -> a
-- fromEnum :: a -> Int

-- For example: Char type is an instance of Enum so it must provide those avoe 4 (and more) functions:
-- ghci> succ 'a'
-- 'b'
-- ghci> pred 'z'
-- 'y'
-- ghci> toEnum 97 :: Char
-- 'a'
-- ghci> fromEnum 'a'
-- 97


-- Question 4
-- Add the most general type signatures possible to the functions below.
-- Then uncomment the functions and try to compile.

-- Answer:
-- + The operator "/" is used, so x, y must be Fractional
-- + The function show is used, so (x / y) must be Show -> x, y must be Show
-- + The operator "++" is used, z must be String to be appended with result of Show
-- + Suitable type for x, y: Float, Double
f1 :: (Show a, Fractional a) => a -> a -> String -> String
f1 x y z = show (x / y) ++ z

-- + The operator "==" is used, so x must be Eq
-- + The functions maxBound and minBound are used, x must be Bounded
-- + The function succ is used, x must be Enum
-- + Suitable types for x: Bool, Char, Int, Word
f2 :: (Eq a, Bounded a, Enum a) => a -> a
f2 x = if x == maxBound then minBound else succ x


-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide to change between numeric types.

-- Answer:
-- + fromInteger :: Num a => Integer -> a
-- + fromInteger can convert Integer to Num, which can be converted to Float or Double
-- ghci> fromIntegral  10 :: Float
-- 10.0

-- + round :: (RealFrac a, Integral b) => a -> b
-- + round can convert real number to Integral, which can be converted to Int/Word/Integer
-- ghci> round 10.5 :: Integer
-- 10