-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?
{-
  minBound :: a
  maxBound :: a

Reference: https://hackage.haskell.org/package/planet-mitchell-0.0.0/docs/Bounded.html
-}


-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.
{- 
They belong to same type classes: Bounded, Read, Enum, Integral, Num, Real, Show, Eq.

The defference between them:Int is a signed value 16-bit (+32767 to -32768 range),
                            Word is an unsigned 16-bit value (0 to +65535 range).

ghci> :i Bounded
type Bounded :: * -> Constraint
class Bounded a where
  minBound :: a
  maxBound :: a
  {-# MINIMAL minBound, maxBound #-}
-}


-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?
{-
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]

Reference: https://hackage.haskell.org/package/base-4.20.0.1/docs/GHC-Enum.html#t:Enum
-}


-- Question 4
-- Add the most general type signatures possible to the functions below.
-- Then uncomment the functions and try to compile.
f1 :: (Show a, Fractional a) => a -> a -> String -> String
f1 x y z = show (x / y) ++ z

f2 :: (Eq a, Bounded a, Enum a) => a -> a
f2 x = if x == maxBound then minBound else succ x


-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide to change between numeric types.
{-
Reference:
    https://www.haskell.org/tutorial/numbers.html
    https://wiki.haskell.org/Numeric_Prelude
    https://hackage.haskell.org/package/numeric-prelude 
-}