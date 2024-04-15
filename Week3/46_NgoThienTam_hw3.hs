-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly.
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).

checkConsume :: Double -> Double -> Double -> String
checkConsume hourConsume time maxAllowed
    | totalConsumed > maxAllowed = "you consumed more than power limit"
    | totalConsumed < maxAllowed = "you consumed less than power limit"
    | otherwise = "you consumed just right power limit"
    where
        totalConsumed = hourConsume * time * 30

-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.
checkConsume2 :: Double -> Double -> Double -> String
checkConsume2 hourConsume hourOfUse maxAllowed
    | totalConsumed > maxAllowed = "you consumed more power limit " ++ show delta
    | totalConsumed < maxAllowed = "you consumed less power limit " ++ show delta
    | otherwise = "you consumed just right power limit"
    where
        totalConsumed = hourConsume * hourOfUse * 30
        delta = if totalConsumed > maxAllowed
                    then totalConsumed - maxAllowed
                    else maxAllowed - totalConsumed


-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.

-- A function that calculate NET income from GROSS in VND unit (simplifed)
-- net = gross - bhxh (10.5%) - tncn (make it simple, just take 5% for all ranges)
calculateNet1 :: Double -> Double
calculateNet1 gross = gross - (gross * 0.105) - (gross - (gross * 0.105) - 11000000) * 0.05

calculateNet2 :: Double -> Double
calculateNet2 gross =
    let bhxh = gross * 0.105
        tncn = (gross - bhxh - 11000000) * 0.05
    in gross - bhxh - tncn

-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.

-- Cases, there are 9 in total
-- a < 0, b < 0 -> if a < b then b / a else a / b
-- a < 0, b = 0 -> b / a = 0 < 1
-- a < 0, b > 0 -> both a / b and b / a < 1

-- a = 0, b < 0 -> a / b = 0 < 1
-- a = 0, b = 0 -> "ERROR"
-- a = 0, b > 0 -> a / b = 0 < 1

-- a > 0, b < 0 -> both a / b and b / a < 1
-- a > 0, b = 0 -> b / a = 0 < 1
-- a > 0, b > 0 -> if a < b then a / b else b / a

-- Group them
-- a = 0, b = 0 -> "ERROR"
-- a = 0, b != 0 -> a / b = 0 <= 1
-- a != 0, b = 0 -> b / a = 0 <= 1
-- otherwise if a / b <= 1 then a / b else b / a

checkQuotient :: Double -> Double -> String
checkQuotient a b
  | a == 0 && b == 0 = "both are zeros"
  | a == 0 && b /= 0 = show (a / b)
  | a /= 0 && b == 0 = show (b / a)
  | otherwise = if a / b <= 1 then show(a / b) else show(b / a)

-- Question 5
-- Write a function that takes in two numbers and calculates the sum of square roots for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block.
sumSqrt :: Double -> Double -> Double
sumSqrt a b = let sqrtProduct = sqrt product
                    where product = a * b
              in sqrtProduct + sqrtQuotient
                    where sqrtQuotient = let quotient = a / b
                                         in sqrt quotient

-- Straight forware way, can ignore
sumSqrt2 :: Double -> Double -> Double
sumSqrt2 a b = sqrt (a * b) + sqrt (a / b)