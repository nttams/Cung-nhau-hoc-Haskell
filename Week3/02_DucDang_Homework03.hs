-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).
checkMonthlyComsumption :: Int -> Int -> Int -> String
-- Use let
checkMonthlyComsumption hc hu maxMC =
    let mUsage c h = c * h * 30
    in if mUsage hc hu > maxMC
            then "the monthly consumption of an electrical device is bigger than the maximum allowed"
            else
                if mUsage hc hu < maxMC
                    then "the monthly consumption of an electrical device is smaller than the maximum allowed"
                    else "the monthly consumption of an electrical device is equal than the maximum allowed"

-- Use where
checkMonthlyComsumption' hc hu maxMC
        | mUsage > maxMC = "the monthly consumption of an electrical device is bigger than the maximum allowed"
        | mUsage < maxMC = "the monthly consumption of an electrical device is smaller than the maximum allowed"
        | otherwise = "the monthly consumption of an electrical device is equal than the maximum allowed"
    where
        mUsage = hc * hu * 30


-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.
checkMonthlyComsumptionQ2 hc hu maxMC
        | mUsage > maxMC = "the monthly consumption of an electrical device is bigger than the maximum allowed" 
        ++ ", excess: " ++ show(mUsage - maxMC) ++ " kW"
        | mUsage < maxMC = "the monthly consumption of an electrical device is smaller than the maximum allowed" 
        ++ ", savings: " ++ show(maxMC - mUsage) ++ " kW"
        | otherwise = "the monthly consumption of an electrical device is equal than the maximum allowed"
    where
        mUsage = hc * hu * 30


-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.
hotterInKelvin :: Double -> Double -> Double
hotterInKelvin c f = if c > (f - 32) * 5 / 9 then c + 273.16 else ((f - 32) * 5 / 9) + 273.16

hotterInKelvin' :: Double -> Double -> Double
hotterInKelvin' c f =
    let fToC t = (t - 32) * 5 / 9
        cToK t = t + 273.16
        fToK t = cToK (fToC t)
    in if c > fToC f then cToK c else fToK f


-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  
checkQotient :: Double -> Double -> String
-- Use guards
checkQotient a b 
    | a / b < 1 = "The quotient of " ++ show(a) ++ " and " ++ show(b) ++ " is " ++ show(a/b)
    | b == 0 = "The number " ++ show(a) ++ " is impossible to divide because the divisor is 0"
    | otherwise = "Done"

-- Use ifthenelse
checkQotient' a b =
    if (a / b) < 1
        then "The quotient of " ++ show(a) ++ " and " ++ show(b) ++ " is " ++ show(a/b)
        else
            if b == 0
                then "The number " ++ show(a) ++ " is impossible to divide because the divisor is 0"
                else "Done"


-- Question 5
-- Write a function that takes in two numbers and calculates the sum of square roots for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block. 

-- Use let
sumOfProductAndQuotient :: Double -> Double -> Double
sumOfProductAndQuotient a b = 
    let 
        sqrtProduct
            | p >= 0 = sqrt p
            | otherwise  = 0
            where p = a * b

        sqrtQuotient  
            | q >= 0 = sqrt q
            | otherwise = 0
            where q =
                    if b == 0 
                        then 0
                        else a / b
    in sqrtProduct + sqrtQuotient

-- Use where
sumOfProductAndQuotient' :: Double -> Double -> Double
sumOfProductAndQuotient' a b = sqrtProduct + sqrtQuotient
    where 
        sqrtProduct =
            let    
                p = a * b
            in if p >= 0 then sqrt p else 0

        sqrtQuotient = 
            let
                q = if b == 0 then 0 else a / b                
            in if q >= 0 then sqrt q else 0