-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).

-- GROUP3 checked, OK
checkMonthlyElectricityConsumption :: Int -> Int -> Int -> String
checkMonthlyElectricityConsumption consumption hours allowedConsumption
        | monthlyElectricityConsumption < allowedConsumption = "The monthly consumption of an electrical device is smaller than the maximum allowed"
        | monthlyElectricityConsumption == allowedConsumption = "The monthly consumption of an electrical device is equal to the maximum allowed"
        | otherwise = "The monthly consumption of an electrical device is bigger than the maximum allowed"
    where
        monthlyElectricityConsumption = consumption * hours * 30
{-
Test_case 1 : checkMonthlyElectricityConsumption 2 3 180
 -> result : "The monthly consumption of an electrical device is equal to the maximum allowed"
Test_case 2 : checkMonthlyElectricityConsumption 2 3 181
 -> result : "The monthly consumption of an electrical device is smaller than the maximum allowed"
Test_case 3 : checkMonthlyElectricityConsumption 2 3 179
 -> result : "The monthly consumption of an electrical device is bigger than the maximum allowed"
-}


-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.

-- GROUP3 checked, OK
checkMonthlyElectricityConsumptionPro :: Int -> Int -> Int -> String
checkMonthlyElectricityConsumptionPro consumption hours allowedConsumption
        | excessOrSavingsConsumption > 0 = "The monthly consumption of an electrical device is smaller than the maximum allowed. The savings of consumption : " ++ show excessOrSavingsConsumption
        | excessOrSavingsConsumption == 0 = "The monthly consumption of an electrical device is equal to the maximum allowed."
        | otherwise = "The monthly consumption of an electrical device is bigger than the maximum allowed. The excess of consumption : " ++ show (excessOrSavingsConsumption * (-1))
    where
        excessOrSavingsConsumption = allowedConsumption - (consumption * hours * 30)
{-
Test_case 1 : checkMonthlyElectricityConsumptionPro 2 3 180
 -> result : "The monthly consumption of an electrical device is equal to the maximum allowed."
Test_case 2 : checkMonthlyElectricityConsumptionPro 2 3 182
 -> result : "The monthly consumption of an electrical device is smaller than the maximum allowed.  The savings of consumption : 2"
Test_case 3 : checkMonthlyElectricityConsumptionPro 2 3 177
 -> result : "The monthly consumption of an electrical device is bigger than the maximum allowed.  The excess of consumption : 3"
-}


-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.

-- Example : Calculates the area of a complex shape (combination of a rectangle and a triangle)
-- GROUP3 checked, OK
calculateAreaOfComplexShapeNotUsingLet :: Double -> Double -> Double -> Double
calculateAreaOfComplexShapeNotUsingLet length width height = length * width + length * height / 2

calculateAreaOfComplexShape :: Double -> Double -> Double -> Double
calculateAreaOfComplexShape length width height = 
    let
        areaOfRectangle = length * width
        areaOfTriangle = (length * height) / 2
    in
        areaOfRectangle + areaOfTriangle    -- Summing the areas to get the total area

{-
Test_case 1 : calculateAreaOfComplexShapeNotUsingLet 2 3 4
 -> result : "10"
Test_case 2 : calculateAreaOfComplexShape 2 3 4
 -> result : "10"
-}

-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  

-- GROUP3 checked, NOT OK
checkDivision :: Double -> Double -> String
checkDivision a b
    | b == 0 = "Cannot perform division by 0"
    | otherwise = if a / b > 1 then show (b / a) else show (a / b)
{-
Test_case 1 : checkDivision 2 0
 -> result : "Cannot perform division by 0"
Test_case 2 : checkDivision 0 2
 -> result : "0.0"
Test_case 3 : checkDivision 4 2
 -> result : "0.5"
Test_case 4 : checkDivision 1 4
 -> result : "0.25"
Test_case 5 : checkDivision 4.5 3.25
 -> result : "0.7222222222222222"
Test_case 6 : checkDivision (-4.5) 3.25
 -> result : "-1.3846153846153846"
Test_case 7 : checkDivision (-4.5) (-3.25)
-> result : "0.7222222222222222"
-}


-- Question 5
-- Write a function that takes in two numbers and calculates the sum of square roots for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block. 

-- GROUP3 checked, NOT OK, logic is correct, but lacks of "where in let"
calculates :: Double -> Double -> String
calculates a b  
    | b == 0 = "Cannot perform division by 0" 
    | a < 0 && b > 0 = "Cannot square root a negative number" 
    | a > 0 && b < 0 = "Cannot square root a negative number" 
    | otherwise = 
        let result = sqrt_product + sqrt_quotient
        in show result
             where 
                sqrt_product = let product = a * b in sqrt product
                sqrt_quotient = let quotient = a / b in sqrt quotient 
                
{-
Test_case 1 : calculates 2 0
 -> result : "Cannot perform division by 0"
Test_case 2 : calculates 0 2
 -> result : "0.0"
Test_case 3 : calculates 2 2
 -> result : "3.0"
Test_case 4 : calculates 3 3
 -> result : "4"
Test_case 5 : calculates 1 1
 -> result : "2"
Test_case 6 : calculates (-4) (-4)
 -> result : "5"
Test_case 7 : calculates (-4) 4
-> result : "Cannot square root a negative number"
Test_case 8 : calculates 4 (-4)
-> result : "Cannot square root a negative number"
-}