-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).


-- cách 1
checkElectricalDevice1 :: Float->Float->Float->String
checkElectricalDevice1 consumption hours maximumAllowed
    |consumption <0 || hours <0 || maximumAllowed <0 = "input values must be >0"
    |monthlyUsage == maximumAllowed = "equal maximum allowed"
    |monthlyUsage > maximumAllowed = "exceeds maximum allowed"
    |otherwise = "within maximum allowed"
        where monthlyUsage =consumption*hours*30
-- cách 2
checkElectricalDevice2 :: Float->Float->Float->String
checkElectricalDevice2 consumption hours maximumAllowed
    |consumption <0 || hours <0 || maximumAllowed <0 = "input values must be >0"
    |otherwise = 
        let monthlyUsage =consumption*hours*30
        in if monthlyUsage == maximumAllowed then "equal maximum allowed" else if monthlyUsage > maximumAllowed then "exceeds maximum allowed" else "within maximum allowed"
            
            


-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.
checkElectricalDevice3 :: Float->Float->Float->String
checkElectricalDevice3 consumption hours maximumAllowed
    |consumption <0 || hours <0 || maximumAllowed <0 = "input values must be >0"
    |monthlyUsage == maximumAllowed = "equal maximum allowed"
    |monthlyUsage > maximumAllowed = "exceeds maximum allowed by " ++ show(a) ++ " Kwh"
    |otherwise = "savings " ++ show(a) ++ " Kwh"
        where 
            monthlyUsage =consumption*hours*30
            a = abs(monthlyUsage-maximumAllowed)
            

-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.

bigExpression :: Double -> Double
bigExpression x = 
    let part1 = x * 2 + 3
        part2 = part1 ** 2
        part3 = x - 1
        in part2 + part3 * 5



-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  



quotient :: Double ->Double ->String
quotient a b
    |b==0 = "Division by zero is not possible"
    |otherwise = if result <=1 then show(a/b) else  show(1/result)
        where result = a/b
        
      

-- Question 5
-- Write a function that takes in two numbers and calculates the sum of square roots for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block. 

-- case1 : use a where block inside a let expression 

sumOfSquare :: Double ->Double->Double
sumOfSquare a b =
   
        let squareProduct = sqrt(productResult)
            squareQuotient = sqrt(quotientResult)
            in squareProduct +squareQuotient
            where
                productResult = a*b
                quotientResult = a/b
      
-- case2 : let expression inside a where block.
sumOfSquare' :: Double ->Double->Double
sumOfSquare' a b = result
    where
        result = 
            let productResult = a*b
                quotientResult =a/b
                in sqrt(productResult)+sqrt(quotientResult)

           