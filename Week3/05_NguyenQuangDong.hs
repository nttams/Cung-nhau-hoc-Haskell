-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
monthlyConsumption :: Float -> Float -> Float -> String
monthlyConsumption hourlyConsumption dailyHours maxMonthly 
    | monthlyUsage > maxMonthly = "The monthly consumption is bigger than the maximum allowed."
    | monthlyUsage == maxMonthly = "The monthly consumption is equal to the maximum allowed."
    | otherwise = "The monthly consumption is smaller than the maximum allowed."
    where monthlyUsage = hourlyConsumption * dailyHours * 30


-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.
monthlyConsumption :: Float -> Float -> Float -> String
monthlyConsumption hourlyConsumption dailyHours maxMonthly 
    | monthlyUsage > maxMonthly = "The monthly consumption is bigger than the maximum allowed. Excess: " ++ show (monthlyUsage - maxMonthly)
    | monthlyUsage == maxMonthly = "The monthly consumption is equal to the maximum allowed."
    | otherwise = "The monthly consumption is smaller than the maximum allowed. Savings: " ++ show (maxMonthly - monthlyUsage)
    where monthlyUsage = hourlyConsumption * dailyHours * 30


-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.
advantagesOfLet :: Float -> Float -> String       -- dien tich hinh tron vs hinh vuong
advantagesOfLet radius side = 
    let 
        circleArea = pi * radius^2
        squareArea = side^2
    in 
        case compare circleArea squareArea of     -- chia ra cac case nhu switch
            GT -> "The circle has a larger area."
            LT -> "The square has a larger area."
            EQ -> "The circle and the square have the same area."


-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  
checkQuotient :: Float -> Float -> String
checkQuotient num1 num2
    | num2 == 0 = "Can not devide by 0."
    | otherwise = 
        let quotient = num1 / num2 in
            if quotient > 1 then "1"
            else show quotient


-- Question 5
-- Write a function that takes in two numbers and calculates the sum of square roots for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block. 
sumOfSquareRoots :: Float -> Float -> Float
sumOfSquareRoots num1 num2 
    | num2 == 0 = error "Cannot divide by zero"          
    | otherwise = 
        let quotient = num1 / num2        -- num2 = 0 throw error
        in sqrt product + sqrt quotient
    where 
        product = let q = num1 * num2 in q