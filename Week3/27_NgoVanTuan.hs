-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).
-- GROUP3 checked, OK
bai1 :: Float -> Float -> Float -> String
bai1 used hour maxused 
    | monthlyused == maxused ="muc tieu thu hang thang bang voi muc toi da"
    | monthlyused < maxused ="muc tieu thu hang thang nho hon muc toi da"
    | otherwise = "muc tieu thu hang thang lon hon muc toi da"
    where monthlyused=used*hour*30
-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.
-- GROUP3 checked, OK
bai2 :: Float -> Float -> Float -> String
bai2 used hour maxused 
    | monthlyused == maxused ="muc tieu thu hang thang bang voi muc toi da"
    | monthlyused < maxused ="muc tieu thu hang thang nho hon " ++ show(maxused-monthlyused) ++ " kwh so voi muc toi da"
    | otherwise = "muc tieu thu hang thang lon hon " ++ show(monthlyused-maxused) ++ " kwh so voi muc toi da"
    where monthlyused=used*hour*30

-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.
-- GROUP3 checked, NOT OK
bai3 :: Float -> Float 
bai3 r = 
    let pi = 3.14
        dienTich = pi * r * r
    in dienTich
-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  
-- GROUP3 checked, BEST SOLUTION
bai4 :: Float->Float->String
bai4 a b
    | a==0 && b==0 ="Khong xac dinh"
    | a==0 || b==0 ="ket qua=0"
    | otherwise = if a/b <= 1
                    then "ket qua = " ++ show(a/b)
                    else "ket qua = " ++ show(b/a)

-- Question 5
-- Write a function that takes in two numbers and calculates the sum of square roots for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block.
-- GROUP3 checked, NOT OK
bai5 :: Float -> Float -> String 
bai5 a b 
    | b == 0 = "khong xac dinh"
    | otherwise = 
        let sqrta = squareroots a
            sqrtb = squareroots b
        in show (sqrta * sqrtb + sqrta / sqrtb)
        where
            squareroots a = sqrt(a)


