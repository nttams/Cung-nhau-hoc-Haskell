-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).
comparePowerMmonthly :: Float->Float->String
comparePowerMmonthly p h 
    |powerMmonthly>quota ="bigger"
    |powerMmonthly<quota ="smaller"
    |powerMmonthly==quota ="equal"
  where
    powerMmonthly=p*h*30
    quota=500


-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.

showComparePowerMmonthly :: Float->Float->String
showComparePowerMmonthly p h 
    |powerMmonthly>quota ="Using excess " ++ show (powerMmonthly-quota) ++ " KW power "
    |powerMmonthly<quota ="Using saving " ++ show (quota-powerMmonthly) ++ " KW power "
    |powerMmonthly==quota ="Using " ++ show powerMmonthly ++ " KW power is equal"
  where
    powerMmonthly=p*h*30
    quota=500
    
-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.
{- Sử dụng biểu thức let in để làm các bài toán lớn chia nhỏ các công thức tính toán thành các công thức nhỏ và dễ nhìn dễ hiểu.
VD: tính diện tích một khu đất hình chữ nhật vào 2 cạnh. Nếu <1000m2 đưa ra kết quả m2, >=1000m2 và <1000000m2 kết quả ra là ha và nếu lớn hơn kết quả là km2.

-}
f3::Float->Float->String
f3 a b=
  let
    dientichm2=a*b
    dientichHa=a*b/10000
    dientichKm2=a*b/1000000
  in
    if dientichm2<1000 then "Dien tich = " ++ show  dientichm2 ++ " m2"
      else
        if dientichm2<1000000 then "Dien tich = " ++ show  dientichHa ++ " Ha"
          else "Dien tich = " ++ show  dientichKm2 ++ " Km2"    

-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  
f4::Float->Float->String
f4 a b=
    if b==0 then "so chia =0"
      else 
        if a<b then "Thuong = " ++ show  (a/b) 
          else  "Thuong >1 " 



f4'::Float->Float->String
f4' a b
    | b==0 = "so chia =0"
    | a<b = "Thuong = " ++ show  (a/b) 
    | otherwise =  "Thuong >1 " 

    
-- Question 5
-- Write a function that takes in two numbers and calculates the sum of squares for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block.
f5 :: Float->Float-> String
f5 x y =
    let sqrtProd = show (sqrt xyProd) where xyProd = x * y
    in   ( sqrtProd) ++" va "++ ( sqrtQuot)
    where
        sqrtQuot
          | y /= 0 = let xyQuot = (/) x y
                        in show (sqrt xyQuot)
          |otherwise  = "The division is not possible"
