-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).
{- Viết một hàm kiểm tra xem lượng tiêu thụ hàng tháng của một thiết bị điện có lớn hơn, bằng hoặc nhỏ hơn lượng tối đa cho phép 
   và trả về một thông báo tương ứng.
   Hàm phải nhận vào lượng tiêu thụ hàng giờ của một thiết bị điện, số giờ sử dụng hàng ngày và lượng tiêu thụ hàng tháng tối đa cho phép.
   (Lượng tiêu thụ hàng tháng = tiêu thụ (kW) * số giờ sử dụng hàng ngày (h) * 30 ngày). -}

-- GROUP3 checked, OK
checkConsumption :: Double -> Double -> Double -> String
checkConsumption p h maxAllowed
      |usageinmonth == maxAllowed = "Monthly Consumption is equal than the maximum allowed"
      |usageinmonth > maxAllowed = "Monthly consumption is bigger than the maximum allowed"
      |otherwise = "Monthly consumption is smaller than the maximum allowed"
         where
         usageinmonth = p * h * 30
            
-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.
-- In the previous function, return the excess/savings of consumption as part of the message.
{- Chúng ta sử dụng hàm `show :: a -> String` để chuyển đổi một kiểu dữ liệu bất kỳ thành một chuỗi.
   Ví dụ `show 3` sẽ cho ra "3" và `show (3 > 2)` sẽ cho ra "True".
   Hãy dùng nó để cập nhật hàm trong bài 1, trả về số điện năng tiêu thụ vượt quá giới hạn hoặc tiết kiệm được trong thông báo.
-}

-- GROUP3 checked, OK
checkConsumption' :: Double -> Double -> Double -> String
checkConsumption' p h maxAllowed
         |usageinmonth == maxAllowed = "Monthly Consumption is equal than the maximum allowed"
         |usageinmonth > maxAllowed = "Monthly consumption is bigger than the maximum allowed" ++ show (usageinmonth - maxAllowed) ++ "kwh"
         |otherwise = "Monthly consumption is smaller than the maximum allowed" ++ show (maxAllowed - usageinmonth) ++ "kwh"
          where
           usageinmonth = p * h * 30

-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.
-- Viết một hàm cho thấy ích lợi của việc sử dụng biểu thức let để chia một biểu thức lớn thành các biểu thức nhỏ hơn.
-- GROUP3 checked, NOT OK, it does not compile.
-- atob and btoa are declared to take 1 argument, but they are provided none
-- exchange :: Double -> Double -> Double
-- exchange a b =
--   let atob t = t * 25037
--       btoa t = t / 25037
--    in if a > b then atob else btoa

-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  

-- Viết một hàm nhận vào hai số và trả về thương của chúng sao cho nó không lớn hơn 1 (Nghĩa là nếu hàm nhận vào 2 số a, b,
-- thì ta có 2 giá trị thương là a/b hoặc b/a, hãy chọn giá trị không > 1 làm kết quả trả về, tất nhiên cần xem xét các trường hợp đặc biệt nữa)
-- Trả về kết quả dưới dạng một chuỗi, và trong trường hợp mẫu số là 0, trả về một thông báo giải thích tại sao phép chia không
-- thể thực hiện được. Hãy thực hiện bài này sử dụng cả guard và if-then-else trong cùng một hàm.
-- GROUP3 checked, OK, but syntax is incorrect
g :: Double -> Double -> String
g x y
   |x == 0 && y == 0 = "Undefined"
   |x == 0 || y == 0 = "=0"
   |otherwise =
      if x / y <= 1
         then "x/y = " ++ show (x / y)
         else "y/x = " ++ show (y / x)


-- Question 5
-- Write a function that takes in two numbers and calculates the sum of square roots for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block. 
{- 
Viết một hàm nhận vào hai số và tính: (căn bậc hai của tích 2 số + căn bậc hai của thương 2 số). 
Viết hàm sao cho trong hàm có cả 2 thứ sau:
   - một khối where bên trong một biểu thức let
   - một biểu thức let bên trong một khối where. 
-}
-- GROUP3 checked, NOT OK
g' :: Double -> Double -> String
g' a b
  |b == 0 = "Undefined"
  |otherwise =
   let sqA = square a
          where square a = a * a
    in show (sqA * sqB + sqA / sqB)
          where
       sqB = 
         let square b = b * b
          in square b