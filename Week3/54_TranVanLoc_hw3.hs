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
   checkConsumption :: Int -> Int -> Int -> String
   checkConsumption consumption hours maximum
    | calculate > maximum = "Bigger"
    | calculate == maximum = "Equal"
    | calculate < maximum = "Smaller"
    where calculate = consumption * hours * 30

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
   checkConsumption' :: Int -> Int -> Int -> String
   checkConsumption' consumption hours maximum
    | calculate > maximum = "Excess " ++ show num
    | calculate == maximum = "Nothing"
    | calculate < maximum = "Savings " ++ show num
    where calculate = consumption * hours * 30
          num = abs (maximum - calculate)

-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.
-- Viết một hàm cho thấy ích lợi của việc sử dụng biểu thức let để chia một biểu thức lớn thành các biểu thức nhỏ hơn.
-- GROUP3 checked, OK
   calculateRoomPrice :: Double -> Double -> Double -> Double
   calculateRoomPrice roomPrice electric water = 
    let 
       electricPrice = electric * 3.5
       waterPrice = water * 8
    in 
        roomPrice + electricPrice + waterPrice

-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.

-- Viết một hàm nhận vào hai số và trả về thương của chúng sao cho nó không lớn hơn 1 (Nghĩa là nếu hàm nhận vào 2 số a, b,
-- thì ta có 2 giá trị thương là a/b hoặc b/a, hãy chọn giá trị không > 1 làm kết quả trả về, tất nhiên cần xem xét các trường hợp đặc biệt nữa)
-- Trả về kết quả dưới dạng một chuỗi, và trong trường hợp mẫu số là 0, trả về một thông báo giải thích tại sao phép chia không
-- thể thực hiện được. Hãy thực hiện bài này sử dụng cả guard và if-then-else trong cùng một hàm.
-- GROUP3 checked, OK, but
-- + Instead of using Int, can you Double, then no need for "fromIntegral"
-- + The last 2 else(s) can never be reached, so don't need if statement there
   calculateNumber :: Int -> Int -> String
   calculateNumber a b
    | b == 0 && a == 0 = "Both of values are Zero"
    | b == 0 || a == 0 = "0"
    | divA < divB = _a
    | otherwise = _b
    where 
        divB = fromIntegral b / fromIntegral a
        divA = fromIntegral a / fromIntegral b
        _b = if a /= 0 then show divB else "Model is zero"
        _a = if b /= 0 then show divA else "Model is zero"


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
{-
    Bai này đang chưa rõ dàng về đề bài, phần thương ở đây là số nào chia cho số nào ạ?
-}
   calculateSumOfSquareRoots :: Double -> Double -> Double
   calculateSumOfSquareRoots x y =
    let
        sqrtProduct = sqrt productResult
        sqrtQuotient = sqrt quotientResult
    in
        sqrtProduct + sqrtQuotient
    where
        productResult = x * y
        quotientResult = let 
            divX = x / y
            divY = y / x
            in if x == 0 then divX else divY