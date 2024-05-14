-- Q1
-- Create a higher-order function that takes 3 parameters: A function and the two parameters that that function takes, and
-- flips the order of the parameters.
-- For example this: `(/) 6 2` returns `3`. But this: `flip' (/) 6 2` returns `0.3333333333`
{-
Tạo một hàm bậc cao nhận vào 3 tham số: Một hàm và hai tham số mà hàm đó nhận, sau đó đảo ngược thứ tự áp dụng của các tham số. 
Ví dụ: `(/) 6 2` trả về  `3`. Nhưng với `flip' (/) 6 2` thì trả về `0.3333333333`.
-}

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- Q2
-- Create the `uncurry'` function that converts a curried function to a function on pairs. So this: `(+) 1 2` that returns `3` can be written as
-- `uncurry' (+) (1,2)` (with the two different arguments inside a pair).
{-
Tạo hàm uncurry' chuyển đổi một hàm curried thành một hàm làm việc với cặp giá trị. 
Ví dụ: `(+) 1 2` trả về `3` có thể viết thành `uncurry' (+) (1,2)` (hai đối số được đưa vào trong một cặp).
-}

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y
-- Q3
-- Create the `curry'` function that converts an uncurried function to a curried function. So this: `fst (1,2)` that returns `1` can be written as
-- `curry' fst 1 2` (with the tuple converted into two different arguments).
{-
Tạo hàm `curry'` chuyển đổi một hàm không curried thành một hàm curried. 
Ví dụ: `fst (1,2)` trả về `1` có thể viết là `curry' fst 1 2` (với tuple được chuyển thành 2 đối số riêng biệt).
-}

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)


-- Q4
-- Use higher-order functions, partial application, and point-free style to create a function that checks if a word has an uppercase letter.
-- Start with using just higher-order functions and build from there. 
{-
Sử dụng hàm bậc cao, áp dụng từng phần, và point-free để tạo một hàm kiểm tra xem một từ có chữ cái viết hoa hay không.
Bắt đầu bằng việc sử dụng chỉ các hàm bậc cao và biến đổi tiếp từ đó.
-}

isEmailExistsUpperCase :: String -> Bool
isEmailExistsUpperCase email = any (\x -> x >= 'A' && x <= 'Z') email

-- Q5
-- Create the `count` function that takes a team ("Red", "Blue", or "Green") and returns the amount of votes the team has inside `votes`.
{-
Tạo hàm `count` nhận vào một đội ("Red", "Blue" hoặc "Green") và trả về số phiếu bầu mà đội đó có trong danh sách `votes`.
-}
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count' :: String -> Int
count' name = length $ filter (== name) votes

-- Q6
-- Create a one-line function that filters `cars` by brand and then checks if there are any left.
--  Tạo hàm một dòng lọc `cars` theo nhãn hiệu và kiểm tra xem còn xe nào không.

cars :: [(String,Int)]
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]

filters :: String -> [(String,Int)]
filters name = filter (\(x, y) -> x == name && y > 0) cars