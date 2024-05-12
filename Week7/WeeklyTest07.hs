-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?
-- Tìm hiểu về lớp kiểu `Bounded`. Nó cung cấp những behaviour nào?


-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.
{-
Các kiểu Int và Word thuộc cùng một lớp kiểu. Sự khác biệt giữa chúng là gì?
Hãy kiểm tra maxBound và minBound của chúng.
-}

-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?
-- Tìm hiểu về lớp kiểu `Enum`. Nó cung cấp những behaviour nào?

-- Question 4
-- Add type signatures to the functions below and use type variables and type classes.
-- Then uncomment the functions and try to compile.
{-
Thêm chữ ký kiểu cho các hàm dưới đây và sử dụng biến kiểu và lớp kiểu.
Sau đó bỏ dấu chú thích ở trước các hàm và thử biên dịch.
-}

--f1 x y z = show (x / y) ++ z

--f2 x = if x == maxBound then minBound else succ x


-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide to change between numeric types.
{-
Tìm hiểu về các lớp kiểu số để tìm ra những behavior mà chúng cung cấp để chuyển đổi giữa các kiểu số.
-}